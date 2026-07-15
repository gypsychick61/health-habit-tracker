import Map "mo:map/Map";
import { thash; phash; nhash } "mo:map/Map";
import Result "mo:base/Result";
import Blob "mo:base/Blob";
import Principal "mo:base/Principal";
import Text "mo:base/Text";
import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import Float "mo:base/Float";
import Int "mo:base/Int";
import Nat "mo:base/Nat";
import Time "mo:base/Time";
import Json "mo:json";
import HttpTypes "mo:http-types";
import Vector "mo:vector";

import Mcp "mo:mcp-motoko-sdk/mcp/Mcp";
import McpTypes "mo:mcp-motoko-sdk/mcp/Types";
import AuthTypes "mo:mcp-motoko-sdk/auth/Types";
import ApiKey "mo:mcp-motoko-sdk/auth/ApiKey";
import AuthState "mo:mcp-motoko-sdk/auth/State";
import AuthCleanup "mo:mcp-motoko-sdk/auth/Cleanup";
import HttpHandler "mo:mcp-motoko-sdk/mcp/HttpHandler";
import SrvTypes "mo:mcp-motoko-sdk/server/Types";
import Cleanup "mo:mcp-motoko-sdk/mcp/Cleanup";
import State "mo:mcp-motoko-sdk/mcp/State";
import HttpAssets "mo:mcp-motoko-sdk/mcp/HttpAssets";
import Beacon "mo:mcp-motoko-sdk/mcp/Beacon";

shared ({ caller = deployer }) persistent actor class McpServer() = self {

  // --- HEALTH LOG DATA MODEL ---

  type EntryKind = { #workout; #sleep; #medication; #habit; #meal };

  type Entry = {
    id : Nat;
    createdAt : Int; // nanoseconds since epoch
    day : Nat; // days since epoch, for windows and streaks
    kind : EntryKind;
    name : Text; // activity / "sleep" / med name / habit name
    amount : Float; // workout minutes / sleep hours / 1.0 otherwise
    detail : ?Text; // intensity / sleep quality / dose
    notes : ?Text;
  };

  let nanosPerDay : Int = 86_400_000_000_000;

  // Structured macros for #meal entries, keyed by entry id. Kept out of Entry
  // so the stable Entry type stays upgrade-compatible with v0.1 data.
  type MealMacros = { calories : ?Float; proteinGrams : ?Float };

  var nextEntryId : Nat = 1;
  let userEntriesV2 : Map.Map<Principal, Vector.Vector<Entry>> = Map.new();
  let mealMacros : Map.Map<Nat, MealMacros> = Map.new();

  // --- MCP SERVER PLUMBING ---

  var stable_http_assets : HttpAssets.StableEntries = [];
  transient let http_assets = HttpAssets.init(stable_http_assets);

  let appContext : McpTypes.AppContext = State.init([]);

  let authContext : AuthTypes.AuthContext = AuthState.initApiKey(deployer);

  Cleanup.startCleanupTimer<system>(appContext);
  AuthCleanup.startCleanupTimer<system>(authContext);

  // Prometheus usage beacon — reports anonymized usage to the tracker canister.
  transient let beaconContext : Beacon.BeaconContext = Beacon.init(
    Principal.fromText("m63pw-fqaaa-aaaai-q33pa-cai"),
    ?(15 * 60),
  );
  Beacon.startTimer<system>(beaconContext);

  // --- HELPERS ---

  func today() : Nat { Int.abs(Time.now() / nanosPerDay) };

  func kindToText(k : EntryKind) : Text {
    switch (k) {
      case (#workout) "workout";
      case (#sleep) "sleep";
      case (#medication) "medication";
      case (#habit) "habit";
      case (#meal) "meal";
    };
  };

  func kindFromText(t : Text) : ?EntryKind {
    switch (t) {
      case ("workout") ?#workout;
      case ("sleep") ?#sleep;
      case ("medication") ?#medication;
      case ("habit") ?#habit;
      case ("meal") ?#meal;
      case (_) null;
    };
  };

  func callerPrincipal(auth : ?AuthTypes.AuthInfo) : ?Principal {
    switch (auth) {
      case (?info) ?info.principal;
      case (null) null;
    };
  };

  func entriesFor(p : Principal) : Vector.Vector<Entry> {
    switch (Map.get(userEntriesV2, phash, p)) {
      case (?v) v;
      case (null) {
        let v = Vector.new<Entry>();
        Map.set(userEntriesV2, phash, p, v);
        v;
      };
    };
  };

  func optText(args : McpTypes.JsonValue, field : Text) : ?Text {
    Result.toOption(Json.getAsText(args, field));
  };

  // Accepts either a JSON number or a numeric string for robustness with agents.
  func optFloat(args : McpTypes.JsonValue, field : Text) : ?Float {
    switch (Result.toOption(Json.getAsFloat(args, field))) {
      case (?f) ?f;
      case (null) {
        switch (Result.toOption(Json.getAsNat(args, field))) {
          case (?n) ?Float.fromInt(n);
          case (null) null;
        };
      };
    };
  };

  func errorResult(msg : Text) : McpTypes.CallToolResult {
    { content = [#text({ text = msg })]; isError = true; structuredContent = null };
  };

  func okResult(payload : Json.Json) : McpTypes.CallToolResult {
    {
      content = [#text({ text = Json.stringify(payload, null) })];
      isError = false;
      structuredContent = ?payload;
    };
  };

  func optJsonText(t : ?Text) : Json.Json {
    switch (t) { case (?v) Json.str(v); case (null) Json.nullable() };
  };

  func optJsonFloat(f : ?Float) : Json.Json {
    switch (f) { case (?v) Json.float(v); case (null) Json.nullable() };
  };

  func entryToJson(e : Entry) : Json.Json {
    let base = [
      ("id", Json.int(e.id)),
      ("kind", Json.str(kindToText(e.kind))),
      ("name", Json.str(e.name)),
      ("amount", Json.float(e.amount)),
      ("detail", optJsonText(e.detail)),
      ("notes", optJsonText(e.notes)),
      ("days_ago", Json.int(today() - e.day)),
      ("created_at_ns", Json.int(e.createdAt)),
    ];
    switch (e.kind, Map.get(mealMacros, nhash, e.id)) {
      case (#meal, ?m) {
        Json.obj(Array.append(base, [
          ("calories", optJsonFloat(m.calories)),
          ("protein_grams", optJsonFloat(m.proteinGrams)),
        ]));
      };
      case (_, _) Json.obj(base);
    };
  };

  func addEntry(p : Principal, kind : EntryKind, name : Text, amount : Float, detail : ?Text, notes : ?Text) : Entry {
    let e : Entry = {
      id = nextEntryId;
      createdAt = Time.now();
      day = today();
      kind = kind;
      name = name;
      amount = amount;
      detail = detail;
      notes = notes;
    };
    nextEntryId += 1;
    Vector.add(entriesFor(p), e);
    e;
  };

  func loggedResponse(e : Entry, message : Text) : Json.Json {
    Json.obj([
      ("message", Json.str(message)),
      ("entry", entryToJson(e)),
    ]);
  };

  // --- TOOL SCHEMAS ---

  func schemaProp(name : Text, jsonType : Text, description : Text) : (Text, Json.Json) {
    (name, Json.obj([("type", Json.str(jsonType)), ("description", Json.str(description))]));
  };

  func objSchema(props : [(Text, Json.Json)], required : [Text]) : Json.Json {
    Json.obj([
      ("type", Json.str("object")),
      ("properties", Json.obj(props)),
      ("required", Json.arr(Array.map<Text, Json.Json>(required, Json.str))),
    ]);
  };

  transient let loggedEntrySchema : Json.Json = objSchema(
    [
      schemaProp("message", "string", "Confirmation message."),
      ("entry", Json.obj([("type", Json.str("object"))])),
    ],
    ["message"],
  );

  transient let tools : [McpTypes.Tool] = [
    {
      name = "log_workout";
      title = ?"Log Workout";
      description = ?"Record a workout: activity name, duration in minutes, optional intensity (light/moderate/hard) and notes.";
      payment = null;
      inputSchema = objSchema(
        [
          schemaProp("activity", "string", "What you did, e.g. 'run', 'yoga', 'weights'."),
          schemaProp("duration_minutes", "number", "How long the workout lasted, in minutes."),
          schemaProp("intensity", "string", "Optional: light, moderate, or hard."),
          schemaProp("notes", "string", "Optional free-form notes."),
        ],
        ["activity", "duration_minutes"],
      );
      outputSchema = ?loggedEntrySchema;
    },
    {
      name = "log_sleep";
      title = ?"Log Sleep";
      description = ?"Record last night's sleep: hours slept, optional quality rating 1-5 and notes.";
      payment = null;
      inputSchema = objSchema(
        [
          schemaProp("hours", "number", "Hours slept, e.g. 7.5."),
          schemaProp("quality", "number", "Optional quality rating from 1 (awful) to 5 (great)."),
          schemaProp("notes", "string", "Optional free-form notes."),
        ],
        ["hours"],
      );
      outputSchema = ?loggedEntrySchema;
    },
    {
      name = "log_medication";
      title = ?"Log Medication";
      description = ?"Record that a medication or supplement was taken: name, optional dose and notes.";
      payment = null;
      inputSchema = objSchema(
        [
          schemaProp("name", "string", "Medication or supplement name."),
          schemaProp("dose", "string", "Optional dose, e.g. '200mg'."),
          schemaProp("notes", "string", "Optional free-form notes."),
        ],
        ["name"],
      );
      outputSchema = ?loggedEntrySchema;
    },
    {
      name = "log_habit";
      title = ?"Log Habit Check-in";
      description = ?"Check in a named daily habit (e.g. 'meditate', 'no sugar'). One check-in per day builds a streak.";
      payment = null;
      inputSchema = objSchema(
        [
          schemaProp("habit", "string", "Habit name. Use the same name each day to build a streak."),
          schemaProp("notes", "string", "Optional free-form notes."),
        ],
        ["habit"],
      );
      outputSchema = ?loggedEntrySchema;
    },
    {
      name = "log_meal";
      title = ?"Log Meal";
      description = ?"Record a meal: what you ate, with optional calories, protein grams, and notes.";
      payment = null;
      inputSchema = objSchema(
        [
          schemaProp("meal", "string", "What you ate, e.g. 'chicken and rice' or 'breakfast: eggs and toast'."),
          schemaProp("calories", "number", "Optional estimated calories."),
          schemaProp("protein_grams", "number", "Optional grams of protein."),
          schemaProp("notes", "string", "Optional free-form notes."),
        ],
        ["meal"],
      );
      outputSchema = ?loggedEntrySchema;
    },
    {
      name = "list_entries";
      title = ?"List Entries";
      description = ?"List your recent entries, newest first. Optionally filter by kind (workout/sleep/medication/habit/meal) and window in days.";
      payment = null;
      inputSchema = objSchema(
        [
          schemaProp("kind", "string", "Optional filter: workout, sleep, medication, habit, or meal."),
          schemaProp("days", "number", "Optional: only entries from the last N days (default 7)."),
          schemaProp("limit", "number", "Optional: max entries to return (default 50)."),
        ],
        [],
      );
      outputSchema = ?objSchema(
        [
          schemaProp("count", "number", "Number of entries returned."),
          ("entries", Json.obj([("type", Json.str("array"))])),
        ],
        ["count", "entries"],
      );
    },
    {
      name = "get_summary";
      title = ?"Get Summary & Trends";
      description = ?"Summarize the last N days (default 7): workout totals, average sleep, meal calories/protein, medication adherence, habit streaks, and nudges.";
      payment = null;
      inputSchema = objSchema(
        [schemaProp("days", "number", "Window in days (default 7).")],
        [],
      );
      outputSchema = ?objSchema(
        [("summary", Json.obj([("type", Json.str("object"))]))],
        ["summary"],
      );
    },
    {
      name = "delete_entry";
      title = ?"Delete Entry";
      description = ?"Delete one of your entries by its id.";
      payment = null;
      inputSchema = objSchema(
        [schemaProp("id", "number", "The entry id to delete.")],
        ["id"],
      );
      outputSchema = ?objSchema(
        [schemaProp("message", "string", "Confirmation message.")],
        ["message"],
      );
    },
  ];

  // --- TOOL IMPLEMENTATIONS ---

  type ToolCb = (Result.Result<McpTypes.CallToolResult, McpTypes.HandlerError>) -> ();

  func requireAuth(auth : ?AuthTypes.AuthInfo, cb : ToolCb) : ?Principal {
    switch (callerPrincipal(auth)) {
      case (?p) ?p;
      case (null) {
        cb(#ok(errorResult("Authentication required: call this tool with a valid x-api-key.")));
        null;
      };
    };
  };

  func logWorkoutTool(args : McpTypes.JsonValue, auth : ?AuthTypes.AuthInfo, cb : ToolCb) : async () {
    let ?p = requireAuth(auth, cb) else return;
    let ?activity = optText(args, "activity") else return cb(#ok(errorResult("Missing 'activity'.")));
    let ?minutes = optFloat(args, "duration_minutes") else return cb(#ok(errorResult("Missing or non-numeric 'duration_minutes'.")));
    if (minutes <= 0.0 or minutes > 1440.0) return cb(#ok(errorResult("'duration_minutes' must be between 0 and 1440.")));
    let e = addEntry(p, #workout, activity, minutes, optText(args, "intensity"), optText(args, "notes"));
    cb(#ok(okResult(loggedResponse(e, "Logged " # Float.toText(minutes) # " min of " # activity # "."))));
  };

  func logSleepTool(args : McpTypes.JsonValue, auth : ?AuthTypes.AuthInfo, cb : ToolCb) : async () {
    let ?p = requireAuth(auth, cb) else return;
    let ?hours = optFloat(args, "hours") else return cb(#ok(errorResult("Missing or non-numeric 'hours'.")));
    if (hours <= 0.0 or hours > 24.0) return cb(#ok(errorResult("'hours' must be between 0 and 24.")));
    let quality : ?Text = switch (optFloat(args, "quality")) {
      case (?q) {
        if (q < 1.0 or q > 5.0) return cb(#ok(errorResult("'quality' must be 1-5.")));
        ?("quality " # Nat.toText(Int.abs(Float.toInt(q))) # "/5");
      };
      case (null) null;
    };
    let e = addEntry(p, #sleep, "sleep", hours, quality, optText(args, "notes"));
    cb(#ok(okResult(loggedResponse(e, "Logged " # Float.toText(hours) # " hours of sleep."))));
  };

  func logMedicationTool(args : McpTypes.JsonValue, auth : ?AuthTypes.AuthInfo, cb : ToolCb) : async () {
    let ?p = requireAuth(auth, cb) else return;
    let ?name = optText(args, "name") else return cb(#ok(errorResult("Missing 'name'.")));
    let e = addEntry(p, #medication, name, 1.0, optText(args, "dose"), optText(args, "notes"));
    cb(#ok(okResult(loggedResponse(e, "Logged dose of " # name # "."))));
  };

  func logHabitTool(args : McpTypes.JsonValue, auth : ?AuthTypes.AuthInfo, cb : ToolCb) : async () {
    let ?p = requireAuth(auth, cb) else return;
    let ?habit = optText(args, "habit") else return cb(#ok(errorResult("Missing 'habit'.")));
    // One check-in per habit per day keeps streak math honest.
    let existing = Vector.toArray(entriesFor(p));
    let d = today();
    for (e in existing.vals()) {
      if (e.kind == #habit and e.name == habit and e.day == d) {
        return cb(#ok(okResult(loggedResponse(e, "'" # habit # "' was already checked in today. Streak: " # Nat.toText(habitStreak(existing, habit)) # " days."))));
      };
    };
    let e = addEntry(p, #habit, habit, 1.0, null, optText(args, "notes"));
    let streak = habitStreak(Vector.toArray(entriesFor(p)), habit);
    cb(#ok(okResult(loggedResponse(e, "Checked in '" # habit # "'. Streak: " # Nat.toText(streak) # " days."))));
  };

  func logMealTool(args : McpTypes.JsonValue, auth : ?AuthTypes.AuthInfo, cb : ToolCb) : async () {
    let ?p = requireAuth(auth, cb) else return;
    let ?meal = optText(args, "meal") else return cb(#ok(errorResult("Missing 'meal'.")));
    let calories = optFloat(args, "calories");
    switch (calories) {
      case (?c) { if (c < 0.0 or c > 20_000.0) return cb(#ok(errorResult("'calories' must be between 0 and 20000."))) };
      case (null) {};
    };
    let protein = optFloat(args, "protein_grams");
    switch (protein) {
      case (?g) { if (g < 0.0 or g > 1_000.0) return cb(#ok(errorResult("'protein_grams' must be between 0 and 1000."))) };
      case (null) {};
    };
    // Human-readable macros go in detail; structured values live in mealMacros.
    let parts = Buffer.Buffer<Text>(2);
    switch (calories) { case (?c) parts.add(Float.toText(c) # " cal"); case (null) {} };
    switch (protein) { case (?g) parts.add(Float.toText(g) # "g protein"); case (null) {} };
    let detail : ?Text = if (parts.size() == 0) null else ?Text.join(", ", parts.vals());
    let e = addEntry(p, #meal, meal, 1.0, detail, optText(args, "notes"));
    Map.set(mealMacros, nhash, e.id, { calories = calories; proteinGrams = protein });
    cb(#ok(okResult(loggedResponse(e, "Logged meal: " # meal # "."))));
  };

  func listEntriesTool(args : McpTypes.JsonValue, auth : ?AuthTypes.AuthInfo, cb : ToolCb) : async () {
    let ?p = requireAuth(auth, cb) else return;
    let kindFilter : ?EntryKind = switch (optText(args, "kind")) {
      case (?k) {
        switch (kindFromText(k)) {
          case (?kk) ?kk;
          case (null) return cb(#ok(errorResult("Unknown kind '" # k # "'. Use workout, sleep, medication, habit, or meal.")));
        };
      };
      case (null) null;
    };
    let days : Nat = switch (optFloat(args, "days")) {
      case (?d) { if (d < 1.0) 1 else Int.abs(Float.toInt(d)) };
      case (null) 7;
    };
    let limit : Nat = switch (optFloat(args, "limit")) {
      case (?l) { if (l < 1.0) 1 else Int.abs(Float.toInt(l)) };
      case (null) 50;
    };
    let cutoff : Int = today() - days;
    let all = Vector.toArray(entriesFor(p));
    let out = Buffer.Buffer<Json.Json>(16);
    var i = all.size();
    label collect while (i > 0) {
      i -= 1;
      let e = all[i];
      if (e.day < cutoff) continue collect;
      switch (kindFilter) {
        case (?k) { if (e.kind != k) continue collect };
        case (null) {};
      };
      out.add(entryToJson(e));
      if (out.size() >= limit) break collect;
    };
    cb(#ok(okResult(Json.obj([
      ("count", Json.int(out.size())),
      ("entries", Json.arr(Buffer.toArray(out))),
    ]))));
  };

  func habitStreak(all : [Entry], habit : Text) : Nat {
    // Current streak: consecutive days with a check-in, ending today or yesterday.
    var streak : Nat = 0;
    var d : Int = today();
    if (not hasHabitOn(all, habit, d)) { d -= 1 };
    label walk loop {
      if (d < 0 or not hasHabitOn(all, habit, d)) break walk;
      streak += 1;
      d -= 1;
    };
    streak;
  };

  func hasHabitOn(all : [Entry], habit : Text, day : Int) : Bool {
    for (e in all.vals()) {
      if (e.kind == #habit and e.name == habit and e.day == day) return true;
    };
    false;
  };

  func getSummaryTool(args : McpTypes.JsonValue, auth : ?AuthTypes.AuthInfo, cb : ToolCb) : async () {
    let ?p = requireAuth(auth, cb) else return;
    let days : Nat = switch (optFloat(args, "days")) {
      case (?d) { if (d < 1.0) 1 else Int.abs(Float.toInt(d)) };
      case (null) 7;
    };
    let cutoff : Int = today() - days;
    let all = Vector.toArray(entriesFor(p));

    var workoutCount = 0;
    var workoutMinutes = 0.0;
    var lastWorkoutDay : Int = -1;
    var sleepNights = 0;
    var sleepHours = 0.0;
    var mealCount = 0;
    var mealCalories = 0.0;
    var mealProtein = 0.0;
    var caloriesLogged = false;
    var proteinLogged = false;
    let mealDays = Map.new<Nat, Bool>();
    let medCounts = Map.new<Text, Nat>();
    let habitNames = Map.new<Text, Bool>();

    for (e in all.vals()) {
      if (e.kind == #habit) Map.set(habitNames, thash, e.name, true);
      if (e.day >= cutoff) {
        switch (e.kind) {
          case (#workout) {
            workoutCount += 1;
            workoutMinutes += e.amount;
            if (e.day > lastWorkoutDay) lastWorkoutDay := e.day;
          };
          case (#sleep) { sleepNights += 1; sleepHours += e.amount };
          case (#medication) {
            let c = switch (Map.get(medCounts, thash, e.name)) { case (?n) n; case (null) 0 };
            Map.set(medCounts, thash, e.name, c + 1);
          };
          case (#habit) {};
          case (#meal) {
            mealCount += 1;
            Map.set(mealDays, nhash, e.day, true);
            switch (Map.get(mealMacros, nhash, e.id)) {
              case (?m) {
                switch (m.calories) {
                  case (?c) { mealCalories += c; caloriesLogged := true };
                  case (null) {};
                };
                switch (m.proteinGrams) {
                  case (?g) { mealProtein += g; proteinLogged := true };
                  case (null) {};
                };
              };
              case (null) {};
            };
          };
        };
      };
    };

    let meds = Buffer.Buffer<Json.Json>(4);
    for ((name, count) in Map.entries(medCounts)) {
      meds.add(Json.obj([("name", Json.str(name)), ("doses_in_window", Json.int(count))]));
    };

    let habits = Buffer.Buffer<Json.Json>(4);
    let nudges = Buffer.Buffer<Json.Json>(4);
    for ((habit, _) in Map.entries(habitNames)) {
      let streak = habitStreak(all, habit);
      habits.add(Json.obj([("habit", Json.str(habit)), ("current_streak_days", Json.int(streak))]));
      if (streak > 0 and not hasHabitOn(all, habit, today())) {
        nudges.add(Json.str("'" # habit # "' streak is at " # Nat.toText(streak) # " days but you haven't checked in today."));
      };
    };

    if (workoutCount == 0) {
      nudges.add(Json.str("No workouts logged in the last " # Nat.toText(days) # " days."));
    } else if (lastWorkoutDay >= 0 and today() - lastWorkoutDay >= 3) {
      nudges.add(Json.str("Last workout was " # Int.toText(today() - lastWorkoutDay) # " days ago."));
    };
    if (sleepNights > 0 and sleepHours / Float.fromInt(sleepNights) < 7.0) {
      nudges.add(Json.str("Average sleep is under 7 hours."));
    };

    let avgSleep : Json.Json = if (sleepNights == 0) Json.nullable() else Json.float(sleepHours / Float.fromInt(sleepNights));
    let summary = Json.obj([
      ("window_days", Json.int(days)),
      ("workouts", Json.obj([
        ("count", Json.int(workoutCount)),
        ("total_minutes", Json.float(workoutMinutes)),
      ])),
      ("sleep", Json.obj([
        ("nights_logged", Json.int(sleepNights)),
        ("average_hours", avgSleep),
      ])),
      ("meals", Json.obj([
        ("count", Json.int(mealCount)),
        ("days_logged", Json.int(Map.size(mealDays))),
        ("total_calories", if (caloriesLogged) Json.float(mealCalories) else Json.nullable()),
        ("avg_calories_per_logged_day", if (caloriesLogged and Map.size(mealDays) > 0) Json.float(mealCalories / Float.fromInt(Map.size(mealDays))) else Json.nullable()),
        ("total_protein_grams", if (proteinLogged) Json.float(mealProtein) else Json.nullable()),
      ])),
      ("medications", Json.arr(Buffer.toArray(meds))),
      ("habits", Json.arr(Buffer.toArray(habits))),
      ("nudges", Json.arr(Buffer.toArray(nudges))),
    ]);
    cb(#ok(okResult(Json.obj([("summary", summary)]))));
  };

  func deleteEntryTool(args : McpTypes.JsonValue, auth : ?AuthTypes.AuthInfo, cb : ToolCb) : async () {
    let ?p = requireAuth(auth, cb) else return;
    let ?idF = optFloat(args, "id") else return cb(#ok(errorResult("Missing or non-numeric 'id'.")));
    let id = Int.abs(Float.toInt(idF));
    let v = entriesFor(p);
    let all = Vector.toArray(v);
    let kept = Array.filter<Entry>(all, func(e) { e.id != id });
    if (kept.size() == all.size()) {
      return cb(#ok(errorResult("No entry with id " # Nat.toText(id) # " in your log.")));
    };
    Vector.clear(v);
    for (e in kept.vals()) Vector.add(v, e);
    Map.delete(mealMacros, nhash, id);
    cb(#ok(okResult(Json.obj([("message", Json.str("Deleted entry " # Nat.toText(id) # "."))]))));
  };

  // --- SDK CONFIG & HTTP WIRING ---

  transient let mcpConfig : McpTypes.McpConfig = {
    self = Principal.fromActor(self);
    allowanceUrl = null;
    serverInfo = {
      name = "health-habit-tracker";
      title = "Health & Habit Tracker";
      version = "0.2.1";
    };
    resources = [];
    resourceReader = func(uri) { Map.get(appContext.resourceContents, thash, uri) };
    tools = tools;
    toolImplementations = [
      ("log_workout", logWorkoutTool),
      ("log_sleep", logSleepTool),
      ("log_medication", logMedicationTool),
      ("log_habit", logHabitTool),
      ("log_meal", logMealTool),
      ("list_entries", listEntriesTool),
      ("get_summary", getSummaryTool),
      ("delete_entry", deleteEntryTool),
    ];
    beacon = ?beaconContext;
  };

  transient let mcpServer = Mcp.createServer(mcpConfig);

  private func _create_http_context() : HttpHandler.Context {
    return {
      self = Principal.fromActor(self);
      active_streams = appContext.activeStreams;
      mcp_server = mcpServer;
      streaming_callback = http_request_streaming_callback;
      auth = ?authContext;
      http_asset_cache = ?http_assets.cache;
      mcp_path = ?"/mcp";
    };
  };

  public query func http_request(req : SrvTypes.HttpRequest) : async SrvTypes.HttpResponse {
    let ctx : HttpHandler.Context = _create_http_context();
    switch (HttpHandler.http_request(ctx, req)) {
      case (?mcpResponse) { mcpResponse };
      case (null) {
        if (req.url == "/") {
          // Query responses need certification on the non-raw gateway; punt to an
          // update call, which is exempt.
          {
            status_code = 204;
            headers = [];
            body = Blob.fromArray([]);
            upgrade = ?true;
            streaming_strategy = null;
          };
        } else {
          {
            status_code = 404;
            headers = [];
            body = Blob.fromArray([]);
            upgrade = null;
            streaming_strategy = null;
          };
        };
      };
    };
  };

  public shared func http_request_update(req : SrvTypes.HttpRequest) : async SrvTypes.HttpResponse {
    let ctx : HttpHandler.Context = _create_http_context();
    switch (await HttpHandler.http_request_update(ctx, req)) {
      case (?res) { res };
      case (null) {
        if (req.url == "/") {
          {
            status_code = 200;
            headers = [("Content-Type", "text/html")];
            body = Text.encodeUtf8("<h1>Health & Habit Tracker MCP Server</h1><p>MCP endpoint at <code>/mcp</code>. Authenticate with an <code>x-api-key</code> header.</p>");
            upgrade = null;
            streaming_strategy = null;
          };
        } else {
          {
            status_code = 404;
            headers = [];
            body = Blob.fromArray([]);
            upgrade = null;
            streaming_strategy = null;
          };
        };
      };
    };
  };

  public query func http_request_streaming_callback(token : HttpTypes.StreamingToken) : async ?HttpTypes.StreamingCallbackResponse {
    let ctx : HttpHandler.Context = _create_http_context();
    return HttpHandler.http_request_streaming_callback(ctx, token);
  };

  system func preupgrade() {
    stable_http_assets := HttpAssets.preupgrade(http_assets);
  };

  system func postupgrade() {
    HttpAssets.postupgrade(http_assets);
  };

  /// Mint a stable API key bound to the caller's principal.
  /// The raw key is returned once and never stored in plaintext.
  public shared (msg) func create_my_api_key(name : Text, scopes : [Text]) : async Text {
    return await ApiKey.create_my_api_key(authContext, msg.caller, name, scopes);
  };
};
