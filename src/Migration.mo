import Map "mo:map/Map";
import { phash } "mo:map/Map";
import Vector "mo:vector";
import Principal "mo:base/Principal";
import McpTypes "mo:mcp-motoko-sdk/mcp/Types";

/// One-time migration for the v0.1 -> v0.2 upgrade.
///
/// v0.2 added #meal to EntryKind. Widening a variant inside the mutable
/// `userEntries` map is memory-incompatible (M0170), so the old map is
/// consumed here and rebuilt as `userEntriesV2`, converting each entry to
/// the widened type. No data is lost; entry ids and timestamps are kept.
///
/// Also consumes the old implicitly-stable `tools` array (now `transient`
/// in the actor) so the persisted v0.1 tool list stops shadowing the
/// tool list compiled into the code.
module {
  public type EntryKindV1 = { #workout; #sleep; #medication; #habit };

  public type EntryV1 = {
    id : Nat;
    createdAt : Int;
    day : Nat;
    kind : EntryKindV1;
    name : Text;
    amount : Float;
    detail : ?Text;
    notes : ?Text;
  };

  public type EntryKind = { #workout; #sleep; #medication; #habit; #meal };

  public type Entry = {
    id : Nat;
    createdAt : Int;
    day : Nat;
    kind : EntryKind;
    name : Text;
    amount : Float;
    detail : ?Text;
    notes : ?Text;
  };

  public func run(
    old : {
      userEntries : Map.Map<Principal, Vector.Vector<EntryV1>>;
      tools : [McpTypes.Tool];
    }
  ) : { userEntriesV2 : Map.Map<Principal, Vector.Vector<Entry>> } {
    let migrated = Map.new<Principal, Vector.Vector<Entry>>();
    for ((p, entries) in Map.entries(old.userEntries)) {
      let converted = Vector.new<Entry>();
      for (e in Vector.vals(entries)) {
        let kind : EntryKind = switch (e.kind) {
          case (#workout) #workout;
          case (#sleep) #sleep;
          case (#medication) #medication;
          case (#habit) #habit;
        };
        Vector.add(converted, { e with kind = kind });
      };
      Map.set(migrated, phash, p, converted);
    };
    { userEntriesV2 = migrated };
  };
};
