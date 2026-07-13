# Health & Habit Tracker — MCP Server

A private, fully on-chain health and habit log for AI agents, built for the
[Prometheus Protocol](https://prometheusprotocol.org) app store on the Internet Computer.

Your agent logs workouts, sleep, medications, and habit check-ins on your behalf,
then reports trends and nudges you when a streak is about to break. Every entry is
scoped to the caller's principal — no other user (and no platform) can read your log.

## Why this exists

The Prometheus app store has the primitives of an agent economy (wallets, secrets,
messaging, oracles) but almost nothing in the life & household layer. This server
fills the "Health & Habit Tracker" gap: a log Big Tech can't mine, on infrastructure
that can't silently delete your data.

## Tools

| Tool | What it does |
|------|--------------|
| `log_workout` | Record activity, duration, intensity, notes |
| `log_sleep` | Record hours slept and quality (1–5) |
| `log_medication` | Record a med/supplement dose taken |
| `log_habit` | Daily check-in for a named habit (builds streaks) |
| `list_entries` | List recent entries, optionally filtered by kind |
| `get_summary` | Trends over N days: workout totals, avg sleep, med adherence, habit streaks, and nudges |
| `delete_entry` | Remove an entry by id |

## Privacy model

All data is partitioned per principal. Tool calls require authentication
(`x-api-key` header); each key is bound to the principal that minted it, and every
read/write only touches that principal's partition. vetKey client-side encryption is
a planned enhancement for at-rest privacy against node providers.

## Getting an API key

```bash
dfx canister call health_tracker create_my_api_key '("my key", vec {})'
```

The returned key goes in the `x-api-key` header. It is shown only once.

## Local development

```bash
mops install
dfx start --background
dfx deploy
```

MCP endpoint: `http://<canister-id>.localhost:4943/mcp` (or
`http://127.0.0.1:4943/mcp` with a `Host: <canister-id>.localhost` header).

## Mainnet

```bash
dfx deploy --network ic
```

MCP endpoint: `https://<canister-id>.icp0.io/mcp`
