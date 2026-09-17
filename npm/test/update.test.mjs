// Tests for the update check in the npm shim. They never reach the network: every request goes
// through a stub. Run them with: node --test npm/test/*.test.mjs
import { EventEmitter } from "node:events";
import { mkdtempSync, writeFileSync } from "node:fs";
import { tmpdir } from "node:os";
import { join } from "node:path";
import assert from "node:assert/strict";
import test from "node:test";

import update from "../polio/bin/update.js";

const {
  CHECK_INTERVAL_MS,
  DEFAULT_REGISTRY,
  RETRY_INTERVAL_MS,
  beginFetch,
  fetchLatest,
  isEnabled,
  isNewer,
  manifestUrl,
  noteFor,
  parseNpmrc,
  readCache,
  registryFor,
  start,
  stateDir,
  wantsNote,
  writeCache,
} = update;

/** A cache that is old enough for a lookup to be due. */
const stale = { checkedAt: 1, triedAt: 1, latest: "" };

/** A directory of its own for one test. */
const scratch = () => mkdtempSync(join(tmpdir(), "polio-shim-"));

/** A request function that answers with body and status, as node:https would. */
function answering(body, statusCode = 200) {
  const calls = [];
  const get = (url, options, callback) => {
    calls.push(url);
    const response = new EventEmitter();
    response.statusCode = statusCode;
    response.setEncoding = () => {};
    response.resume = () => {};
    callback(response);
    queueMicrotask(() => {
      response.emit("data", body);
      response.emit("end");
    });
    const request = new EventEmitter();
    request.destroy = () => {};
    return request;
  };
  get.calls = calls;
  return get;
}

/** A request function that never answers. */
function silent() {
  const get = () => {
    const request = new EventEmitter();
    request.destroy = () => {};
    return request;
  };
  return get;
}

/** Lets pending promises settle. */
const settle = () => new Promise((resolve) => setImmediate(resolve));

test("stateDir follows the rules the binary uses", () => {
  assert.equal(stateDir({ POLIO_HOME: "/one" }), "/one");
  assert.equal(stateDir({ XDG_STATE_HOME: "/two", HOME: "/home/x" }), "/two/polio");
  assert.equal(stateDir({ HOME: "/home/x" }), "/home/x/.local/state/polio");
  assert.equal(stateDir({ POLIO_HOME: "/one", XDG_STATE_HOME: "/two" }), "/one");
});

test("the check is on unless the setting says otherwise", () => {
  const dir = scratch();
  assert.equal(isEnabled(dir), true, "a missing setting means on");

  writeFileSync(join(dir, "update.json"), '{"version":1,"enabled":false}\n');
  assert.equal(isEnabled(dir), false);

  writeFileSync(join(dir, "update.json"), '{"version":1,"enabled":true}\n');
  assert.equal(isEnabled(dir), true);

  writeFileSync(join(dir, "update.json"), "{not json");
  assert.equal(isEnabled(dir), true, "a broken setting means on");
});

test("the cache survives a round trip, and a missing or broken one reads as nothing", () => {
  const dir = scratch();
  assert.equal(readCache(dir), null, "no record yet");

  writeCache(dir, { checkedAt: 1234, triedAt: 1234, latest: "1.2.3" });
  assert.deepEqual(readCache(dir), { checkedAt: 1234, triedAt: 1234, latest: "1.2.3" });

  writeFileSync(join(dir, "npm-check.json"), "}{");
  assert.equal(readCache(dir), null, "a broken record");
});

test("npmrc settings are read, comments and all", () => {
  const settings = parseNpmrc([
    "# a comment",
    "; another",
    "",
    "registry=https://mirror.example.com/",
    '@dovaogedot:registry = "https://scoped.example.com"',
    "broken-line",
    "//mirror.example.com/:_authToken=secret",
  ].join("\n"));
  assert.equal(settings.get("registry"), "https://mirror.example.com/");
  assert.equal(settings.get("@dovaogedot:registry"), "https://scoped.example.com");
  assert.equal(settings.has("broken-line"), false);
});

test("the registry follows the environment, then the npmrc, then npmjs.com", () => {
  assert.equal(registryFor({}, ""), DEFAULT_REGISTRY);
  assert.equal(registryFor({}, "registry=https://mirror.example.com/"), "https://mirror.example.com");
  assert.equal(
    registryFor({}, "registry=https://mirror.example.com\n@dovaogedot:registry=https://scoped.example.com"),
    "https://scoped.example.com",
    "the setting for this scope wins",
  );
  assert.equal(
    registryFor({ npm_config_registry: "https://env.example.com" }, "registry=https://mirror.example.com"),
    "https://env.example.com",
    "the environment wins over the file",
  );
  assert.equal(registryFor({ NPM_CONFIG_REGISTRY: "https://shouty.example.com" }, ""), "https://shouty.example.com");
});

test("the manifest url hangs off the registry", () => {
  const dir = scratch();
  writeFileSync(join(dir, "npmrc"), "registry=https://mirror.example.com/\n");
  const url = manifestUrl({ npm_config_userconfig: join(dir, "npmrc") });
  assert.equal(url, "https://mirror.example.com/@dovaogedot%2Fpolio/latest");
});

test("the first run only starts the clock", async () => {
  const dir = scratch();
  const get = answering('{"version":"9.9.9"}');
  const check = start(["sync"], { env: { POLIO_HOME: dir }, current: "0.4.2", now: 5_000, get });

  await settle();
  assert.equal(get.calls.length, 0, "a polio just installed has nothing newer to find");
  assert.equal(check.finish(), null);
  assert.deepEqual(readCache(dir), { checkedAt: 5_000, triedAt: 5_000, latest: "" });
});

test("isNewer compares versions as numbers", () => {
  assert.equal(isNewer("0.4.2", "0.4.1"), true);
  assert.equal(isNewer("0.10.0", "0.9.9"), true);
  assert.equal(isNewer("1.0.0", "0.99.99"), true);
  assert.equal(isNewer("0.4.2", "0.4.2"), false);
  assert.equal(isNewer("0.4.1", "0.4.2"), false);
  assert.equal(isNewer("", "0.4.2"), false);
  assert.equal(isNewer("nonsense", "0.4.2"), false);
  assert.equal(isNewer("0.5", "0.4.9"), true);
});

test("only a sync that prints for a person gets a note", () => {
  assert.equal(wantsNote(["sync"]), true);
  assert.equal(wantsNote(["sync", "-f"]), true);
  assert.equal(wantsNote(["sync", "-q"]), false);
  assert.equal(wantsNote(["sync", "--quiet"]), false);
  assert.equal(wantsNote(["sync", "-s"]), false);
  assert.equal(wantsNote(["sync", "--shush"]), false);
  assert.equal(wantsNote(["sync", "--porcelain"]), false);
  assert.equal(wantsNote(["status"]), false);
  assert.equal(wantsNote([]), false);
});

test("the note names the release and the command", () => {
  assert.equal(noteFor("0.5.0"), "polio 0.5.0 is out — run: npm install -g @dovaogedot/polio@latest");
});

test("a lookup reads the version out of the answer", async () => {
  const version = await fetchLatest({ get: answering('{"version":"0.4.2"}') });
  assert.equal(version, "0.4.2");
});

test("a lookup gives nothing on a bad answer", async () => {
  assert.equal(await fetchLatest({ get: answering("{}", 404) }), null, "a status that is not 200");
  assert.equal(await fetchLatest({ get: answering("not json") }), null, "a body that does not parse");
  assert.equal(await fetchLatest({ get: answering("{}") }), null, "an answer without a version");
});

test("a lookup gives nothing when the registry stays silent", async () => {
  const started = Date.now();
  assert.equal(await fetchLatest({ get: silent(), timeoutMs: 20 }), null);
  assert.ok(Date.now() - started < 1000, "the timeout answered");
});

test("a lookup gives nothing when the request cannot start", async () => {
  const get = () => {
    throw new Error("no network");
  };
  assert.equal(await fetchLatest({ get }), null);
});

test("a cancelled lookup answers at once", async () => {
  const attempt = beginFetch({ get: silent(), timeoutMs: 60_000 });
  attempt.cancel();
  assert.equal(await attempt.promise, null);
});

test("a stale cache is refreshed and the note comes from what was found", async () => {
  const dir = scratch();
  writeCache(dir, stale);
  const get = answering('{"version":"9.9.9"}');
  const check = start(["sync"], { env: { POLIO_HOME: dir }, current: "0.4.2", now: 1_000_000_000_000, get });

  await settle();
  assert.equal(get.calls.length, 1, "the registry was asked");
  assert.equal(check.finish(), noteFor("9.9.9"));
  assert.deepEqual(readCache(dir), { checkedAt: 1_000_000_000_000, triedAt: 1_000_000_000_000, latest: "9.9.9" });
});

test("a fresh cache warns without asking again", async () => {
  const dir = scratch();
  writeCache(dir, { checkedAt: 1_000_000_000_000, triedAt: 1_000_000_000_000, latest: "9.9.9" });
  const get = answering('{"version":"9.9.9"}');
  const when = 1_000_000_000_000 + CHECK_INTERVAL_MS - 1;
  const check = start(["sync"], { env: { POLIO_HOME: dir }, current: "0.4.2", now: when, get });

  await settle();
  assert.equal(get.calls.length, 0, "the registry was left alone");
  assert.equal(check.finish(), noteFor("9.9.9"));
});

test("a release that is not newer says nothing", async () => {
  const dir = scratch();
  writeCache(dir, stale);
  const get = answering('{"version":"0.4.2"}');
  const check = start(["sync"], { env: { POLIO_HOME: dir }, current: "0.4.2", now: 1_000_000_000_000, get });

  await settle();
  assert.equal(check.finish(), null);
});

test("nothing is checked when the setting is off or the command is not a sync", () => {
  const dir = scratch();
  writeCache(dir, stale);
  writeFileSync(join(dir, "update.json"), '{"version":1,"enabled":false}\n');
  const get = answering('{"version":"9.9.9"}');

  assert.equal(start(["sync"], { env: { POLIO_HOME: dir }, current: "0.4.2", get }), null, "the setting is off");
  assert.equal(start(["status"], { env: { POLIO_HOME: scratch() }, current: "0.4.2", get }), null, "not a sync");
  assert.equal(get.calls.length, 0, "the registry was left alone");
});

test("a lookup that has not answered records the attempt, not an answer", async () => {
  const dir = scratch();
  const known = 1;
  const when = known + CHECK_INTERVAL_MS + 1;
  writeCache(dir, { checkedAt: known, triedAt: known, latest: "9.9.9" });
  const check = start(["sync"], { env: { POLIO_HOME: dir }, current: "0.4.2", now: when, get: silent() });

  assert.equal(check.finish(), noteFor("9.9.9"), "the note comes from what was already known");
  assert.deepEqual(
    readCache(dir),
    { checkedAt: known, triedAt: when, latest: "9.9.9" },
    "the attempt is recorded and the answer is not",
  );
});

test("an attempt that did not answer comes round again within the hour", async () => {
  const dir = scratch();
  const first = 1_000_000_000_000;
  const get = answering('{"version":"9.9.9"}');
  writeCache(dir, stale);

  start(["sync"], { env: { POLIO_HOME: dir }, current: "0.4.2", now: first, get: silent() }).finish();
  assert.deepEqual(readCache(dir), { checkedAt: stale.checkedAt, triedAt: first, latest: "" });

  const tooSoon = start(["sync"], { env: { POLIO_HOME: dir }, current: "0.4.2", now: first + 1000, get });
  await settle();
  assert.equal(get.calls.length, 0, "a second attempt waits");
  assert.equal(tooSoon.finish(), null);

  const later = start(["sync"], {
    env: { POLIO_HOME: dir },
    current: "0.4.2",
    now: first + RETRY_INTERVAL_MS,
    get,
  });
  await settle();
  assert.equal(get.calls.length, 1, "the attempt comes round");
  assert.equal(later.finish(), noteFor("9.9.9"));
});
