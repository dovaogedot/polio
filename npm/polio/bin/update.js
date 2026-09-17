// The check for a newer polio. It lives in the shim, which is already a process, so the check costs
// none of its own, and it asks the npm registry, which knows what `npm install -g` would fetch.
// Nothing here waits: a lookup that has not answered by the time polio exits is dropped, and what it
// found lands in the cache for the next run.
"use strict";

const { mkdirSync, readFileSync, writeFileSync } = require("node:fs");
const { homedir } = require("node:os");
const { join } = require("node:path");

/** How long an answer from the registry stands before the shim asks again. */
const CHECK_INTERVAL_MS = 24 * 60 * 60 * 1000;

/**
 * How long an attempt that never answered holds off the next one. A sync shorter than a round trip to
 * the registry ends before the answer arrives, and nothing waits for it, so the attempt has to come
 * round again sooner than a whole day or a host of quick syncs would never learn of a release.
 */
const RETRY_INTERVAL_MS = 60 * 60 * 1000;

/** How long the registry gets to answer before the request is dropped. */
const REQUEST_TIMEOUT_MS = 2000;

/** The registry npm reads when nothing else names one. */
const DEFAULT_REGISTRY = "https://registry.npmjs.org";

/** The manifest of the release npm would install, under whichever registry serves it. */
const MANIFEST_PATH = "@dovaogedot%2Fpolio/latest";

/** The command that upgrades an npm install. */
const UPGRADE = "npm install -g @dovaogedot/polio@latest";

/** The flags that mean the output is not for a person to read. */
const QUIET = new Set(["-q", "--quiet", "-s", "--shush", "--porcelain"]);

/**
 * Where polio keeps the state of this host: POLIO_HOME, or the polio directory under XDG_STATE_HOME,
 * or under ~/.local/state. It follows Layout in src/Config.scala.
 */
function stateDir(env) {
  if (env.POLIO_HOME) return env.POLIO_HOME;
  const base = env.XDG_STATE_HOME || join(env.HOME || homedir(), ".local/state");
  return join(base, "polio");
}

/** The settings in an npmrc text. Comments, blank lines and lines without a name are left out. */
function parseNpmrc(text) {
  const settings = new Map();
  for (const line of String(text).split("\n")) {
    const bare = line.trim();
    if (!bare || bare.startsWith("#") || bare.startsWith(";")) continue;
    const at = bare.indexOf("=");
    if (at < 0) continue;
    const name = bare.slice(0, at).trim();
    const value = bare.slice(at + 1).trim().replace(/^["']|["']$/g, "");
    if (name) settings.set(name, value);
  }
  return settings;
}

/** The text of the npmrc of this user, or an empty one when there is none. */
function readNpmrc(env) {
  const path = env.npm_config_userconfig || join(env.HOME || homedir(), ".npmrc");
  try {
    return readFileSync(path, "utf8");
  } catch {
    return "";
  }
}

/**
 * The registry to ask: the environment, then the setting for this scope in the npmrc, then its plain
 * registry setting, then npmjs.com. A host that installs from a mirror has to be asked about that
 * mirror, or the check looks somewhere npm never installs from.
 */
function registryFor(env, npmrc) {
  const settings = parseNpmrc(npmrc);
  const chosen =
    env.npm_config_registry ||
    env.NPM_CONFIG_REGISTRY ||
    settings.get("@dovaogedot:registry") ||
    settings.get("registry") ||
    DEFAULT_REGISTRY;
  return String(chosen).replace(/\/+$/, "");
}

/** Where the manifest of the newest release is on this host. */
function manifestUrl(env) {
  return `${registryFor(env, readNpmrc(env))}/${MANIFEST_PATH}`;
}

/** Whether the check is on. A missing or unreadable setting means on, as it does in the binary. */
function isEnabled(dir) {
  try {
    return JSON.parse(readFileSync(join(dir, "update.json"), "utf8")).enabled !== false;
  } catch {
    return true;
  }
}

/**
 * When a lookup last answered and when one was last started, in epoch milliseconds, and the release
 * the answer named. null when this host has no record yet, or none that can be read.
 */
function readCache(dir) {
  try {
    const cache = JSON.parse(readFileSync(join(dir, "npm-check.json"), "utf8"));
    return {
      checkedAt: Number(cache.checkedAt) || 0,
      triedAt: Number(cache.triedAt) || 0,
      latest: String(cache.latest || ""),
    };
  } catch {
    return null;
  }
}

/** Records a lookup. A directory that cannot be written leaves the cache as it was. */
function writeCache(dir, cache) {
  try {
    mkdirSync(dir, { recursive: true });
    writeFileSync(join(dir, "npm-check.json"), JSON.stringify(cache) + "\n");
  } catch {
    // A note is worth nothing beside a sync that works, so a cache that cannot be written is dropped.
  }
}

/** The parts of a version as numbers. A part that does not start with digits counts as zero. */
function numbers(version) {
  return String(version).split(".").map((part) => parseInt(part, 10) || 0);
}

/** Whether candidate names a release newer than current. */
function isNewer(candidate, current) {
  const left = numbers(candidate);
  const right = numbers(current);
  for (let i = 0; i < Math.max(left.length, right.length); i++) {
    const a = left[i] || 0;
    const b = right[i] || 0;
    if (a !== b) return a > b;
  }
  return false;
}

/** Whether this command line is a sync whose output a person reads. */
function wantsNote(argv) {
  return argv[0] === "sync" && !argv.some((arg) => QUIET.has(arg));
}

/** The line that names the release and the command that installs it. */
function noteFor(latest) {
  return `polio ${latest} is out — run: ${UPGRADE}`;
}

/**
 * Asks the registry for the release it calls latest. Returns the pending answer and the way to drop
 * it: cancel() ends the request, so nothing holds the process open once polio is done. The answer is
 * the version, or null on any error, on a body that does not parse, and when it takes longer than
 * timeoutMs. get sends the request, so a test answers without a network.
 */
function beginFetch(options = {}) {
  const {
    url = manifestUrl(process.env),
    timeoutMs = REQUEST_TIMEOUT_MS,
    get = require("node:https").get,
  } = options;
  let drop = () => {};
  const promise = new Promise((resolve) => {
    let request = null;
    let timer = null;
    let settled = false;
    const done = (value) => {
      if (settled) return;
      settled = true;
      if (timer) clearTimeout(timer);
      if (value === null && request && request.destroy) request.destroy();
      resolve(value);
    };
    const read = (response) => {
      if (response.statusCode !== 200) {
        if (response.resume) response.resume();
        done(null);
        return;
      }
      let body = "";
      if (response.setEncoding) response.setEncoding("utf8");
      response.on("data", (chunk) => {
        body += chunk;
      });
      response.on("error", () => done(null));
      response.on("end", () => {
        try {
          done(String(JSON.parse(body).version || "") || null);
        } catch {
          done(null);
        }
      });
    };
    drop = () => done(null);
    timer = setTimeout(() => done(null), timeoutMs);
    if (timer.unref) timer.unref();
    try {
      request = get(url, { headers: { accept: "application/json" } }, read);
      request.on("error", () => done(null));
    } catch {
      done(null);
    }
  });
  return { promise, cancel: () => drop() };
}

/** The version of the release the registry calls latest, or null. See beginFetch. */
function fetchLatest(options = {}) {
  return beginFetch(options).promise;
}

/**
 * Starts the check for this command line. Returns null when there is nothing to check: the command
 * prints for no one, or this host has the check off. Otherwise it returns a handle whose finish()
 * gives the line to print, or null when the release is not newer. A lookup that answered holds for
 * CHECK_INTERVAL_MS and one that did not for RETRY_INTERVAL_MS, so a network that never answers is
 * asked once an hour, not once a sync. finish() never waits: it uses the answer only if it arrived
 * while polio worked, and otherwise the cache keeps the release from the last one that did.
 */
function start(argv, options = {}) {
  const {
    env = process.env,
    current = require("../package.json").version,
    now = Date.now(),
    get,
  } = options;
  if (!wantsNote(argv)) return null;
  const dir = stateDir(env);
  if (!isEnabled(dir)) return null;

  const cache = readCache(dir);
  if (!cache) {
    // Nothing is newer than a polio that was just installed, so the first run only starts the clock.
    writeCache(dir, { checkedAt: now, triedAt: now, latest: "" });
    return { finish: () => null };
  }

  const stale = now - cache.checkedAt >= CHECK_INTERVAL_MS;
  const rested = now - cache.triedAt >= RETRY_INTERVAL_MS;
  let found = null;
  let cancel = () => {};
  if (stale && rested) {
    writeCache(dir, { ...cache, triedAt: now });
    const attempt = beginFetch({ get, url: manifestUrl(env) });
    cancel = attempt.cancel;
    attempt.promise.then((latest) => {
      if (!latest) return;
      found = latest;
      writeCache(dir, { checkedAt: now, triedAt: now, latest });
    });
  }
  return {
    finish() {
      cancel();
      const latest = found || cache.latest;
      return latest && isNewer(latest, current) ? noteFor(latest) : null;
    },
  };
}

module.exports = {
  CHECK_INTERVAL_MS,
  DEFAULT_REGISTRY,
  MANIFEST_PATH,
  REQUEST_TIMEOUT_MS,
  RETRY_INTERVAL_MS,
  UPGRADE,
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
};
