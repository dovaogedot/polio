#!/usr/bin/env node
// Runs the native polio binary for this machine. The binary lives in the optional dependency
// polio-<platform>-<arch>; npm installs only the one that matches the machine.
"use strict";

const { spawn } = require("node:child_process");
const update = require("./update.js");

const pkg = `polio-${process.platform}-${process.arch}`;
let bin;
try {
  bin = require.resolve(`${pkg}/bin/polio`);
} catch {
  console.error(`polio: no binary for ${process.platform}-${process.arch} (package ${pkg} is not installed)`);
  process.exit(1);
}

const argv = process.argv.slice(2);
const check = update.start(argv);
const run = spawn(bin, argv, { stdio: "inherit" });

run.on("error", (error) => {
  console.error(`polio: ${error.message}`);
  process.exit(1);
});

// The note goes out after polio has printed, and the exit code is set rather than forced, so the
// last line is on its way out before the process ends.
run.on("exit", (code, signal) => {
  const note = check && check.finish();
  if (note) console.log(note);
  if (signal) {
    process.kill(process.pid, signal);
    return;
  }
  process.exitCode = code === null ? 1 : code;
});
