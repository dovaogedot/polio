package polio

/** The update setting: it starts on, `polio updates` reports it, a change is remembered, junk is refused. */
object UpdateSuite extends SandboxSuite {

  sandboxed("the check is on until it is turned off, and the setting is remembered") { sb =>
    for
      on      <- sb.polio("updates")
      off     <- sb.polio("updates", "off")
      setting <- sb.polio("updates")
      back    <- sb.polio("updates", "on")
    yield
      on.has("update check: on")
        && off.has("update check: off")
        && setting.has("update check: off")
        && back.has("update check: on")
  }

  sandboxed("an unknown setting is refused") { sb =>
    sb.tryPolio("updates", "maybe").map: run =>
      check(run.code != 0, "an unknown setting was accepted")
        && run.err.has("unknown setting: maybe")
  }
}
