# >>> uvr >>>
local({
  lib <- file.path(getwd(), ".uvr", "library")
  if (dir.exists(lib)) {
    cli::cli_alert_info("Linking to uvr library at {.path {lib}}")
    .libPaths(lib)
  }
})
# <<< uvr <<<
