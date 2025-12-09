# CRAN Readiness Review for WeatherFunctions.R

## Critical issues

- **Package load side effects.** `WeatherFunctions.R` creates
  directories under
  [`tools::R_user_dir()`](https://rdrr.io/r/tools/userdir.html) during
  package load via top-level code, which can cause CRAN checks to fail
  because packages should not write to the file system at load time.
  Move directory creation inside functions or an explicit setup step
  instead of executing at the top level.
  【F:R/WeatherFunctions.R†L31-L118】
- **Unscoped [`library()`](https://rdrr.io/r/base/library.html) calls in
  code.** Multiple [`library()`](https://rdrr.io/r/base/library.html)
  calls appear at the top of the file; in packages these should be
  declared in `DESCRIPTION`/`NAMESPACE` and functions should use
  `pkg::fun` or roxygen `@import` statements rather than calling
  [`library()`](https://rdrr.io/r/base/library.html) during load, which
  CRAN discourages. Consider replacing with namespace-qualified calls
  and imports. 【F:R/WeatherFunctions.R†L10-L27】

## Suggested fixes

- Wrap directory initialization in a dedicated helper (e.g.,
  `ensure_data_dir()`) that callers invoke explicitly, and avoid
  performing file system writes during package attachment.
- Replace top-level [`library()`](https://rdrr.io/r/base/library.html)
  statements with roxygen `@import` tags or explicit namespace
  references to align with CRAN policies and reduce unexpected side
  effects on attach.
