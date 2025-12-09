# CRAN Readiness Review for `WeatherFunctions.R`

## Critical issues
- **Package load side effects.** Directory creation has been moved behind `weatherfunctions_data_dir()` with an opt-in `.onLoad` hook controlled by `options(weatherfunctions.auto_setup = TRUE)`, which aligns with CRAN guidance against unprompted file-system writes during package attach. Ensure the option remains opt-in and avoid other load-time side effects. 【F:R/WeatherFunctions.R†L29-L88】
- **Unscoped `library()` calls in code.** Multiple `library()` calls appear at the top of the file; in packages these should be declared in `DESCRIPTION`/`NAMESPACE` and functions should use `pkg::fun` or roxygen `@import` statements rather than calling `library()` during load, which CRAN discourages. Consider replacing with namespace-qualified calls and imports. 【F:R/WeatherFunctions.R†L10-L27】

## Suggested fixes
- Wrap directory initialization in a dedicated helper (e.g., `ensure_data_dir()`) that callers invoke explicitly, and avoid performing file system writes during package attachment.
- Replace top-level `library()` statements with roxygen `@import` tags or explicit namespace references to align with CRAN policies and reduce unexpected side effects on attach.
- To keep `DataDir` widely available while staying CRAN-compatible, expose a user-facing initializer (for example, `initialize_weatherfunctions_data_dir()`) that sets up the cache on demand, and gate any automatic setup behind an opt-in option (e.g., `options(weatherfunctions.auto_setup = TRUE)` inside `.Rprofile`) so the package never writes to disk on load without consent. 【F:R/WeatherFunctions.R†L29-L62】【F:R/WeatherFunctions.R†L1-L27】
