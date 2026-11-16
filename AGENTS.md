# AGENTS.md

## Project Overview

Weatherfunctions is an R package for downloading daily weather and precipitation data from the German Weather Service (DWD), selecting nearby stations, interpolating station observations to locations, and calculating derived variables such as radiation, evapotranspiration, and canopy temperature.

Treat this repository as an R package. Package metadata and dependencies live in `DESCRIPTION`; exports and imports are represented in `NAMESPACE` and are primarily generated from roxygen2 comments in `R/`.

This is an early-stage R package and is not related to RAD Studio, Delphi, or Pascal. Do not use RAD Studio/Kai integration or Pascal-specific workflows in this repository; use standard R-package and filesystem tooling instead.

Some workflows contact DWD or other remote services and can download substantial data. Do not run network-dependent examples or bulk-download functions unless the task requires it and the user has agreed to the network and storage cost.

## Repo Map

- `R/WeatherFunctions.R`: core DWD download, station selection, interpolation, plotting, cache-directory, and forecast functions. This is a large central source file; keep edits narrowly scoped.
- `R/EvapFuncs.R`: evapotranspiration, radiation, aerodynamic resistance, canopy resistance, and related calculations.
- `R/RenderImportFunctions.R`: helpers that render weather import and analysis reports.
- `R/Weatherfunctions-package.R`: package-level roxygen2 import declarations.
- `man/`: generated Rd reference documentation.
- `vignettes/Weatherfunctions.Rmd`: package vignette.
- `README.Rmd`: source for `README.md`; edit the Rmd, not the generated Markdown file.
- `dev/`: development helpers, not installed package code.
- `docs/`, `doc/`, and `Meta/`: generated build/site artifacts and ignored local output.
- `.github/workflows/pkgdown.yaml`: pkgdown site build and deployment workflow.
- `CRAN_review.md`: known CRAN-check observations and review notes.

There is currently no committed `tests/testthat/` suite. Do not imply that automated unit tests exist; add focused tests when introducing logic that can be tested without remote services.

## Build And Verification

Run commands from the repository root. Prefer the narrowest useful verification first.

1. Parse changed R source files:

   ```powershell
   Rscript -e "files <- c('R/WeatherFunctions.R'); invisible(lapply(files, parse)); cat('Parse OK\n')"
   ```

   Adjust `files` to cover every changed R file.

2. Regenerate roxygen2 output after changing exported functions, signatures, documentation, imports, or package-level declarations:

   ```powershell
   Rscript -e "roxygen2::roxygenise()"
   ```

3. Run a package check when dependencies are installed:

   ```powershell
   Rscript -e "devtools::check(document = FALSE)"
   ```

   An equivalent base workflow is `R CMD build .` followed by `R CMD check <tarball>`. Keep generated tarballs and check directories out of commits.

4. If `README.Rmd` changes, regenerate its derived file:

   ```powershell
   Rscript -e "rmarkdown::render('README.Rmd', output_format = 'github_document')"
   ```

5. If the vignette or pkgdown configuration changes, verify the relevant rendered output only after the package-level checks pass. The CI workflow builds the pkgdown site.

Package checks or examples may reach remote services, create user cache data, take considerable time, or fail when offline. Distinguish code failures from unavailable network/data. Never substitute live DWD downloads for small local fixtures in routine tests.

## Working Rules

- Preserve the existing public API unless the requested change explicitly calls for a breaking change.
- Keep DWD field names, station identifiers, date conventions, units, and historical/recent repository distinctions intact. Document any conversion at the point where it occurs.
- Prefer explicit namespace calls such as `dplyr::filter()` in new code where package conflicts are plausible. If imports change, update roxygen2 declarations and regenerate `NAMESPACE`.
- Match the surrounding style in legacy code; avoid unrelated whole-file formatting in the large source files.
- Use `TRUE` and `FALSE` in new code rather than `T` and `F`, and use clear argument names for non-obvious calls.
- Do not add package-load or attach-time writes, prompts, or downloads. Cache creation must remain explicit or opt-in through the existing data-directory helpers.
- Treat remote directory layouts and filenames as external inputs: validate assumptions, handle missing data, and keep diagnostics useful.
- Do not silently change default cache locations or write downloaded data inside the repository.
- Keep changes focused. Do not update generated documentation or the website unless their source changed.

## Generated And High-Risk Files

- `.RData` and `.Rhistory` are large/local workspace state. Never inspect, modify, regenerate, or commit them as part of normal package work.
- `LocalCopyDWD/`, `DWD_tmp/`, downloaded ZIP files, FST datasets, and user cache directories may contain large external datasets. Do not commit them.
- `man/*.Rd` and `NAMESPACE` are generated by roxygen2. Change the roxygen comments in `R/`, then regenerate these files.
- `README.md` is generated from `README.Rmd`.
- `docs/`, `doc/`, `Meta/`, `inst/doc/`, package tarballs, and `*.Rcheck/` directories are build products unless a task explicitly targets published artifacts.
- DWD download/update functions and forecast retrieval are network-dependent and may perform bulk writes. Use them only when explicitly needed.
- Functions that write HUME files or initialize cache directories have filesystem side effects. Test them with temporary paths, never with a user's real data directory.

## Documentation Workflow

- For a function change, update its adjacent roxygen2 block, including parameters, return value, units, side effects, and a runnable example where practical.
- Keep examples deterministic and lightweight. Wrap examples requiring network access, large downloads, or user-specific files so they are not run during routine checks, or replace them with local examples.
- Run `roxygen2::roxygenise()` and review resulting changes to `man/` and `NAMESPACE`; do not hand-edit generated Rd files.
- Edit `README.Rmd` before regenerating `README.md` and keep both changes together.
- Update the vignette when a user-facing workflow changes materially.

## Change Notes

In the final handoff, report:

- the user-visible behavior changed;
- source and generated files changed;
- verification commands run and their results;
- checks not run, especially those skipped because dependencies, network access, or DWD data were unavailable;
- any compatibility, cache, storage, or data-format implications.
