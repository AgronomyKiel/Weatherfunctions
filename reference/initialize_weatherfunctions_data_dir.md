# Explicitly initialize the Weatherfunctions data directory.

This is a user-facing helper to establish the cache directory in a way
that avoids performing file system writes during package load. Users can
call it directly (e.g., from their \`.Rprofile\`) or enable automatic
setup via the \`weatherfunctions.auto_setup\` option before attaching
the package.

## Usage

``` r
initialize_weatherfunctions_data_dir(ask = interactive())
```

## Arguments

- ask:

  Logical; when interactive and creation is requested, ask before
  writing to the file system.
