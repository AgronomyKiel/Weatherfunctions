# Return the Weatherfunctions data directory path and optionally create it.

The directory is not created automatically on package load to avoid CRAN
side effects. Users can call this helper explicitly or opt into
automatic creation by setting \`options(weatherfunctions.auto_setup =
TRUE)\` before loading the package.

## Usage

``` r
weatherfunctions_data_dir(create = FALSE, ask = interactive())
```

## Arguments

- create:

  Logical; create the directory if it does not exist.

- ask:

  Logical; when interactive and creation is requested, ask before
  writing to the file system.

## Value

A character string with the path used for local cache files.
