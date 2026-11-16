# Legacy historical DWD FST filename prefix

Retained for compatibility with code that constructs year-only filenames
such as `weather_dat_1990.fst`. New code should use
[`dwd_fst_filename()`](https://agronomykiel.github.io/Weatherfunctions/reference/dwd_fst_filename.md),
which includes the complete start date.

## Usage

``` r
fn_histDWD_data_str
```

## Format

A character scalar containing a filename prefix.

## Deprecated

This prefix is retained only for locating legacy year-only FST files.
