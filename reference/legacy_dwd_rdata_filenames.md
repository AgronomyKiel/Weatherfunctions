# Legacy DWD RData filenames

These exported constants are retained for compatibility with workflows
from an earlier development stage that stored complete downloads as
RData files. New code should use
[`dwd_fst_filename()`](https://agronomykiel.github.io/Weatherfunctions/reference/dwd_fst_filename.md)
for the current FST-based workflow.

## Usage

``` r
HistoricalDWD

HistoricalDWDRain

fn_HistoricalDWD

fn_HistoricalDWDRain
```

## Format

A character scalar containing a legacy RData filename.

An object of class `character` of length 1.

An object of class `character` of length 1.

An object of class `character` of length 1.

An object of class `character` of length 1.

## Deprecated

These RData filename constants are deprecated and must not be repurposed
as FST filenames. They remain exported only for backward compatibility.
