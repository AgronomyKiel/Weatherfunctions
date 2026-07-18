test_that("DWD FST filenames include the complete start date", {
  expect_identical(
    dwd_fst_filename("historical", "1990-01-01"),
    "weather_dat_1990-01-01.fst"
  )
  expect_identical(
    dwd_fst_filename("recent", as.Date("2025-07-01")),
    "recent_weather_dat_2025-07-01.fst"
  )
})

test_that("different dates in one year produce different filenames", {
  expect_false(identical(
    dwd_fst_filename("historical", "1990-01-01"),
    dwd_fst_filename("historical", "1990-07-01")
  ))
})

test_that("invalid filename inputs fail clearly", {
  expect_error(
    dwd_fst_filename("forecast", "1990-01-01"),
    "should be one of"
  )
  expect_error(
    dwd_fst_filename("historical", c("1990-01-01", "1991-01-01")),
    "exactly one date"
  )
  expect_error(
    dwd_fst_filename("historical", "not-a-date"),
    "valid Date"
  )
})
