test_that("daily KL product files support regular and klima_zug names", {
  expect_identical(
    find_dwd_product_file(
      c("Metadaten.txt", "produkt_klima_tag_20250101_20250930_00044.txt"),
      archive = "tageswerte_KL_00044_akt.zip",
      product = "KL"
    ),
    "produkt_klima_tag_20250101_20250930_00044.txt"
  )

  expect_identical(
    find_dwd_product_file(
      c("Metadaten.txt", "produkt_klima_zug_tag_20250329_20260929_20278.txt"),
      archive = "tageswerte_KL_20278_akt.zip",
      product = "KL"
    ),
    "produkt_klima_zug_tag_20250329_20260929_20278.txt"
  )
})

test_that("missing and ambiguous product data fail clearly", {
  expect_error(
    find_dwd_product_file(
      "Metadaten.txt",
      archive = "tageswerte_KL_20278_akt.zip",
      product = "KL"
    ),
    "found 0"
  )

  expect_error(
    find_dwd_product_file(
      c(
        "produkt_klima_tag_first.txt",
        "produkt_klima_zug_tag_second.txt"
      ),
      archive = "duplicate.zip",
      product = "KL"
    ),
    "found 2"
  )
})

test_that("precipitation product filenames remain supported", {
  expect_identical(
    find_dwd_product_file(
      "produkt_nieder_tag_20250101_20250930_00044.txt",
      archive = "tageswerte_RR_00044_akt.zip",
      product = "RR"
    ),
    "produkt_nieder_tag_20250101_20250930_00044.txt"
  )
})
