context("Show information about BRENDA")

test_that("Show BRENDA fields and acronyms ", {
  expect_error(ShowFields())
  brenda_txt <- system.file("extdata", "brenda_download_test.txt",
                            package = "brendaDb")
  df <- ReadBrenda(brenda_txt)
  df <- ShowFields(df)

  expect_match(unique(df$field), "^[A-Z_05]+$")
  expect_match(unique(df$acronym), "^[A-Z05]+$")
})
