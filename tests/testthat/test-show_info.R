context("Show information about BRENDA")

test_that("Show BRENDA fields and acronyms ", {
  brenda_txt <- system.file("extdata", "brenda_download_test.txt",
                            package = "brendaDb")
  df <- ReadBrenda(brenda_txt)
  df <- ShowFields(df)

  expect_match(unique(df$field), "^[A-Z_]+$")
  expect_match(unique(df$acronym), "^[A-Z]+$")
})