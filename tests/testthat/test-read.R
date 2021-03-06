context("Read BRENDA txt")

test_that("Read BRENDA txt file correctly into matrix ", {
  brenda_txt <- system.file("extdata", "brenda_download_test.txt",
                            package = "brendaDb")
  df <- ReadBrenda(brenda_txt, clean = TRUE)
  expect_error(ReadBrenda("fakefile.txt"))
  expect_is(df, "tbl_df")
  expect_equal(colnames(df), c("ID", "field", "description"))

  # First column should be EC numbers
  expect_match(unique(df$ID), "^(\\d+\\.){3}\\d+$")
  # Second column should be field full names
  expect_match(unique(df$field), "^[A-Z_05]+$")
})
