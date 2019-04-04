context("Query brenda")

df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
                             package = "brendaDb"))

test_that("Query specific enzyme ", {
  expect_error(QueryBrenda(df, "8.8.8.8"))
  x <- QueryBrenda(df, EC = "1.1.1.1")

  expect_equal(x$nomenclature$ec, "1.1.1.1")
  expect_is(x, "brenda.entry")
  # None of the elements should be NA
  expect_true(all(unlist(lapply(x, function(x) !is.na(x)))))
})
