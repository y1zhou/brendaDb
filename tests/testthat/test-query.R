context("Query brenda")

df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
                             package = "brendaDb"))

test_that("Query enzymes", {
  expect_message(QueryBrenda(df, "8.8.8.8"), "^Invalid.*8.8.8.8")
  x <- QueryBrenda(df, EC = c("1.1.1.1", "6.3.5.8"), n.core = 2)

  expect_equal(x[[1]]$nomenclature$ec, "1.1.1.1")
  expect_true(all(sapply(x, is.brenda.entry)))
  # None of the elements should be NA
  expect_true(all(unlist(lapply(x, function(x) !is.na(x)))))
})
