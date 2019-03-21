context("Brenda entry object")

test_that("Creating brenda entry object ", {
  expect_error(InitBrendaEntry())
  x <- InitBrendaEntry(EC = "1.1.1.1")
  expect_equal(names(x), c("nomenclature", "interactions", "parameters",
                           "organism", "molecular", "structure", "bibliography"))
  expect_equal(x$nomenclature$ec, "1.1.1.1")
  expect_is(x, "brenda.entry")
  expect_equal(is.brenda.entry(x), TRUE)
})
