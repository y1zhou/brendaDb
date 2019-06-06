context("Brenda entry object")

test_that("Creating brenda entry object ", {
  expect_error(InitBrendaEntry())
  x <- InitBrendaEntry(EC = "1.1.1.1")
  expect_equal(names(x), c("nomenclature", "interactions", "parameters",
                           "organism", "molecular", "structure", "bibliography"))
  expect_equal(x$nomenclature$ec, "1.1.1.1")
  expect_equal(is.brenda.entry(x), TRUE)
  expect_equal(is.brenda.deprecated.entry(x), FALSE)

  x <- InitBrendaDeprecatedEntry("6.3.5.8", "transferred to EC 2.6.1.85.")
  expect_equal(names(x), c("nomenclature", "msg"))
  expect_equal(is.brenda.deprecated.entry(x), TRUE)
})
