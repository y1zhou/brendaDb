context("Query brenda")

test_that("Query specific enzyme ", {
  df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
                               package = "brendaDb"))
  expect_error(QueryBrenda(df, "8.8.8.8"))
  x <- QueryBrenda(df, EC = "1.1.1.10")

  expect_equal(x$nomenclature$ec, "1.1.1.10")
  expect_is(x, "brenda.entry")
  expect_equal(x$nomenclature$ec, "1.1.1.10")
  expect_is(x$nomenclature$protein, "data.table")
  expect_equal(
    x$nomenclature$systematic.name,
    "xylitol:NADP+ 4-oxidoreductase (L-xylulose-forming)"
  )
  expect_equal(x$nomenclature$recommended.name, "L-xylulose reductase")
  expect_is(x$nomenclature$synonyms, "data.table")
})
