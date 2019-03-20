context("Query brenda")

df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
                             package = "brendaDb"))

test_that("Query specific enzyme ", {
  expect_error(QueryBrenda(df, "8.8.8.8"))
  x <- QueryBrenda(df, EC = "1.1.1.1")

  expect_equal(x$nomenclature$ec, "1.1.1.1")
  expect_is(x, "brenda.entry")
  expect_is(x$nomenclature$protein, "tbl_df")
  expect_equal(
    x$nomenclature$systematic.name,
    "alcohol:NAD+ oxidoreductase"
  )
  expect_equal(x$nomenclature$recommended.name, "alcohol dehydrogenase")
  expect_is(x$nomenclature$synonyms, "tbl_df")
})
