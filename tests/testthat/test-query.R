context("Query brenda")

df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
                             package = "brendaDb"))

test_that("Query enzymes", {
  expect_message(QueryBrenda(df, "8.8.8.8"), "^\\d+ invalid.*8.8.8.8")
  x <- QueryBrenda(df, EC = c("1.1.1.1", "6.3.5.8"), n.core = 2)
  expect_equal(x[[1]]$nomenclature$ec, "1.1.1.1")

  # None of the elements should be NA
  expect_true(all(unlist(lapply(x, function(x) !is.na(x)))))

  # Extra test for is.brenda.entry
  expect_message(is.brenda.entry(x), ".+is.brenda.deprecated")
  expect_equivalent(is.brenda.deprecated.entry(x), c(FALSE, TRUE))
})

test_that("Query single enzyme output", {
  expect_message(QueryBrendaBase(df, "6.3.5.8"), "^6.3.5.8.*deleted.")
})

test_that("Query single enzyme with certain organisms", {
  x <- QueryBrendaBase(df, "1.1.1.1", organisms = "Homo sapiens")
  expect_equal(dim(x$parameters$km.value), c(163, 5))
})

test_that("ID to enzyme conversion", {
  x <- ID2Enzyme(df, c("CD38", "ADH4", "pyruvate dehydrogenase"))
  expect_equal(dim(x), c(4, 5))
  expect_equal(x$ID,c("ADH4", "CD38",
                      "pyruvate dehydrogenase", "pyruvate dehydrogenase"))
  # NA-filled rows
  expect_equal(length(x$RECOMMENDED_NAME[is.na(x$RECOMMENDED_NAME)]), 2)
  expect_equal(length(x$SYNONYMS[is.na(x$SYNONYMS)]), 0)
  expect_equal(length(x$SYSTEMATIC_NAME[is.na(x$SYSTEMATIC_NAME)]), 3)

  # Correctly parsed values
  expect_equal(x$RECOMMENDED_NAME[3], "pyruvate dehydrogenase (NADP+)")
  expect_equal(length(str_split(x$SYNONYMS[2], "\n")[[1]]), 6)
  expect_equal(
    x$SYSTEMATIC_NAME[4],
    "ATP:[pyruvate dehydrogenase (acetyl-transferring)] phosphotransferase"
  )
})
