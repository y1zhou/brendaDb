context("Parse nomenclature")

test_that("Parse PROTEIN", {
  expect_true(is.na(ParseProtein(NA)))
  x <- ParseProtein(
    paste0(
      "PR\t#1# Cavia porcellus   (#1# SULT1A2 <1,2,6,7>) <1,2,6,7>\n",
      "PR\t#2# Mus musculus   <11,18,\n\t19>\n",
      "PR\t#3# Homo sapiens   <11,12,18,20,22>\n",
      "PR\t#120# Aspergillus terreus A0A068FNL1 <136>\n",
      "PR\t#121# Actinomyces sp. oral taxon 448 F9ECN6 UniProt <121>\n",
      "PR\t#42# Suberites domuncula O97342 SwissProt <64>\n"
    )
  )
  expect_is(x, "tbl_df")
  expect_equal(dim(x), c(6, 5))
  expect_equal(x$refID[[1]], "1,2,6,7")
  expect_equal(sum(is.na(x$uniprot)), 3)
  expect_equal(sum(is.na(x$commentary)), 5)
})

test_that("Parse RECOMMENDED_NAME", {
  expect_equal(
    ParseRecommendedName("RN\tD-arabinose 1-dehydrogenase (NAD+)"),
    "D-arabinose 1-dehydrogenase (NAD+)"
  )
  expect_warning(ParseRecommendedName("XY\tD-arabinose 1-dehydrogenase (NAD+)"))
})
