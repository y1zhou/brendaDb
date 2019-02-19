context("Parse nomenclature")

test_that("Parse Protein field", {
  x <- ParseProtein(paste0(
    "PR\t#1# Cavia porcellus   (#1# SULT1A2 <1,2,6,7>) <1,2,6,7>\n",
    "PR\t#2# Mus musculus   <11,18,19>\n",
    "PR\t#3# Homo sapiens   <11,12,18,20,22>\n"))
  expect_is(x, "data.table")
  expect_equal(dim(x), c(3, 3))
  expect_equal(length(x$reference[[1]]), 4)
  expect_match(x$organism, "^[^[:space:]].*[^[:space:]]$")
})

test_that("Parse recommended name", {
  expect_equal(ParseRecommendedName("RN	D-arabinose 1-dehydrogenase (NAD+)"),
               "D-arabinose 1-dehydrogenase (NAD+)")
  expect_error(ParseRecommendedName("XY	D-arabinose 1-dehydrogenase (NAD+)"))
})

test_that("Parse systematic name", {
  expect_equal(ParseSystematicName("SN\talcohol:NAD+ oxidoreductase"),
               "alcohol:NAD+ oxidoreductase")
  expect_error(ParseSystematicName("XY\talcohol:NAD+ oxidoreductase"))
})

test_that("Parse synonyms", {
  x <- ParseSynonyms(paste0(
    "SY\t aldehyde reductase\nSY\t dehydrogenase, alcohol\n",
    "SY\t#8,10,95,97,112,113,135# ADH1 (#10# isozyme <202>)\n",
    "\t<156,172,202,215,228,252,282>\n"))
  expect_is(x, "data.table")
  expect_equal(dim(x), c(3, 3))
  expect_equal(length(x$reference[[3]]), 7)
  expect_match(x$synonym, "^[^[:space:]].*[^[:space:]]$")
})
