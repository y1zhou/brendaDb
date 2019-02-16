context("Parse entry descriptions")

test_that("Parse protein number ", {
  expect_equal(ParseProteinNum("#1,2,3#", type = "protein"), c("1", "2", "3"))
  expect_identical(ParseProteinNum("<123>", type = "reference"), "123")
  expect_error(ParseProteinNum("#1,2#"))
  expect_error(ParseProteinNum("#1,2#", type = "randomtype"))
  expect_error(ParseProteinNum("#1, 2#", type = "protein"))
  expect_error(ParseProteinNum("#1,#", type = "protein"))
  expect_error(ParseProteinNum("#1,<#", type = "reference"))
})

test_that("Parse Protein field", {
  x <- ParseProtein(paste0(
    "PR\t#1# Cavia porcellus   (#1# SULT1A2 <1,2,6,7>) <1,2,6,7>\n",
    "PR\t#2# Mus musculus   <11,18,19>\n",
    "PR\t#3# Homo sapiens   <11,12,18,20,22>\n"))
  expect_equal(length(x), 3)
  expect_equal(length(x[["1"]]$reference), 4)
})