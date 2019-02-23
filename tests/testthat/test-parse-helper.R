context("Parse helper")

test_that("Parse protein number ", {
  expect_equal(ParseProteinNum(NA), NA)
  expect_equal(ParseProteinNum("#1,2,3#", type = "protein"), c("1", "2", "3"))
  expect_identical(ParseProteinNum("<123>", type = "reference"), "123")
  expect_error(ParseProteinNum("#1,2#"))
  expect_error(ParseProteinNum("#1,2#", type = "randomtype"))
  expect_error(ParseProteinNum("#1, 2#", type = "protein"))
  expect_error(ParseProteinNum("#1,#", type = "protein"))
  expect_error(ParseProteinNum("#1,<#", type = "reference"))
})

test_that("Separate subentries", {
  expect_error(SeparateSubentries("SN\talcohol oxidoreductase", "SS"))
  expect_equal(SeparateSubentries("SN\txx\n\tyyy", "SN"), "xx yyy")
  expect_equal(SeparateSubentries("SN\txx\n\tyyy\nSN\tzzz", "SN"),
               c("xx yyy", "zzz"))
})

test_that("Parse commentaries", {
  expect_error(ParseCommentary())
  expect_error(ParseCommentary(c("1", "2")))
  expect_equal(ParseCommentary("Cavia porcellus)"), NA)

  x <- ParseCommentary("Cavia porcellus   (#1# SULT1A2(1) <1,2,6,7>)")
  expect_equal(dim(x), c(1, 3))
  expect_equal(x$commentary[[1]], "SULT1A2(1)")
  expect_equal(length(x$references[[1]]), 4)

  x <- ParseCommentary("E coli (#1,2# SULT1A2(1)\n\tSALT <1,2,\n\t6,7>)")
  expect_equal(dim(x), c(1, 3))
  expect_equal(length(x$id[[1]]), 2)
  expect_equal(x$commentary[[1]], "SULT1A2(1) SALT")
  expect_equal(length(x$references[[1]]), 4)
})
