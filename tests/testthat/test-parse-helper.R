context("Parse helper")

test_that("Parse protein number ", {
  expect_equal(ParseProteinNum(NA), NA)
  expect_equal(ParseProteinNum("#1,2,3#", type = "protein"), c("1,2,3"))
  expect_identical(ParseProteinNum("<123>", type = "reference"), "123")
  expect_error(ParseProteinNum("#1,2#"))
  expect_error(ParseProteinNum("#1,2#", type = "randomtype"))
  expect_equal(ParseProteinNum("#1,,2#", type = "protein"), "1,2")
  expect_equal(ParseProteinNum("#1, 2#", type = "protein"), "1,2")
  expect_error(ParseProteinNum("#1,#", type = "protein"))
  expect_error(ParseProteinNum("#1,<#", type = "reference"))
})

test_that("Separate subentries", {
  expect_message(SeparateSubentries(NA))
  expect_true(is.na(SeparateSubentries(NA, "any_acronym")))
  expect_warning(SeparateSubentries("SN\talcohol oxidoreductase", "SS"))
  expect_equal(SeparateSubentries("SN\txx\n\tyyy", "SN"), "xx yyy")
  expect_equal(SeparateSubentries("SN\txx\n\tyyy\nSN\tzzz", "SN"),
               c("xx yyy", "zzz"))
})
