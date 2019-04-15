context("Parse interactions")

test_that("Parse SUBSTRATE_PRODUCT", {
  # NATURAL_SUBSTRATE_PRODUCT is very similar
  expect_true(is.na(ParseReaction(NA)))
  x <- ParseReaction(
    paste0(
      "SP\t#10# n-propanol + NAD+ = n-propanal + NADH {r} <120>\n",
      "SP\t#10# ethylenglycol + NAD+ = ? + NADH {r} <120>\n",
      "SP\t#10,25,92,104,112# (R)-2-butanol + NAD+ = 2-butanone + NADH + H+ (#92#\n\t",
      "50% of the activity with 2-propanol <137>; #25# 55% of activity with\n\t",
      "N-benzyl-3-pyrrolidinol <188>) {ir} <137,188,202,207>\n",
      "SP\t#1,2,3,4,5,6,7# 4-carboxybut-2-enoyl-CoA = but-2-enoyl-CoA + CO2 (#7#\n\t",
      "i.e. glutaconyl-CoA <10>) |#7# i.e. crotonyl-CoA <10>|\n\t",
      "<1,2,3,4,5,6,7,10>\n"
    ), acronym = "SP"
  )
  expect_equal(dim(x), c(4, 7))
  expect_equal(x$substrate[3], "(R)-2-butanol + NAD+")
  expect_equal(sum(is.na(x$commentarySubstrate)), 2)
  expect_equal(sum(is.na(x$commentaryProduct)), 3)
  expect_equal(x$reversibility, c("r", "r", "ir", NA))
})

test_that("Parse COFACTOR", {
  x <- ParseGeneric("CF\t#36# FMN (#36# covalently bound <73>) <73>\n", "CF")
  expect_equal(dim(x), c(1, 5))
})
