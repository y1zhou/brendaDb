context("Parse bibliography")

test_that("Parse references", {
  x <- ParseReference(
    paste0(
      "RF\t<1> Talbot, B.G.; Thirion, J.P.: Purification\n\t",
      "and properties of two distinct groups of ADH isozymes from Chinese\n\t",
      "hamster liver. Biochem. Genet. (1981) 19, 813-829. {Pubmed:6794566}\n",
      "RF\t<12> Woronick, C.L.: Alcohol dehydrogenase human liver. Methods\n\t",
      "Enzymol. (1975) 41B, 369-374. {Pubmed:236461} (c,review)\n",
      "RF\t<10> Herrera, E.; Zorzano, A... {Pubmed:} (c,review)\n"
    )
  )
  expect_is(x, "tbl_df")
  expect_equal(dim(x), c(3, 3))
  expect_equal(x$refID, list("1", "12", "10"))
  expect_true(is.na(x[3, "pubmed"]))
})
