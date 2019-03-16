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
  expect_is(x, "data.table")
  expect_equal(dim(x), c(3, 3))
  expect_equal(x$refID, list("1", "12", "10"))
  expect_true(is.na(x[3, "pubmed"]))
})

test_that("Parse application", {
  x <- ParseGeneric(
    paste0(
      "AP\t#13# biotechnology (#13# possible usage in bioindustrial\n\t",
      "processes and as biosensor <126>) <126>\n",
      "AP\t#6,10,13,15,28,29,34,36,39,41,43,88,92,118,123,139,140,143,144,\n\t",
      "151,152# synthesis (#43# enzyme can be used in preparative... <1>)",
      "<114,131,132,133,134,137,169,179,187,219,232,234,240,242,245,246,248\n\t",
      "250,251,253,254,261,262,263,264,266,269,272>"
  ), acronym = "AP")
  expect_equal(dim(x), c(2, 5))
  expect_equal(length(x$proteinID[[2]]), 21)
  expect_true(all(is.na(x$fieldInfo)))
  expect_match(x$commentary, "^#.*?>$")
})
