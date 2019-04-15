context("Parse molecular")

test_that("Parse GENERAL_STABILITY", {
  expect_true(is.na(ParseNoDescription(NA)))
  x <- ParseNoDescription(
    paste0(
      "GS\t#1# (the ChOx-immobilized silk mat maintains its initial activity till\n\t",
      "the fourth successive measurement and retains about 50% of its initial\n\t",
      "activity at the end of 25 measurements) <75>\n",
      "GS\t#12# (isoforms OxO1-4 are resistant to a treatment with pepsin at 37°C\n\t",
      "for 30 min at pH 2.0, while the activity of isoform OxO4 is actually\n\t",
      "increased on pepsin treatment) <59>\n"
    ), acronym = "GS"
  )
  expect_equal(dim(x), c(2, 3))
})
