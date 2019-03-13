context("Parse parameters")

test_that("Parse Km value", {
  x <- ParseKmValue(paste0(
    "KM\t#1# -999 {more}  (#1# in phosphate buffer, enzyme shows\n\t",
    "marked cooperativity with respect to NAD+ binding. <15>) <15>\n",
    "KM\t#1# 0.045 {GDP-D-mannose} (#1# pH 8.0, 5°C, recombinant mutant C268A\n\t",
    "<17>) <17>\n",
    "KM\t#1# 0.1 {NAD+(test)}  (#1# pH 8.0, phosphate buffer <15>) <15>\n"
    ))
  expect_equal(dim(x), c(3, 5))
  expect_equal(x$description[1], "additional_information")
  expect_equal(x$fieldInfo[3], "NAD+(test)")
})

test_that("Parse pH optimum", {
  x <- ParsePhOptimum(
    paste0(
      "PHO\t#10# 8.3 (#10# alcohol dehydrogenase IV <87>) <87>\n",
      "PHO\t#9,68,78,132# -999 (#78# oxidation of ethanol, pyrazole-insensitive\n\t",
      "enzyme <24>; #68# ethanol oxidation, enzyme form ADH-2 and ADH-3 <60>;\n\t",
      "#9# oxidation of octanol <49>; #132# optimally active with ethanol and\n\t",
      "1-propanol at pH 11.0 with 3 M KCl <237>) <24,49,60,237>\n"
    )
  )
  expect_is(x, "data.table")
  expect_equal(dim(x), c(2, 5))
  expect_equal(length(x$refID[[2]]), 4)
  expect_true(all(is.na(x$fieldInfo)))
  expect_equal(x$description[[2]], "additional_information")
})

test_that("Parse pH range", {
  x <- ParsePhRange(
    paste0(
      "PHR\t#10# 5-9 <196>\n",
      "PHR\t#10# 8.2-9.5 (#10# pH 8.2: about 10% of maximal activity, pH 9.5: about\n\t",
      "40% of maximal activity <87>) <87>\n",
      "PHR\t#10,113,114# 6-9 <122,215>\n",
      "PHR\t#149# 6 (#149# 50% of maximum activity for reduction of aldehydes\n\t",
      "<243>) <243>\n",
      "PHR\t#51# -999 (#51# pH profiles of the ATP-cleavage reaction in the\n\t",
      "presence and absence of free biotin <90>) <90>\n"
    )
  )
  expect_is(x, "data.table")
  expect_equal(dim(x), c(5, 5))
  expect_equal(length(x$refID[[3]]), 2)
  expect_true(all(is.na(x$fieldInfo)))
  expect_equal(x$description[[5]], "additional_information")
})
