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

test_that("Parse SYSTEMATIC_NAME", {
  expect_equal(
    ParseSystematicName("SN\talcohol:NAD+ oxidoreductase"),
    "alcohol:NAD+ oxidoreductase"
  )
  expect_warning(ParseSystematicName("XY\talcohol:NAD+ oxidoreductase"))
})

test_that("Parse SYNONYMS", {
  x <- ParseGeneric(
    paste0(
      "SY\t aldehyde reductase\n",
      "SY\t dehydrogenase, alcohol\n",
      "SY\t#8,10,95,97,112\n\t113,135# ADH1 (#10# isozyme <202>)\n",
      "\t<156,172,202,215,228,\n\t252,282>\n"
    ), acronym = "SY")
  expect_is(x, "tbl_df")
  expect_equal(dim(x), c(3, 5))
  expect_equal(x$refID[[3]], "156,172,202,215,228,252,282")
  expect_equal(x$proteinID[[3]], "8,10,95,97,112,113,135")
  expect_match(x$description, "^[^[:space:]].*[^[:space:]]$")

  # Expect to work for a single entry
  expect_equal(dim(
    ParseGeneric("SY\t#8,10,95,97,112,113,135# ADH1 (#10# isozyme <202>)\n",
                 acronym = "SY")
  ), c(1, 5))
})

test_that("Parse REACTION", {
  x <- ParseGeneric(paste0(
    "RE\ta (3R)-3-hydroxyacyl-[acyl-carrier protein] + NADP+ = a\n\t",
    "3-oxoacyl-[acyl-carrier protein] + NADPH + H+ (#16# belongs to family\n\t",
    "of short-chain alcohol dehydrogenases (SDR) with catalytic triad\n\t",
    "Ser154, Tyr167 and Lys171, catalytic mechanism <16>; #42# ordered bi bi\n\t",
    "kinetic mechanism, [acyl-carrier protein] recognition mechanism <23>)\n"
  ), "RE")
  expect_is(x, "tbl_df")
  expect_equal(dim(x), c(1, 5))
  expect_true(all(is.na(c(x$proteinID, x$fieldInfo, x$refID))))
})

test_that("Parse REACTION_TYPE", {
  x <- ParseGeneric("RT\tredox reaction\nRT\toxidation\nRT\treduction\n", "RT")
  expect_equal(dim(x), c(3, 5))
  expect_true(all(is.na(c(x$proteinID, x$fieldInfo, x$commentary, x$refID))))
})
