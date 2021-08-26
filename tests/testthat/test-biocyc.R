context("Retrieve information from BioCyc")

test_that("Get enzymes in BioCyc pathway ", {
  expect_error(BiocycPathwayEnzymes())
  expect_error(BiocycPathwayEnzymes(org.id = "HUMAN", pathway = NA))
  expect_message(BiocycPathwayEnzymes(org.id = "HUMAN", pathway = "PWY66666"))
  expect_message(
    {x <- BiocycPathwayEnzymes(org.id = "HUMAN", pathway = "PWY66-400", sleep = 1)},
    regexp = "Found 10.+HUMAN.+PWY66-400"
  )
  expect_equal(dim(x), c(11, 5))
  expect_match(x$EC, "^(\\d+\\.){3}\\d+$")
})

test_that("Get genes in BioCyc pathway ", {
  expect_error(BiocycPathwayGenes())
  expect_error(BiocycPathwayGenes(org.id = "HUMAN", pathway = NA))
  expect_message(BiocycPathwayGenes(org.id = "HUMAN", pathway = "PWY66666"))
  expect_message(
    {x <- BiocycPathwayGenes(org.id = "HUMAN", pathway = "PWY66-400")},
    regexp = "Found 25.+HUMAN.+PWY66-400"
  )
  expect_equal(dim(x), c(25, 4))
  expect_match(x$Ensembl, "^ENSG\\d{11}")
})
