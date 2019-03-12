#' @title Parse a "REFERENCE" entry.
#'
#' @description Expand the string into a `data.table`.
#'
#' @param description The description string in a "REFERENCE" entry.
#'
#' @return A `data.table` with three columns: refID, title and pubmed.
#'
#' @examples
#' x <- paste0(
#' "RF\t<1> Talbot, B.G.; Thirion, J.P.: Purification\n\t",
#' "and properties of two distinct groups of ADH isozymes from Chinese\n\t",
#' "hamster liver. Biochem. Genet. (1981) 19, 813-829. {Pubmed:6794566}\n",
#' "RF\t<12> Woronick, C.L.: Alcohol dehydrogenase from human liver. Methods\n\t",
#' "Enzymol. (1975) 41B, 369-374. {Pubmed:236461} (c,review)\n",
#' "RF\t<10> Herrera, E.; Zorzano, A... {Pubmed:} (c,review)\n")
#' brendaDb:::ParseReference(x)
#'
#' @import stringr
#' @importFrom data.table data.table
ParseReference <- function(description) {
  x <- SeparateSubentries(description, acronym = "RF")

  # Split reference IDs, titles, and pubmed IDs ------------------------------
  ref.num <- str_extract(x, "^<\\d+>")
  ref.num <- lapply(ref.num, function(x)
    ParseProteinNum(x, type = "reference"))

  pubmed <- str_extract(x, "\\{Pubmed:\\d+\\}")
  pubmed <- lapply(pubmed, function(x) str_extract(x, "\\d+"))

  ref.title <-
    str_trim(str_remove_all(x, "(^<\\d+>)|(\\{Pubmed.*$)"))

  res <- data.table(refID = ref.num,
                    title = ref.title,
                    pubmed = pubmed)
  return(res)
}


#' @title Parse a "APPLICATION" entry.
#'
#' @description Expand the string into a `data.table`.
#'
#' @param description The description string in a "APPLICATION" entry.
#'
#' @return A `data.table` with columns: proteinID, description, fieldInfo,
#' commentary and refID. The description column is the application, and the
#' fieldInfo column should be all NAs.
#'
#' @examples
#' x <- paste0(
#' "AP\t#13# biotechnology (#13# possible usage in bioindustrial\n\t",
#' "processes and as biosensor <126>) <126>\n",
#' "AP\t#6,10,13,15,28,29,34,36,39,41,43,88,92,118,123,139,140,143,144,\n\t",
#' "151,152# synthesis (#43# enzyme can be used in preparative... <1>)",
#' "<114,131,132,133,134,137,169,179,187,219,232,234,240,242,245,246,248\n\t",
#' "250,251,253,254,261,262,263,264,266,269,272"
#' )
#' brendaDb:::ParseApplication(x)
ParseApplication <- function(description) {
  x <- SeparateSubentries(description, acronym = "AP")
  res <- ParseGeneric(x)
  return(res)
}
