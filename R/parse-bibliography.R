#' @title Parse a "REFERENCE" entry.
#'
#' @description Expand the string into a `tibble`.
#'
#' @param description The description string in a "REFERENCE" entry.
#'
#' @return A `tibble` with three columns: refID, title and pubmed.
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
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
ParseReference <- function(description) {
  x <- SeparateSubentries(description, acronym = "RF")

  # Split reference IDs, titles, and pubmed IDs ------------------------------
  ref.num <- x %>%
    str_extract("^<\\d+>") %>%
    map_chr(function(x) ParseProteinNum(x, type = "reference"))

  pubmed <- x %>%
    str_extract("\\{Pubmed:\\d+\\}") %>%
    map_chr(function(x) str_extract(x, "\\d+"))

  ref.title <-
    str_trim(str_remove_all(x, "(^<\\d+>)|(\\{Pubmed.*$)"))

  res <- tibble(refID = ref.num,
                title = ref.title,
                pubmed = pubmed)
  return(res)
}
