#' @title Parse a "PROTEIN" entry.
#'
#' @description Expand the string into a `tibble`.
#'
#' @param description The description string in a "PROTEIN" entry.
#'
#' @return A `tibble` with five columns: proteinID, description, uniprot,
#' commentary and reference. The description column is the source organism.
#'
#' @examples
#' x <- paste0(
#' "PR\t#1# Cavia porcellus  (#1# SULT1A2 <1,2,6,7>) <1,2,6,7>\n",
#' "PR\t#2# Mus musculus <11,18,19>\n")
#' brendaDb:::ParseProtein(x)
#'
#' @importFrom magrittr %>%
#' @import stringr
#' @importFrom tibble as_tibble
#' @importFrom dplyr mutate select
ParseProtein <- function(description) {
  res <- ParseGeneric(description, acronym = "PR") %>%
    as_tibble() %>%
    mutate(
      uniprot = str_extract(
        description,
        "[OPQ][0-9][A-Z0-9]{3}[0-9]|[A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2}"
      )) %>%  # regex taken from https://www.uniprot.org/help/accession_numbers
    select(proteinID, description, uniprot, commentary, refID)
  return(res)
}


#' @title Parse a "RECOMMENDED_NAME" entry into a string.
#'
#' @description Remove useless characters in the description.
#'
#' @param description The description string in a "RECOMMENDED_NAME" entry.
#'
#' @return A string to fill into the `recommended.name` field in
#' `brenda.nomenclature`.
#'
#' @examples
#' x <- "RN	D-arabinose 1-dehydrogenase (NAD+)"
#' brendaDb:::ParseRecommendedName(x)
ParseRecommendedName <- function(description) {
  x <- SeparateSubentries(description, acronym = "RN")
  return(x)
}


#' @title Parse a "SYSTEMATIC_NAME" entry into a string.
#'
#' @description Remove useless characters in the description.
#'
#' @param description The description string in a "SYSTEMATIC_NAME" entry.
#'
#' @return A string to fill into the `systematic.name` field in
#' `brenda.nomenclature`.
#'
#' @examples
#' x <- "SN\talcohol:NAD+ oxidoreductase"
#' brendaDb:::ParseSystematicName(x)
ParseSystematicName <- function(description) {
  x <- SeparateSubentries(description, acronym = "SN")
  return(x)
}
