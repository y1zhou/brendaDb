#' @title Parse a "PROTEIN" entry.
#'
#' @description Expand the string into a `tibble`.
#'
#' @param description The description string in a "PROTEIN" entry.
#'
#' @return A `tibble` with five columns: proteinID, description, uniprot,
#' commentary and reference. The description column is the source organism.
#' @keywords internal
#'
#' @examples
#' x <- paste0(
#' "PR\t#1# Cavia porcellus  (#1# SULT1A2 <1,2,6,7>) <1,2,6,7>\n",
#' "PR\t#2# Mus musculus <11,18,19>\n")
#' brendaDb:::ParseProtein(x)
#'
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom tibble as_tibble is_tibble
#' @importFrom dplyr mutate select distinct
#' @importFrom rlang .data
ParseProtein <- function(description) {
  # regex taken from https://www.uniprot.org/help/accession_numbers
  uniprot.regex <- regex(
    "([OPQ][0-9][A-Z0-9]{3}[0-9]|
    [A-NR-Z][0-9]([A-Z][A-Z0-9]{2}[0-9]){1,2})|  # official accession numbers
    \\w+\\s([uU]ni|[sS]wiss)[pP]rot  # non-canonical IDs",
    comments = TRUE)
  res <- ParseGeneric(description, acronym = "PR")
  if (is_tibble(res)) {
    res <- res %>%
      mutate(
        uniprot = str_extract(.data$description, uniprot.regex)
      ) %>%
      mutate(
        description = str_remove(.data$description, uniprot.regex),
        description = str_trim(str_remove(.data$description,
                                          "([uU]ni|[sS]wiss)[pP]rot")),
        uniprot = toupper(str_remove(.data$uniprot,
                                     "\\s+([uU]ni|[sS]wiss)[pP]rot"))
      ) %>%
      select(.data$proteinID, .data$description, .data$uniprot,
             .data$commentary, .data$refID) %>%
      distinct()
    return(res)
  } else {
    return(NA)
  }
}


#' @title Parse a "RECOMMENDED_NAME" entry into a string.
#'
#' @description Remove useless characters in the description.
#'
#' @param description The description string in a "RECOMMENDED_NAME" entry.
#'
#' @return A string to fill into the `recommended.name` field in
#' `brenda.nomenclature`.
#' @keywords internal
#'
#' @examples
#' x <- "RN\tD-arabinose 1-dehydrogenase (NAD+)"
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
#' @keywords internal
#'
#' @examples
#' x <- "SN\talcohol:NAD+ oxidoreductase"
#' brendaDb:::ParseSystematicName(x)
ParseSystematicName <- function(description) {
  x <- SeparateSubentries(description, acronym = "SN")
  return(x)
}
