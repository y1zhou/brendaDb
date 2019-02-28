#' @title Parse a "PROTEIN" entry.
#'
#' @description Expand the string into a `data.table`.
#'
#' @param description The description string in a "PROTEIN" entry.
#'
#' @return A `data.table` with three columns: id, organism and reference.
#'
#' @examples
#' x <- paste0(
#' "PR\t#1# Cavia porcellus  (#1# SULT1A2 <1,2,6,7>) <1,2,6,7>\n",
#' "PR\t#2# Mus musculus <11,18,19>\n")
#' brendaDb:::ParseProtein(x)
#'
#' @importFrom data.table data.table
ParseProtein <- function(description) {
  # Separate experiments, and strip unnecessary whitespace -------------------
  x <- SeparateSubentries(description, acronym = "PR")

  # Split protein numbers, organisms and references --------------------------
  protein.num <- sub("^(#\\d+#).*$", "\\1", x)
  protein.num <- lapply(protein.num, function(x)
    ParseProteinNum(x, type = "protein"))
  # TODO: protein.org string may still contain commentaries wrapped in ()
  protein.org <- trimws(sub("^#\\d+#(.*)<[0-9, ]+>$", "\\1", x))

  ref.num <- sub(".*(<[0-9, ]+>)$", "\\1", x)
  ref.num <- gsub("\\s+", ",", ref.num)
  ref.num <- lapply(ref.num, function(x)
    ParseProteinNum(x, type = "reference"))


  res <- data.table(id = protein.num,
                    organism = protein.org,
                    references = ref.num)
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


#' @title Parse a "SYNONYMS" entry into list of lists.
#'
#' @description Expand the string into a `data.table`.
#'
#' @param description The description string in a "SYNONYMS" entry.
#'
#' @return A `data.table` with three columns: id, synonym and reference.
#'
#' @examples
#' x <- paste0(
#' "SY\t aldehyde reductase\nSY\t dehydrogenase, alcohol\n",
#' "SY\t#8,10,95,97,112,113,135# ADH1 (#10# isozyme <202>)\n",
#' "\t<156,172,202,215,228,252,282>\n")
#' brendaDb:::ParseSynonyms(x)
#'
#' @importFrom stringr str_glue str_extract str_sub str_split str_remove_all
#' @importFrom data.table data.table
ParseSynonyms <- function(description) {
  # Separate experiments, and strip unnecessary whitespace -------------------
  x <- SeparateSubentries(description, acronym = "SY")

  # Some items would be missing protein IDs and/or references ----------------
  protein.num <- sub("^(#[0-9,]+#).*$", "\\1", x)
  protein.num[!grepl("#", protein.num)] <- NA
  protein.num <-
    lapply(protein.num, function(y)
      ParseProteinNum(y, type = "protein"))

  ref.num <- str_extract(x, "<[0-9, ]+>$")
  ref.num[!grepl("<", ref.num)] <- NA
  ref.num <- gsub("\\s+", ",", ref.num)
  ref.num <- lapply(ref.num, function(y)
    ParseProteinNum(y, type = "reference"))

  # TODO: synonym string may still contain commentaries wrapped in ()
  synonym <-
    trimws(sub("^(#[0-9,]+#)?(.*?)(<[0-9, ]+>)?$", "\\2", x))

  res <- data.table(id = protein.num,
                    synonym = synonym,
                    references = ref.num)
  return(res)
}
