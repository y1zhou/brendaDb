#' @title Parse protein information strings or reference strings.
#'
#' @description Given a string like "#1,45,72#", parse into a character vector
#' of c("1", "45", "72"). Consecutive commas are collapsed into one, and spaces
#' are treated as commas.
#'
#' @param x A string in the format of "#1#" or "#1,2,3#" or "<1,3>".
#' @param type Either "protein" or "reference".
#'
#' @return A string, or a vector of strings of protein numbers.
#' @keywords internal
#'
#' @examples
#' brendaDb:::ParseProteinNum("#1,2,3#", "protein")
#' # [1] "1,2,3"
#' brendaDb:::ParseProteinNum("<123>", "reference")
#' # [1] "123"
#'
#'@import stringr
ParseProteinNum <- function(x, type) {
  if (is.na(x)) {
    return(NA)
  }
  if (missing(type)) {
    stop("Missing parameter: type.")
  }
  if (type == "protein") {
    if (str_detect(x, "^#[0-9, ]+#$", negate = TRUE) |
        str_detect(x, "(#,)|(,#)")) {
      stop(
        str_glue(
          '"{x}" is not a valid protein string. ',
          "Please check brendaDb:::ParseProteinNum"
        )
      )
    }
    delim <- "#"
  } else if (type == "reference") {
    if (str_detect(x, "<[0-9, ]+>$", negate = TRUE) |
        str_detect(x, "(<,)|(,>)")) {
      stop(
        str_glue(
          '"{x}" is not a valid reference string. ',
          "Please check brendaDb:::ParseProteinNum"
        )
      )
    }
    delim <- "[<>]"
  } else {
    stop(str_glue('Unknown value for parameter type: "{type}".'))
  }

  x <- str_remove_all(x, delim)
  x <- str_replace_all(x, "[\\s,]+", ",")
  return(x)
}


#' @title Preprocessing of entry description.
#'
#' @description Separate subentries and strip unnecessary whitespace.
#'
#' @param description The `description` string of the field.
#' @param acronym The acronym of the field. Can be found with [ShowFields()].
#'
#' @return A list of strings with each subentry as an element.
#' @keywords internal
#'
#' @examples
#' x <- "SN\talcohol:NAD+ oxidoreductase"
#' brendaDb:::SeparateSubentries(x, "SN")
#' @import stringr
SeparateSubentries <- function(description, acronym = NA) {
  if (is.na(acronym)) {
    message("Please specify the acronym.")
    return(NA)
  }
  if (is.na(description) | str_trim(description) == acronym) {
    return (NA)
  }
  if (str_detect(description, paste0("^", acronym, "\t"), negate = TRUE)) {
    warning(str_glue(
      "The description doesn't seem to match your provided acronym '{acronym}'")
    )
    return(NA)
  }
  x <- str_split(description, paste0("\n", acronym, "\t"))[[1]]
  x <- x %>%
    str_trim() %>%
    str_remove_all(paste0("^", acronym, "(\\s+)?")) %>%
    # The following line will result in some references delimited by " "
    # instead of ,
    str_replace_all(regex("\n\t", fixed = TRUE), " ")
  return(x)
}


#' @title Generic parser for a description string.
#'
#' @description Descriptions are generally structured as the following:
#' - Protein information is included in '#'...#',
#' - Literature citations are in '<...>',
#' - Commentaries in '(...)', and
#' - field-special information in '{...}'.
#'
#' This function separates these fields into different columns.
#'
#' @param description A description string from one of the entries.
#' @param acronym The acronym of the field. Can be found with [ShowFields()].
#'
#' @details
#' The `description` column contains values extracted by BRENDA in each field.
#'
#' The `fieldInfo` column contains different information in different fields:
#' - In a `SYNONYMS` entry, it is either the source of the identifier, or part
#' of the description (a false positive).
#' - In `KM_VALUE`, `TURNOVER_NUMBER` entries, it's the corresponding substrate.
#'
#' @return A `tibble` with columns: proteinID, description, fieldInfo,
#' commentary, and refID
#' @keywords internal
#'
#' @import stringr
#' @importFrom magrittr %>%
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
#' @importFrom rlang .data
ParseGeneric <- function(description, acronym) {
  if (is.na(description)) {
    return(NA)
  }
  des.list <- SeparateSubentries(description, acronym = acronym)
  protein.id <- des.list %>%
    str_extract("^#[0-9, ]+#") %>%
    map_chr(function(x) ParseProteinNum(x, type = "protein"))

  ref.id <- des.list %>%
    str_extract("<[0-9, ]+>$") %>%
    map_chr(function(x) ParseProteinNum(x, type = "reference"))

  if (acronym %in% c("SY", "TN", "KM", "IN", "KI", "IC50")) {
    field.info <- des.list %>%
      str_extract("\\{.*?(\\}$|\\}\\s+[(<])") %>%  # should have <= 1 match
      # remove {}, and trailing < or (
      str_remove_all("([\\s{}])|[(<]$")
    description <- des.list %>%
      str_remove("^#[0-9, ]+#") %>%
      str_remove("<[0-9, ]+>$") %>%
      # remove field-specific information, but keep markers for commentaries
      str_replace("\\{.*?\\}(\\s+\\()?", "\\1")
  } else {
    field.info <- NA
    description <- des.list %>%
      str_remove("^#[0-9, ]+#") %>%
      str_remove("<[0-9, ]+>$")
  }

  commentary <- description %>%
    str_extract("\\(#.*>\\)") %>%  # greedy; there could be ) in commentary
    str_sub(2, -2)

  description <- description %>%
    str_remove("\\(#.*>\\)") %>%
    str_trim()

  res <- tibble(
    proteinID = protein.id,
    description = description,
    fieldInfo = field.info,
    commentary = commentary,
    refID = ref.id
  ) %>%
    distinct(.data$description, .data$fieldInfo, .data$commentary,
             .keep_all = TRUE)
  # https://www.brenda-enzymes.org/enzyme.php?ecno=1.1.1.1&organism=Mus+musculus#pH%20OPTIMUM
  res$description[res$description %in% c("-999", "more", "More")] <- "additional information"
  return(res)
}


#' @title Generic parser for a description string without extracted values.
#'
#' @description This parser works for fields `storage.stability`,
#' `general.stability`, `oxidation.stability`, `cloned`, `purification`,
#' `crystallization` and `renatured`.
#'
#' These fields in BRENDA don't have extracted values - the commentary itself is
#' the extracted value.
#'
#' @param description A description string from one of the entries.
#' @param acronym The acronym of the field. Can be found with [ShowFields()].
#'
#' @return A `tibble` with columns: proteinID, description and refID.
#' @keywords internal
#'
#' @import stringr
#' @importFrom purrr map_chr
#' @importFrom tibble tibble
ParseNoDescription <- function(description, acronym) {
  if (is.na(description)) {
    return(NA)
  }
  des.list <- SeparateSubentries(description, acronym = acronym)
  protein.id <- des.list %>%
    str_extract("^#[0-9, ]+#") %>%
    map_chr(function(x) ParseProteinNum(x, type = "protein"))

  ref.id <- des.list %>%
    str_extract("<[0-9, ]+>$") %>%
    map_chr(function(x) ParseProteinNum(x, type = "reference"))

  description <- des.list %>%
    str_remove("^#[0-9, ]+#") %>%
    str_remove("<[0-9, ]+>$") %>%
    str_extract("\\(.*\\)") %>%
    str_sub(2, -2)  # remove parentheses

  res <- tibble(
    proteinID = protein.id,
    description = description,
    refID = ref.id
  ) %>%
    distinct(description, .keep_all = TRUE)
  return(res)
}
