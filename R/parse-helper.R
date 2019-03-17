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
#'
#' @examples
#' brendaDb:::ParseProteinNum("#1,2,3#", "protein")
#' # [1] "1,2,3"
#' brendaDb:::ParseProteinNum("<123>", "reference")
#' # [1] "123"
#'
#'@importFrom stringr str_detect str_glue str_remove_all str_replace_all str_split
ParseProteinNum <- function(x, type) {
  if (is.na(x)) {
    return(NA)
  }
  if (missing(type)) {
    stop("Missing parameter: type.")
  }
  if (type == "protein") {
    if (str_detect(x, "^#[0-9, ]+#$", negate = T) |
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
    if (str_detect(x, "<[0-9, ]+>$", negate = T) |
        str_detect(x, "(<,)|(,>)")) {
      stop(
        str_glue(
          '"{x}" is not a valid reference string. ',
          "Please check brendaDb:::ParseProteinNum"
        )
      )
    }
    delim <- "<|>"
  } else {
    stop(str_glue('Unknown value for parameter type: "{type}".'))
  }

  # Sanity check finished, now parse the string
  x <- str_remove_all(x, delim)
  if (str_detect(x, "^\\d+$")) {
    return(x)
  } else {
    x <- str_replace_all(x, "\\s+", ",")
    x <- str_replace_all(x, ",+", ",")
    return(str_split(x, ",")[[1]])
  }
}


#' @title Preprocessing of entry description.
#'
#' @description Separate subentries and strip unnecessary whitespace.
#'
#' @param description The `description` string of the field.
#' @param acronym The acronym of the field. Can be found with `ShowFields()`.
#'
#' @return A list of strings with each subentry as an element.
#'
#' @examples
#' x <- "SN\talcohol:NAD+ oxidoreductase"
#' brendaDb:::SeparateSubentries(x, "SN")
#' @importFrom stringr str_detect str_split str_remove_all str_replace_all
SeparateSubentries <- function(description, acronym) {
  if (str_detect(description, paste0("^", acronym, "\t"), negate = T)) {
    warning("The description doesn't seem to match your provided acronym.")
    return(NA)
  }
  x <- str_split(description, paste0("\n", acronym, "\t"))[[1]]
  x <- str_remove_all(trimws(x), paste0("^", acronym, "(\\s+)?"))
  # The following line will result in some references delimited by " "
  # instead of ,
  x <- str_replace_all(x, "\n\t", " ")
  return(x)
}


#' @title Parse commentaries inside the description.
#'
#' @description Parse the commentaries inside parentheses in the given string.
#'
#' @param description A BRENDA description entry with commentaries.
#'
#' @return A list of commentaries.
#'
#' @examples
#' brendaDb:::ParseCommentary("Cavia porcellus   (#1# SULT1A2 <1,2,6,7>)")
#'
#' @importFrom stringr str_replace_all str_extract str_sub str_split str_remove_all
#' @importFrom tibble tibble
ParseCommentary <- function(description) {
  if (missing(description)) {
    stop("Parameter description missing.")
  }
  if (!(is.character(description)) | length(description) > 1) {
    stop("Parameter description has to be a single string.")
  }

  description <- str_replace_all(description, "\n\t", " ")
  if (!(grepl("\\(#.*?>\\)", description))) {
    return(NA)
  } else {
    description <- str_extract(description, "\\(#.*>\\)")
    description <- str_sub(description, 2,-2)  # Remove parentheses
    description <- str_split(description, "; ")[[1]]

    protein.id <- str_extract(description, "^#[0-9,]+#")
    protein.id <- lapply(protein.id, function(x)
      ParseProteinNum(x, type = "protein"))

    refs <- str_extract(description, "<[0-9, ]+>$")
    refs <- str_replace_all(refs, "\\s+", ",")
    refs <- str_replace_all(refs, ",+", ",")
    refs <- lapply(refs, function(x)
      ParseProteinNum(x, type = "reference"))

    commentary <- str_remove_all(description,
                                 "(^#[0-9,]+#\\s+)|(\\s+<[0-9, ]+>$)")
    res <- tibble(id = protein.id,
                      commentary = commentary,
                      references = refs)
    return(res)
  }
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
#' @param acronym The acronym of the field. Can be found with `ShowFields()`.
#'
#' @details
#' The `description` column contains values extracted by BRENDA in each field.
#'
#' The `fieldInfo` column contains different information in different fields:
#' - In a `SYNONYMS` entry, it is either the source of the identifier, or part
#' of the description (a false positive).
#' - In `KM_VALUE`, `TURNOVER_NUMBER` entries, it is the corresponding substrate.
#'
#' @return A `tibble` with columns: proteinID, description, fieldInfo,
#' commentary, and refID
#'
#' @importFrom stringr str_extract str_sub str_remove str_trim
#' @importFrom tibble tibble
ParseGeneric <- function(description, acronym) {
  des.list <- SeparateSubentries(description, acronym = acronym)
  protein.id <- str_extract(des.list, "^#[0-9, ]+#")
  protein.id <- lapply(protein.id, function(x)
    ParseProteinNum(x, type = "protein"))

  ref.id <- str_extract(des.list, "<[0-9, ]+>$")
  ref.id <- lapply(ref.id, function(x)
    ParseProteinNum(x, type = "reference"))

  field.info <- des.list %>%
    str_extract("\\{.*?\\}") %>%  # should have at most one match
    str_sub(2, -2)

  description <- des.list %>%
    str_remove("^#[0-9, ]+#") %>%
    str_remove("<[0-9, ]+>$") %>%
    str_remove("\\{.*?\\}")  # remove first because there could be () in {}

  commentary <- description %>%
    str_extract("\\(.*\\)") %>%  # greedy because there could be ) in commentary
    str_sub(2, -2)

  description <- description %>%
    str_remove("\\(.*\\)") %>%
    str_trim()

  res <- tibble(
    proteinID = protein.id,
    description = description,
    fieldInfo = field.info,
    commentary = commentary,
    refID = ref.id
  )
  # https://www.brenda-enzymes.org/enzyme.php?ecno=1.1.1.100&organism%5B%5D=Mycobacterium+tuberculosis#pH%20OPTIMUM
  res$description[res$description == "-999"] <- "additional_information"
  return(res)
}
