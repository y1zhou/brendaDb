#' @title Parser for the description strings of certain reaction-related fields.
#'
#' @description This parser works for the fields `substrate.product` and
#' `natural.substrate.product`.
#'
#' @param description A description string from one of the entries.
#' @param acronym The acronym of the field. Can be found with [ShowFields()].
#'
#' @details The reversibility of the reactions are wrapped in {}; substrates
#' and products are separated by "="; commentaries on substrates are wrapped in
#' "()", and commentaries on products are wrapped in "||".
#'
#' @return A `tibble` with columns: proteinID, substrate, product,
#' commentarySubstrate, commentaryProduct, fieldInfo and refID
#' @keywords internal
#'
#' @import stringr
#' @importFrom tibble tibble
#' @importFrom dplyr distinct
#' @importFrom rlang .data
#' @importFrom purrr map_chr
ParseReaction <- function(description, acronym) {
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

  reversibility <- des.list %>%
    str_extract("\\{(i?r|\\??)\\}") %>%  # {r}, {ir}, {?}, {}
    str_sub(2, -2)

  description <- des.list %>%
    str_remove("^#[0-9, ]+#") %>%
    str_remove("<[0-9, ]+>$") %>%
    str_remove("\\{(i?r|\\??)\\}")

  commentary.substrate <- description %>%
    str_extract("\\(#.*>\\)") %>%
    str_sub(2, -2)
  commentary.product <- description %>%
    str_extract("\\|#.*>\\|") %>%
    str_sub(2, -2)

  description <- description %>%
    str_remove("\\(#.*>\\)") %>%
    str_remove("\\|#.*>\\|") %>%
    str_trim() %>%
    str_split(fixed(" = "))  # separate substrate and product

  res <- tibble(
    proteinID = protein.id,
    substrate = map_chr(description, function(x) x[1]),
    product = map_chr(description, function(x) x[2]),
    commentarySubstrate = commentary.substrate,
    commentaryProduct = commentary.product,
    reversibility = reversibility,
    refID = ref.id
  ) %>%
    distinct(.data$substrate, .data$product, .data$commentarySubstrate,
             .data$commentaryProduct, .data$reversibility, .keep_all = TRUE)
  return(res)
}
