#' @title Show all unique BRENDA fields and their corresponding acronyms.
#'
#' @param df A data.frame with columns "field" and "description"
#'
#' @return A data.frame with columns "field" and "acronym".
#'
#' @importFrom dplyr distinct mutate select
#' @importFrom stringr str_extract
#' @importFrom tibble as_tibble
#' @export
ShowFields <- function(df) {
  if (missing(df)) {
    stop("Missing parameter df. If you want all the fields, please use data(acronyms).")
  } else {
    acronyms <- df %>%
      distinct(field, .keep_all = T) %>%
      mutate(acronym = str_extract(description, "^[A-Z05]+")) %>%
      select(field, acronym) %>%
      as_tibble()
  }
  return(acronyms)
}
