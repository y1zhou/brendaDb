#' @title Show all unique BRENDA fields and their corresponding acronyms.
#'
#' @param df A data.frame with columns "field" and "description"
#'
#' @return A data.frame with columns "field" and "acronym".
#'
#' @export
ShowFields <- function(df) {
  if (missing(df)) {
    stop("Missing parameter df. If you want all the fields, please use data(acronyms).")
  } else {
    df_field <- unique(df$field)
    acronyms <- df[!duplicated(df$field), ]
    acronyms$acronym <- sub("^([A-Z]+)\t.*$", "\\1", acronyms$description)
    acronyms <- acronyms[ ,c("field", "acronym")]
  }
  return(acronyms)
}
