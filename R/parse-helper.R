#' @title Parse protein information strings or reference strings.
#'
#' @description Given a string like "#1,45,72#", parse into a character vector
#' of c("1", "45", "72").
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
#'@importFrom stringr str_glue
ParseProteinNum <- function(x, type) {
  if (missing(type)) {
    stop("Missing parameter: type.")
  }
  if (type == "protein") {
    delim <- "#"
    if (!(grepl("^#(\\d+,)*\\d+#$", x))) {
      stop(str_glue('"{x}" is not a valid protein string. ',
                    "Please check brendaDb:::ParseProteinNum"))
    }
  } else if(type == "reference") {
    delim <- "<|>"
    if (!(grepl("^<(\\d+,)*\\d+>$", x))) {
      stop(str_glue('"{x}" is not a valid reference string. ',
                    "Please check brendaDb:::ParseProteinNum"))
    }
  } else {
    stop(str_glue('Unknown value for parameter type: "{type}".'))
  }

  # Sanity check finished, now parse the string
  x <- gsub(delim, "", x)
  if (grepl("^\\d+$", x)) {
    return(x)
  } else {
    return(strsplit(x, ",")[[1]])
  }
}


