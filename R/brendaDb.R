#' brendaDb: the BRENDA enzyme database
#'
#' The main aims of brendaDb include:
#' - Make it easier to import and analyze enzyme information in R
#' - Generate enzyme activity profiles
#'
#' @useDynLib brendaDb
#' @importFrom Rcpp sourceCpp
"_PACKAGE"

#' Information fields and their corresponding acronyms
#' @docType data
#' @keywords internal
#' @name acronyms
#' @usage data(acronyms)
#' @format A data.frame with 39 rows and 2 variables
"acronyms"
