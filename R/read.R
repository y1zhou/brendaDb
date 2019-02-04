#' @title Read BRENDA text file into matrix.
#'
#' @inherit separate_entries return description
#'
#' @inheritParams read_brenda_file
#'
#' @return A matrix containing information about the EC entries.
#'
#' @examples
#' df <- read.brenda("inst/extdata/brenda_download_test.txt")
#' # Reading BRENDA text file...
#' # Converting text into matrix...This might take a while...
#' head(df)
#' # ID         field             description
#' # 1.1.1.10   PROTEIN           PR	PR  #1# Cavia porcellus   (#1# SULT1A2...
#' # 1.1.1.10   RECOMMENDED_NAME  RN  L-xylulose reductase
#' # 1.1.1.10   SYSTEMATIC_NAME   SN  xylitol:NADP+ 4-oxidoreductase (L-xyl...
#'
#'@importFrom data.table as.data.table
#' @export
read.brenda <- function(filepath) {
  # src/read_brenda
  message("Reading BRENDA text file...")
  df <- read_brenda_file(filepath)

  message("Converting text into matrix. This might take a while...")
  df <- separate_entries(df)

  # Convert list of lists to matrix
  df <- matrix(unlist(df, use.names = F), ncol = length(df[[1]]), byrow = T)
  df <- data.table::as.data.table(df)
  colnames(df) <- c("ID", "field", "description")
  return(df)
}
