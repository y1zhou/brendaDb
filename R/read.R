#' @title Read BRENDA text file into matrix.
#'
#' @inherit separate_entries return description
#'
#' @inheritParams read_brenda_file
#'
#' @return A matrix containing information about the EC entries.
#'
#' @examples
#' df <- read.brenda("/path/to/brenda/textfile")
#' # Reading BRENDA text file...
#' # Converting text into matrix...This might take a while...
#' head(df)
#' # ID        field             description
#' # 1.1.1.1   PROTEIN           PR	#1# Gallus gallus <44> PR	#2# Cricetulus g...
#' # 1.1.1.1   RECOMMENDED_NAME  RN	alcohol dehydrogenase
#' # 1.1.1.1   SYSTEMATIC_NAME   SN	alcohol:NAD+ oxidoreductase
#'
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
