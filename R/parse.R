#' @title Read BRENDA text file into matrix.
#'
#' @inherit long_to_wide return description
#'
#' @inheritParams read_brenda
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
  df <- read_brenda(filepath)  # src/read_brenda
  df <- do.call(rbind, df)
  colnames(df) <- c("ID", "field", "description")
  return(df)
}
