#' @title Read BRENDA text file into matrix.
#'
#' @inherit SeparateEntries return description
#'
#' @inheritParams ReadBrendaFile
#'
#' @return A matrix containing information about the EC entries.
#'
#' @examples
#' brenda_txt <- system.file("extdata", "brenda_download_test.txt",
#'                           package = "brendaDb")
#' df <- ReadBrenda(brenda_txt)
#' # Reading BRENDA text file...
#' # Converting text into matrix...This might take a while...
#' head(df)
#' # ID         field             description
#' # 1.1.1.10   PROTEIN           PR	PR  #1# Cavia porcellus   (#1# SULT1A2...
#' # 1.1.1.10   RECOMMENDED_NAME  RN  L-xylulose reductase
#' # 1.1.1.10   SYSTEMATIC_NAME   SN  xylitol:NADP+ 4-oxidoreductase (L-xyl...
#'
#' @importFrom data.table as.data.table
#' @importFrom dplyr distinct
#' @export
ReadBrenda <- function(filepath) {
  # src/read_brenda
  message("Reading BRENDA text file...")
  filepath <- path.expand(filepath)
  df <- ReadBrendaFile(filepath)

  message("Converting text into matrix. This might take a while...")
  df <- SeparateEntries(df)

  # Convert list of lists to matrix
  df <- matrix(unlist(df, use.names = F), ncol = length(df[[1]]), byrow = T)

  message("Converting matrix to data.table and removing duplicated entries...")
  df <- data.table::as.data.table(df)
  colnames(df) <- c("ID", "field", "description")
  df <- distinct(df)
  return(df)
}
