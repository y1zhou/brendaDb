#' @title Query for a specific enzyme.
#'
#' @description Use a EC number to retrieve information from the BRENDA
#' `data.table` read in by `ReadBrenda`.
#'
#' @param brenda A `data.table` containing information from BRENDA.
#' @param EC A string of the EC number.
#'
#' @return A `brenda.entry` object.
#'
#' @seealso ReadBrenda InitBrendaEntry
#' @export
#' @examples
#' df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
#'                           package = "brendaDb"))
#' QueryBrenda(brenda = df, EC = "1.1.1.10")
#'
#' @importFrom data.table as.data.table
#' @importFrom stringr str_glue
QueryBrenda <- function(brenda, EC) {
  brenda <- as.data.table(brenda)
  brenda <- brenda[ID == EC]
  if (nrow(brenda) == 0) {
    stop(str_glue("EC {EC} not found in brenda data table."))
  }
  x <- InitBrendaEntry(EC)
  x$nomenclature$protein <-
    ParseProtein(brenda[field == "PROTEIN", description])
  x$nomenclature$systematic.name <-
    ParseSystematicName(brenda[field == "SYSTEMATIC_NAME", description])
  x$nomenclature$recommended.name <-
    ParseRecommendedName(brenda[field == "RECOMMENDED_NAME", description])
  x$nomenclature$synonyms <-
    ParseSynonyms(brenda[field == "SYNONYMS", description])
  return(x)
}
