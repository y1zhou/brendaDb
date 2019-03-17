#' @title Query for a specific enzyme.
#'
#' @description Use a EC number to retrieve information from the BRENDA
#' `tibble` read in by `ReadBrenda()`.
#'
#' @param brenda A `tibble` containing information from BRENDA.
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
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter
#' @importFrom stringr str_glue
QueryBrenda <- function(brenda, EC) {
  brenda <- brenda %>%
    as_tibble() %>%
    filter(ID == EC)
  if (nrow(brenda) == 0) {
    stop(str_glue("EC {EC} not found in brenda table."))
  }
  x <- InitBrendaEntry(EC)
  x$nomenclature$protein <-
    ParseProtein(brenda[brenda$field == "PROTEIN", ]$description)
  x$nomenclature$systematic.name <-
    ParseSystematicName(brenda[brenda$field == "SYSTEMATIC_NAME", ]$description)
  x$nomenclature$recommended.name <-
    ParseRecommendedName(brenda[brenda$field == "RECOMMENDED_NAME", ]$description)
  x$nomenclature$synonyms <-
    ParseGeneric(brenda[brenda$field == "SYNONYMS", ]$description, "SY")

  x$parameters$km.value <-
    ParseGeneric(brenda[brenda$field == "KM_VALUE", ]$description, "KM")
  x$parameters$ph.optimum <-
    ParseGeneric(brenda[brenda$field == "PH_OPTIMUM", ]$description, "PHO")
  x$parameters$ph.range <-
    ParseGeneric(brenda[brenda$field == "PH_RANGE", ]$description, "PHR")

  x$bibliography$reference <-
    ParseReference(brenda[brenda$field == "REFERENCE", ]$description)
  x$bibliography$application <-
    ParseGeneric(brenda[brenda$field == "APPLICATION", ]$description, "AP")
  return(x)
}
