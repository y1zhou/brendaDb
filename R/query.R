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
#' QueryBrenda(brenda = df, EC = "1.1.1.1")
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
  # Nomenclature -------------------------------------------------------------
  x$nomenclature$protein <-
    ParseProtein(brenda[brenda$field == "PROTEIN", ]$description)
  x$nomenclature$systematic.name <-
    ParseSystematicName(brenda[brenda$field == "SYSTEMATIC_NAME", ]$description)
  x$nomenclature$recommended.name <-
    ParseRecommendedName(brenda[brenda$field == "RECOMMENDED_NAME", ]$description)
  x$nomenclature$synonyms <-
    ParseGeneric(brenda[brenda$field == "SYNONYMS", ]$description, "SY")
  x$nomenclature$reaction <-
    ParseGeneric(brenda[brenda$field == "REACTION", ]$description, "RE")
  x$nomenclature$reaction.type <-  # TODO: only one column has values
    ParseGeneric(brenda[brenda$field == "REACTION_TYPE", ]$description, "RT")

  # Interactions -------------------------------------------------------------
  x$interactions$substrate.product <-
    ParseReaction(brenda[brenda$field == "SUBSTRATE_PRODUCT", ]$description, "SP")
  x$interactions$natural.substrate.product <-
    ParseReaction(brenda[brenda$field == "NATURAL_SUBSTRATE_PRODUCT", ]$description, "NSP")
  x$interactions$cofactor <-
    ParseGeneric(brenda[brenda$field == "COFACTOR", ]$description, "CF")
  x$interactions$metals.ions <-
    ParseGeneric(brenda[brenda$field == "METALS_IONS", ]$description, "ME")
  x$interactions$activating.compound <-
    ParseGeneric(brenda[brenda$field == "ACTIVATING_COMPOUND", ]$description, "AC")
  x$interactions$inhibitors <-
    ParseGeneric(brenda[brenda$field == "INHIBITORS", ]$description, "IN")
  x$interactions$activating.compound <-
    ParseGeneric(brenda[brenda$field == "ACTIVATING_COMPOUND", ]$description, "AC")

  # Parameters ---------------------------------------------------------------
  x$parameters$km.value <-
    ParseGeneric(brenda[brenda$field == "KM_VALUE", ]$description, "KM")
  x$parameters$turnover.number <-
    ParseGeneric(brenda[brenda$field == "TURNOVER_NUMBER", ]$description, "TN")
  x$parameters$ki.value <-
    ParseGeneric(brenda[brenda$field == "KI_VALUE", ]$description, "KI")
  x$parameters$pi.value <-
    ParseGeneric(brenda[brenda$field == "PI_VALUE", ]$description, "PI")
  x$parameters$ph.optimum <-
    ParseGeneric(brenda[brenda$field == "PH_OPTIMUM", ]$description, "PHO")
  x$parameters$ph.range <-
    ParseGeneric(brenda[brenda$field == "PH_RANGE", ]$description, "PHR")
  x$parameters$temperature.optimum <-
    ParseGeneric(brenda[brenda$field == "TEMPERATURE_OPTIMUM", ]$description, "TO")
  x$parameters$temperature.range <-
    ParseGeneric(brenda[brenda$field == "TEMPERATURE_RANGE", ]$description, "TR")
  x$parameters$specific.activity <-
    ParseGeneric(brenda[brenda$field == "SPECIFIC_ACTIVITY", ]$description, "SA")
  x$parameters$ic50 <-
    ParseGeneric(brenda[brenda$field == "IC50_VALUE", ]$description, "IC50")

  # Organism information -----------------------------------------------------
  x$organism$source.tissue <-
    ParseGeneric(brenda[brenda$field == "SOURCE_TISSUE", ]$description, "ST")
  x$organism$localization <-
    ParseGeneric(brenda[brenda$field == "LOCALIZATION", ]$description, "LO")

  # Structure ----------------------------------------------------------------
  x$structure$molecular.weight <-
    ParseGeneric(brenda[brenda$field == "MOLECULAR_WEIGHT", ]$description, "MW")
  x$structure$subunits <-
    ParseGeneric(brenda[brenda$field == "SUBUNITS", ]$description, "SU")
  x$structure$posttranslational.modification <-
    ParseGeneric(brenda[brenda$field == "POSTTRANSLATIONAL_MODIFICATION", ]$description, "PM")
  x$structure$cystallization <-
    ParseNoDescription(brenda[brenda$field == "CRYSTALLIZATION", ]$description, "CR")

  # Molecular ----------------------------------------------------------------
  x$molecular$stability$general.stability <-
    ParseNoDescription(brenda[brenda$field == "GENERAL_STABILITY", ]$description, "GS")
  x$molecular$stability$storage.stability <-
    ParseNoDescription(brenda[brenda$field == "STORAGE_STABILITY", ]$description, "SS")
  x$molecular$stability$ph.stability <-
    ParseGeneric(brenda[brenda$field == "PH_STABILITY", ]$description, "PHS")
  x$molecular$stability$organic.solvent.stability <-
    ParseGeneric(brenda[brenda$field == "ORGANIC_SOLVENT_STABILITY", ]$description, "OSS")
  x$molecular$stability$oxidation.stability <-
    ParseNoDescription(brenda[brenda$field == "OXIDATION_STABILITY", ]$description, "OS")
  x$molecular$stability$temperature.stability <-
    ParseGeneric(brenda[brenda$field == "TEMPERATURE_STABILITY", ]$description, "TS")

  x$molecular$purification <-
    ParseGeneric(brenda[brenda$field == "PURIFICATION", ]$description, "PU")
  x$molecular$cloned <-
    ParseNoDescription(brenda[brenda$field == "CLONED", ]$description, "CL")
  x$molecular$engineering <-
    ParseGeneric(brenda[brenda$field == "ENGINEERING", ]$description, "EN")
  x$molecular$renatured <-
    ParseNoDescription(brenda[brenda$field == "RENATURED", ]$description, "REN")
  x$molecular$application <-
    ParseGeneric(brenda[brenda$field == "APPLICATION", ]$description, "AP")

  # Bibliography -------------------------------------------------------------
  x$bibliography <-
    ParseReference(brenda[brenda$field == "REFERENCE", ]$description)

  return(x)
}
