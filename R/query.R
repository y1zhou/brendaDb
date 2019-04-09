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
#' @importFrom tibble as_tibble deframe
#' @importFrom dplyr filter select
#' @importFrom stringr str_glue
QueryBrenda <- function(brenda, EC) {
  brenda <- brenda %>%
    filter(ID == EC) %>%
    select(field, description) %>%
    deframe()  # two columns to named vector
  if (length(brenda) == 0) {
    stop(str_glue("EC {EC} not found in brenda table."))
  }
  if ("TRANSFERRED_DELETED" %in% names(brenda)) {
    message(str_glue("{EC} was transferred or deleted."))
    x <- structure(
      list(
        ec = EC,
        msg = brenda[["TRANSFERRED_DELETED"]]
      ),
      class = "brenda.entry"
    )
  } else {
    x <- InitBrendaEntry(
      EC,
      # Nomenclature -------------------------------------------------------------
      protein = ParseProtein(brenda["PROTEIN"]),
      systematic.name = ParseSystematicName(brenda["SYSTEMATIC_NAME"]),
      recommended.name = ParseRecommendedName(brenda["RECOMMENDED_NAME"]),
      synonyms = ParseGeneric(brenda["SYNONYMS"], "SY"),
      reaction = ParseGeneric(brenda["REACTION"], "RE"),
      reaction.type = ParseGeneric(brenda["REACTION_TYPE"], "RT"),

      # Interactions -------------------------------------------------------------
      substrate.product = ParseReaction(brenda["SUBSTRATE_PRODUCT"], "SP"),
      natural.substrate.product = ParseReaction(brenda["NATURAL_SUBSTRATE_PRODUCT"], "NSP"),
      cofactor = ParseGeneric(brenda["COFACTOR"], "CF"),
      metals.ions = ParseGeneric(brenda["METALS_IONS"], "ME"),
      inhibitors = ParseGeneric(brenda["INHIBITORS"], "IN"),
      activating.compound = ParseGeneric(brenda["ACTIVATING_COMPOUND"], "AC"),

      # Parameters ---------------------------------------------------------------
      km.value = ParseGeneric(brenda["KM_VALUE"], "KM"),
      turnover.number = ParseGeneric(brenda["TURNOVER_NUMBER"], "TN"),
      ki.value = ParseGeneric(brenda["KI_VALUE"], "KI"),
      pi.value = ParseGeneric(brenda["PI_VALUE"], "PI"),
      ph.optimum = ParseGeneric(brenda["PH_OPTIMUM"], "PHO"),
      ph.range = ParseGeneric(brenda["PH_RANGE"], "PHR"),
      temperature.optimum = ParseGeneric(brenda["TEMPERATURE_OPTIMUM"], "TO"),
      temperature.range = ParseGeneric(brenda["TEMPERATURE_RANGE"], "TR"),
      specific.activity = ParseGeneric(brenda["SPECIFIC_ACTIVITY"], "SA"),
      ic50 = ParseGeneric(brenda["IC50_VALUE"], "IC50"),

      # Organism information -----------------------------------------------------
      source.tissue = ParseGeneric(brenda["SOURCE_TISSUE"], "ST"),
      localization = ParseGeneric(brenda["LOCALIZATION"], "LO"),

      # Structure ----------------------------------------------------------------
      molecular.weight = ParseGeneric(brenda["MOLECULAR_WEIGHT"], "MW"),
      subunits = ParseGeneric(brenda["SUBUNITS"], "SU"),
      posttranslational.modification = ParseGeneric(brenda["POSTTRANSLATIONAL_MODIFICATION"], "PM"),
      crystallization = ParseNoDescription(brenda["CRYSTALLIZATION"], "CR"),

      # Molecular ----------------------------------------------------------------
      general.stability = ParseNoDescription(brenda["GENERAL_STABILITY"], "GS"),
      storage.stability = ParseNoDescription(brenda["STORAGE_STABILITY"], "SS"),
      ph.stability = ParseGeneric(brenda["PH_STABILITY"], "PHS"),
      organic.solvent.stability = ParseGeneric(brenda["ORGANIC_SOLVENT_STABILITY"], "OSS"),
      oxidation.stability = ParseNoDescription(brenda["OXIDATION_STABILITY"], "OS"),
      temperature.stability = ParseGeneric(brenda["TEMPERATURE_STABILITY"], "TS"),

      purification = ParseGeneric(brenda["PURIFICATION"], "PU"),
      cloned = ParseNoDescription(brenda["CLONED"], "CL"),
      engineering = ParseGeneric(brenda["ENGINEERING"], "EN"),
      renatured = ParseNoDescription(brenda["RENATURED"], "REN"),
      application = ParseGeneric(brenda["APPLICATION"], "AP"),

      # Bibliography -------------------------------------------------------------
      bibliography = ParseReference(brenda["REFERENCE"])
    )
  }
  return(x)
}
