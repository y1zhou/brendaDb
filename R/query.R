#' @title Query for multiple enzymes.
#'
#' @description Use a vector of EC numbers to retrieve information from the BRENDA
#' `tibble` read in by `ReadBrenda()`. Invalid EC numbers will be removed.
#'
#' @inheritParams QueryBrendaBase
#' @inheritParams ConfigBPCores
#'
#' @return A list of `brenda.entry` objects.
#'
#' @seealso QueryBrendaBase
#' @export
#' @examples
#' df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
#'                           package = "brendaDb"))
#' QueryBrenda(brenda = df, EC = c("1.1.1.1", "1.1.1.10", "6.3.5.8"))
#'
#' @import BiocParallel
#' @importFrom purrr map_chr
QueryBrenda <- function(brenda, EC, fields = F, n.core = 0) {
  EC.not.found <- EC[!EC %in% brenda$ID]
  if (length(EC.not.found) != 0) {
    message(str_glue(
      "Invalid EC number(s) removed: {paste(EC.not.found, collapse = ', ')}"
    ))
  }
  EC <- EC[EC %in% brenda$ID]
  BP.param <- ConfigBPCores(n.core = n.core)
  res <- bplapply(EC, function(x) QueryBrendaBase(brenda, x, fields),
                  BPPARAM = BP.param)
  names(res) <- map_chr(res, function(x) x$nomenclature$ec)
  return(res)
}


#' @title Query for a specific enzyme.
#'
#' @description Use a EC number to retrieve information from the BRENDA
#' `tibble` read in by `ReadBrenda()`.
#'
#' @param brenda A `tibble` containing information from BRENDA.
#' @param EC A string of the EC number.
#' @param fields A character vector indicating fields to parse. Default is
#' FALSE, which would be returning all fields.
#'
#' @return A `brenda.entry` object.
#'
#' @seealso ReadBrenda InitBrendaEntry
#' @examples
#' df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
#'                           package = "brendaDb"))
#' brendaDb:::QueryBrendaBase(brenda = df, EC = "1.1.1.1")
#'
#' @importFrom tibble as_tibble deframe
#' @importFrom dplyr filter select
#' @importFrom stringr str_glue
QueryBrendaBase <- function(brenda, EC, fields = F) {
  brenda <- brenda %>%
    filter(ID == EC) %>%
    select(field, description) %>%
    deframe()  # two columns to named vector

  # Don't use InitBrendaEntry for transferred / deleted entries
  if ("TRANSFERRED_DELETED" %in% names(brenda)) {
    message(str_glue("{EC} was transferred or deleted."))
    x <- structure(
      list(
        nomenclature = list(
          ec = EC
        ),
        msg = brenda[["TRANSFERRED_DELETED"]]
      ),
      class = "brenda.entry"
    )
  } else {
    # Select certain fields
    if (is.character(fields)) {
      brenda <- brenda[names(brenda) %in% fields]
    }
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


#' @title Configure the number of cores used by BiocParallel.
#'
#' @param n.core Integer specifying the number of cores to use. Default is 0,
#' which would result in using all available cores.
#'
#' @return The back-end of type bpparamClass.
#'
#' @examples
#' brendaDb:::ConfigBPCores(2)
#'
#' @import BiocParallel
ConfigBPCores <- function(n.core = 0){
  if (n.core != 0) {
    if (.Platform$OS.type == "windows") {
      # Use the cluster approach to spawn processes on Windows machines
      res <- SnowParam(workers = n.core)
    } else {
      # The multicore approach is recommended on other platforms
      res <- MulticoreParam(workers = n.core)
    }
  } else {
    # By default, use the first registered backend returned by bpparam()
    res <- bpparam()
  }
  return(res)
}
