#' @title Query for multiple enzymes.
#'
#' @description Use a vector of EC numbers to retrieve information from the BRENDA
#' `tibble` read in by [ReadBrenda()]. Invalid EC numbers will be removed.
#'
#' @inheritParams QueryBrendaBase
#' @inheritParams ConfigBPCores
#' @param ... Other parameters passed to [QueryBrendaBase()].
#'
#' @return A list of `brenda.entry` objects.
#'
#' @seealso [QueryBrendaBase()] [ConfigBPCores()] [SelectOrganism()]
#' @export
#' @examples
#' df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
#'                           package = "brendaDb"))
#' res <- QueryBrenda(brenda = df, EC = c("1.1.1.1", "1.1.1.10", "6.3.5.8"),
#'                    n.core = 2, organisms = "Homo sapiens")
#'
#' @import BiocParallel
#' @importFrom purrr map_chr
QueryBrenda <- function(brenda, EC, n.core = 0, ...) {
  EC.not.found <- EC[!EC %in% brenda$ID]
  if (length(EC.not.found) != 0) {
    message(str_glue(
      "Invalid EC number(s) removed: {paste(EC.not.found, collapse = ', ')}"
    ))
  }
  EC <- EC[EC %in% brenda$ID]
  BP.param <- ConfigBPCores(n.core = n.core)
  res <- bplapply(EC, function(x) QueryBrendaBase(brenda, x, ...),
                  BPPARAM = BP.param)
  names(res) <- map_chr(res, function(x) x$nomenclature$ec)
  class(res) <- "brenda.entries"
  return(res)
}


#' @title A helper function for converting names/synonyms to EC numbers.
#'
#' @param brenda A `tibble` generated from [ReadBrenda()].
#' @param ids A character vector of IDs to be converted.
#'
#' @return A tibble with columns ID, EC, and at least one of (RECOMMENDED_NAME,
#' SYSTEMATIC_NAME and SYNONYMS).
#'
#' @details The function goes through "RECOMMENDED_NAME", "SYSTEMATIC_NAME", and
#' "SYNONYMS" in the BRENDA file, and uses regexes to look for the given IDs.
#' Values in the three columns are kept if the regex had a hit, otherwise NA is filled.
#' The function can take in IDs of multiple sources, e.g. `c("ADH4", "CD38",
#' "pyruvate dehydrogenase")`. Note that using aliases instead of symbols could
#' lead to false positives in the output table.
#'
#' @import stringr
#' @importFrom dplyr filter mutate select
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#' @importFrom tidyr spread
#'
#' @export
ID2Enzyme <- function(brenda, ids) {
  ids <- as.character(ids[!is.na(ids)])

  brenda <- brenda %>%
    filter(field %in% c("RECOMMENDED_NAME", "SYSTEMATIC_NAME", "SYNONYMS")) %>%
    mutate(
      description = str_remove(description, "^(RN|SN|SY)\\s+"),
      description = str_replace_all(description, "\nSY\\s+", "\n"),
      description = str_trim(description)
    )

  map_dfr(ids, function(x) {
    brenda %>%
      filter(str_detect(description, str_glue("\\b({x})"))) %>%
      mutate(fromID = x)
  }) %>%
    as_tibble() %>%
    select(ID = fromID, EC = ID, field, description) %>%
    spread(key = "field", value = "description")
}


#' @title Query for a specific enzyme.
#'
#' @description Use a EC number to retrieve information from the BRENDA
#' `tibble` read in by [ReadBrenda()].
#'
#' @param brenda A `tibble` containing information from BRENDA.
#' @param EC A string of the EC number.
#' @param fields A character vector indicating fields to parse. Default is
#' FALSE, which would be returning all fields.
#' @param organisms A character vector indicating organisms to keep. Default is
#' FALSE, which would keep all organisms.
#'
#' @return A `brenda.entry` object.
#'
#' @seealso [ReadBrenda()] [InitBrendaEntry()]
#' @examples
#' df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
#'                           package = "brendaDb"))
#' brendaDb:::QueryBrendaBase(brenda = df, EC = "1.1.1.1",
#'                            fields = "PH_RANGE", organisms = "Homo sapiens")
#'
#' @importFrom tibble as_tibble deframe
#' @importFrom dplyr filter select
#' @importFrom stringr str_glue
QueryBrendaBase <- function(brenda, EC, fields = F, organisms = F) {
  brenda <- brenda %>%
    filter(ID == EC) %>%
    select(field, description) %>%
    deframe()  # two columns to named vector

  if ("TRANSFERRED_DELETED" %in% names(brenda)) {
    message(str_glue("{EC} was transferred or deleted."))
    query <- InitBrendaDeprecatedEntry(EC, brenda[["TRANSFERRED_DELETED"]])
  } else {
    # Select certain fields
    if (is.character(fields)) {
      brenda <- brenda[names(brenda) %in% c("PROTEIN", "REFERENCE", fields)]
    }
    query <- InitBrendaEntry(
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

    if (is.character(organisms)) {
      org.id <- query$organism$organism %>%
        filter(description %in% organisms) %>%
        select(description, proteinID) %>%
        deframe()
      query <- SelectOrganism(query, org.id)
    }
  }
  return(query)
}


#' @title Configure the number of cores used by BiocParallel.
#'
#' @param n.core Integer specifying the number of cores to use. Default is 0,
#' which would result in using all available cores.
#'
#' @return The back-end of type `bpparamClass`.
#'
#' @seealso [BiocParallel::bpparam()]
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


#' @title Select a subset of organisms in the brenda.query object.
#'
#' @param query A brenda.entry object from [QueryBrendaBase()].
#' @param org.id A named character vector with values as `proteinID`s.
#'
#' @return A subset of the given brenda.query object with the given organisms
#' selected.
#'
#' @seealso [QueryBrendaBase()]
#'
#' @importFrom dplyr filter
#' @importFrom purrr map map_lgl
#' @importFrom tibble is_tibble
SelectOrganism <- function(query, org.id) {
  if(inherits(query, c("brenda.entry", "brenda.sublist"))) {
    res.class <- class(query)
    res <- map(query, function(x) SelectOrganism(x, org.id))
    class(res) <- res.class
  } else if(is_tibble(query) & ("proteinID" %in% colnames(query))) {
    # Rows should contain at least one of the organism IDs.
    res <- query %>%
      filter(
        map_lgl(str_split(proteinID, ","),
                function(x) any(x %in% org.id) | all(is.na(x)))
      )
  } else {
    res <- query
  }
  return(res)
}
