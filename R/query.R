#' @title Query for multiple enzymes.
#'
#' @description Use a vector of EC numbers to retrieve information from the
#' BRENDA `tibble` read in by [ReadBrenda()]. Invalid EC numbers will be removed
#' and a message will be generated.
#'
#' @inheritParams QueryBrendaBase
#' @inheritParams ConfigBPCores
#' @param fields A character vector indicating fields to parse. Default is
#' FALSE, which would be returning all fields.
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
QueryBrenda <- function(brenda, EC, n.core = 0, fields = FALSE, ...) {
  # Select certain fields
  if (is.character(fields)) {
    brenda <- brenda[brenda$field %in% c("PROTEIN", "REFERENCE", "RECOMMENDED_NAME", fields), ]
  }
  EC.not.found <- EC[!EC %in% brenda$ID]
  if (length(EC.not.found) != 0) {
    message(str_glue(
      "{length(EC.not.found)} invalid EC number(s) removed:\n",
      "{paste(EC.not.found[1:min(length(EC.not.found), 50)], collapse = ', ')}"
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
#' Values in the three columns are kept if the regex had a hit, otherwise NA is
#' filled. The function can take in IDs of multiple sources, e.g. `c("ADH4",
#' "CD38", "pyruvate dehydrogenase")`. Note that using aliases instead of
#' symbols could lead to false positives in the output table.
#'
#' @import stringr
#' @importFrom dplyr filter mutate select
#' @importFrom rlang .data
#' @importFrom purrr map_dfr
#' @importFrom tibble as_tibble
#' @importFrom tidyr spread
#'
#' @examples
#' df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
#'                           package = "brendaDb"))
#' ID2Enzyme(df, c("CD38", "ADH4", "pyruvate dehydrogenase"))
#'
#' @export
ID2Enzyme <- function(brenda, ids) {
  ids <- as.character(ids[!is.na(ids)])

  brenda <- brenda %>%
    filter(
      .data$field %in% c("RECOMMENDED_NAME","SYSTEMATIC_NAME", "SYNONYMS")
    ) %>%
    mutate(
      description = str_remove(.data$description, "^(RN|SN|SY)\\s+"),
      description = str_replace_all(.data$description, "\nSY\\s+", "\n"),
      description = str_trim(.data$description)
    )

  map_dfr(ids, function(x) {
    brenda %>%
      filter(str_detect(.data$description, str_glue("\\b({x})"))) %>%
      mutate(fromID = x)
  }) %>%
    as_tibble() %>%
    select(ID = .data$fromID, EC = .data$ID, .data$field, .data$description) %>%
    spread(key = "field", value = "description")
}


#' @title Query for a specific enzyme.
#'
#' @description Use a EC number to retrieve information from the BRENDA
#' `tibble` read in by [ReadBrenda()].
#'
#' @param brenda A `tibble` containing information from BRENDA.
#' @param EC A string of the EC number.
#' @param organisms A character vector indicating organisms to keep. Default is
#' FALSE, which would keep all organisms.
#'
#' @return A `brenda.entry` object.
#' @keywords internal
#'
#' @seealso [ReadBrenda()] [InitBrendaEntry()]
#' @examples
#' df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
#'                           package = "brendaDb"))
#' brendaDb:::QueryBrendaBase(brenda = df, EC = "1.1.1.1",
#'                            organisms = "Homo sapiens")
#'
#' @importFrom tibble as_tibble deframe
#' @importFrom dplyr filter select
#' @importFrom rlang .data
#' @importFrom stringr str_glue
QueryBrendaBase <- function(brenda, EC, organisms = FALSE) {
  brenda <- brenda %>%
    filter(.data$ID == EC) %>%
    select(.data$field, .data$description) %>%
    deframe()  # two columns to named vector

  if ("TRANSFERRED_DELETED" %in% names(brenda)) {
    message(str_glue("{EC} was transferred or deleted."))
    query <- InitBrendaDeprecatedEntry(EC, brenda[["TRANSFERRED_DELETED"]])
  } else {
    query <- InitBrendaEntry(
      EC,
      # Nomenclature -----------------------------------------------------------
      protein = ParseProtein(brenda["PROTEIN"]),
      systematic.name = ParseSystematicName(brenda["SYSTEMATIC_NAME"]),
      recommended.name = ParseRecommendedName(brenda["RECOMMENDED_NAME"]),
      synonyms = ParseGeneric(brenda["SYNONYMS"], "SY"),
      reaction = ParseGeneric(brenda["REACTION"], "RE"),
      reaction.type = ParseGeneric(brenda["REACTION_TYPE"], "RT"),

      # Interactions -----------------------------------------------------------
      substrate.product = ParseReaction(brenda["SUBSTRATE_PRODUCT"], "SP"),
      natural.substrate.product = ParseReaction(brenda["NATURAL_SUBSTRATE_PRODUCT"], "NSP"),
      cofactor = ParseGeneric(brenda["COFACTOR"], "CF"),
      metals.ions = ParseGeneric(brenda["METALS_IONS"], "ME"),
      inhibitors = ParseGeneric(brenda["INHIBITORS"], "IN"),
      activating.compound = ParseGeneric(brenda["ACTIVATING_COMPOUND"], "AC"),

      # Parameters -------------------------------------------------------------
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

      # Organism information ---------------------------------------------------
      source.tissue = ParseGeneric(brenda["SOURCE_TISSUE"], "ST"),
      localization = ParseGeneric(brenda["LOCALIZATION"], "LO"),

      # Structure --------------------------------------------------------------
      molecular.weight = ParseGeneric(brenda["MOLECULAR_WEIGHT"], "MW"),
      subunits = ParseGeneric(brenda["SUBUNITS"], "SU"),
      posttranslational.modification = ParseGeneric(brenda["POSTTRANSLATIONAL_MODIFICATION"], "PM"),
      crystallization = ParseNoDescription(brenda["CRYSTALLIZATION"], "CR"),

      # Molecular --------------------------------------------------------------
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

      # Bibliography -----------------------------------------------------------
      bibliography = ParseReference(brenda["REFERENCE"])
    )

    if (is.character(organisms)) {
      org.id <- query$organism$organism %>%
        filter(.data$description %in% organisms) %>%
        select(.data$description, .data$proteinID) %>%
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
#' @keywords internal
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
#' @keywords internal
#'
#' @seealso [QueryBrendaBase()]
#'
#' @importFrom dplyr filter
#' @importFrom rlang .data
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
        map_lgl(str_split(.data$proteinID, ","),
                function(x) any(x %in% org.id) | all(is.na(x)))
      )
  } else {
    res <- query
  }
  return(res)
}


#' @title Extract a specific field from a brenda.entries object.
#'
#' @description Retrieve one field from all the brenda.entry objects. A column
#' of EC numbers will be added to distinguish different enzymes.
#'
#' @param res A brenda.entries object from [QueryBrenda()].
#' @param field A string indicating the field to extract. Nested fields should
#' be separated by `$`, e.g. `parameters$ph.optimum`.
#' @param entries A character vector with values of EC numbers. This should be a
#' subset of `names(res)`.
#'
#' @return A tibble with all columns from the tibble in the given field, and
#' extra columns containing the EC numbers and organisms.
#' @export
#'
#' @import stringr
#' @importFrom purrr map_dfr
#'
#' @examples
#' df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
#'                           package = "brendaDb"))
#' res <- QueryBrenda(brenda = df, EC = c("1.1.1.1", "6.3.5.8"),
#'                    n.core = 2)
#' ExtractField(res, field = "molecular$stability$general.stability")
#' ExtractField(res, field = "structure$subunits")
ExtractField <- function(res, field, entries = NULL) {
  # Sanity checks
  if (missing(field)) {
    stop("Please specify the field you want to extract.")
  }
  if (!str_detect(field, "\\$")) {
    stop("Please check the format of argument 'field'.")
  }
  if (!all(is.brenda.entry(res))) {
    stop("Please check your input res object. It should be from QueryBrenda().")
  }
  # Clean and check the res argument
  if (!is.null(entries)) {
    res <- res[names(res) %in% entries]
  }
  if (any(is.brenda.deprecated.entry(res))) {
    message("Deprecated entries in the res object will be removed.")
    res[is.brenda.deprecated.entry(res)] <- NULL
  }
  field <- str_split(field, "\\$")[[1]]

  map_dfr(res, function(x) {ExtractFieldHelper(x, field)})
}


#' @keywords internal
#'
#' @importFrom dplyr mutate left_join select everything arrange
#' @importFrom tidyr unnest
#' @import stringr
#' @importFrom rlang .data
#' @importFrom purrr map_dfr
ExtractFieldHelper <- function(x, field) {
  # The only case length(field) == 3 is molecular$stability$...
  if (length(field) == 3) {
    if (!field[3] %in% names(x[["molecular"]][["stability"]])) {
      stop("Argument 'field' is invalid.")
    }
    tmp <- x[["molecular"]][["stability"]][[field[3]]]
  } else {
    if ((!field[1] %in% names(x)) | (!field[2] %in% names(x[[field[1]]]))) {
      stop("Argument 'field' is invalid.")
    }
    tmp <- x[[field[1]]][[field[2]]]
  }
  if (is_tibble(tmp) && "proteinID" %in% colnames(tmp)) {
    tmp %>%
      mutate(
        ec = x$nomenclature$ec,
        proteinID = str_split(.data$proteinID, ",")
      ) %>%
      unnest(.data$proteinID) %>%
      left_join(
        x$organism$organism %>%
          select(.data$proteinID, organism = .data$description,
                 .data$uniprot, org.commentary = .data$commentary),
        by = "proteinID"
      ) %>%
      select(.data$ec, .data$organism, .data$proteinID, .data$uniprot,
             .data$org.commentary, everything()) %>%
      arrange(.data$ec, .data$organism,
              as.numeric(.data$proteinID), .data$description)
  } else {
    NULL
  }
}

