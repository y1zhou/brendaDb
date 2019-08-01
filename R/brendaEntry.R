#' @title Create a `brenda.entry` object.
#'
#' @description The list should contain 6 sublists: `nomenclature`,
#' `interactions`, `parameters`, `molecular`, `stability`, and `bibliography`.
#' All sublists should be empty by default apart from `EC` under `nomenclature`.
#'
#' @param EC A string indicating EC number of the enzyme.
#'
#' @param protein The description string of a PR field.
#' @param systematic.name The description string of a SN field.
#' @param recommended.name The description string of a RN field.
#' @param synonyms The description string of a SY field.
#' @param reaction The description string of a RE field.
#' @param reaction.type The description string of a RT field.
#'
#' @param substrate.product The description string of a SP field.
#' @param natural.substrate.product The description string of a NSP field.
#' @param cofactor The description string of a CF field.
#' @param metals.ions The description string of a ME field.
#' @param inhibitors The description string of a IN field.
#' @param activating.compound The description string of a AC field.
#'
#' @param km.value The description string of a KM field.
#' @param turnover.number The description string of a TN field.
#' @param ki.value The description string of a KI field.
#' @param pi.value The description string of a PI field.
#' @param ph.optimum The description string of a PHO field.
#' @param ph.range The description string of a PHR field.
#' @param temperature.optimum The description string of a TO field.
#' @param temperature.range The description string of a TR field.
#' @param specific.activity The description string of a SA field.
#' @param ic50 The description string of a IC50 field.
#'
#' @param source.tissue The description string of a ST field.
#' @param localization The description string of a LO field.
#'
#' @param general.stability The description string of a GS field.
#' @param storage.stability The description string of a SS field.
#' @param ph.stability The description string of a PHS field.
#' @param organic.solvent.stability The description string of a OSS field.
#' @param oxidation.stability The description string of a OS field.
#' @param temperature.stability The description string of a TS field.
#'
#' @param purification The description string of a PU field.
#' @param cloned The description string of a CL field.
#' @param engineering The description string of a EN field.
#' @param renatured The description string of a REN field.
#' @param application The description string of a AP field.
#'
#' @param molecular.weight The description string of a MW field.
#' @param subunits The description string of a SU field.
#' @param posttranslational.modification The description string of a PM field.
#' @param crystallization The description string of a CR field.
#'
#' @param bibliography The description string of a RF field.
#'
#' @return A `brenda.entry` object with all fields other than nomenclature$ec
#' being NA.
#'
#' @examples
#' brendaDb:::InitBrendaEntry("1.1.1.100")
InitBrendaEntry <- function(EC, protein = NA, systematic.name = NA,
                            recommended.name = NA, synonyms = NA, reaction = NA,
                            reaction.type = NA, substrate.product = NA,
                            natural.substrate.product = NA, cofactor = NA,
                            metals.ions = NA, inhibitors = NA,
                            activating.compound = NA, km.value = NA,
                            turnover.number = NA, ki.value = NA, pi.value = NA,
                            ph.optimum = NA, ph.range = NA,
                            temperature.optimum = NA, temperature.range = NA,
                            specific.activity = NA, ic50 = NA,
                            source.tissue = NA, localization = NA,
                            general.stability = NA, storage.stability = NA,
                            ph.stability = NA, organic.solvent.stability = NA,
                            oxidation.stability = NA,
                            temperature.stability = NA, purification = NA,
                            cloned = NA, engineering = NA, renatured = NA,
                            application = NA, molecular.weight = NA,
                            subunits = NA, posttranslational.modification = NA,
                            crystallization = NA, bibliography = NA) {

  if (missing(EC)) {
    stop("Missing parameter: EC.")
  }
  x <- structure(
    list(
      nomenclature = structure(
        list(
          ec = EC,
          systematic.name = systematic.name,
          recommended.name = recommended.name,
          synonyms = synonyms,
          reaction = reaction,
          reaction.type = reaction.type
        ),
        class = "brenda.sublist"
      ),

      interactions = structure(
        list(
          substrate.product = substrate.product,
          natural.substrate.product = natural.substrate.product,
          cofactor = cofactor,
          metals.ions = metals.ions,
          inhibitors = inhibitors,
          activating.compound = activating.compound
        ),
        class = "brenda.sublist"
      ),

      parameters = structure(
        list(
          km.value = km.value,
          turnover.number = turnover.number,
          ki.value = ki.value,
          pi.value = pi.value,
          ph.optimum = ph.optimum,
          ph.range = ph.range,
          temperature.optimum = temperature.optimum,
          temperature.range = temperature.range,
          specific.activity = specific.activity,
          ic50 = ic50
        ),
        class = "brenda.sublist"
      ),

      organism = structure(
        list(
          organism = protein,
          source.tissue = source.tissue,
          localization = localization
        ),
        class = "brenda.sublist"
      ),

      molecular = structure(
        list(
          stability = structure(
            list(
              general.stability = general.stability,
              storage.stability = storage.stability,
              ph.stability = ph.stability,
              organic.solvent.stability = organic.solvent.stability,
              oxidation.stability = oxidation.stability,
              temperature.stability = temperature.stability
            ),
            class = "brenda.sublist"
          ),
          purification = purification,
          cloned = cloned,
          engineering = engineering,
          renatured = renatured,
          application = application
        ),
        class = "brenda.sublist"
      ),

      structure = structure(
        list(
          molecular.weight = molecular.weight,
          subunits = subunits,
          posttranslational.modification = posttranslational.modification,
          crystallization = crystallization
        ),
        class = "brenda.sublist"
      ),

      bibliography = structure(
        list(
          reference = bibliography
        ),
        class = "brenda.sublist")
    ),
    class = "brenda.entry"
  )
  return(x)
}


#' @title Create a `brenda.deprecated.entry` object.
#'
#' @description Some EC numbers are transferred or deleted. For these entries,
#' return a `brenda.deprecated.entry` object and the corresponding message.
#'
#' @param EC A string indicating EC number of the enzyme.
#' @param msg A string of the transferred / deleted message.
#'
#' @return A `brenda.deprecated.entry` obeject.
#'
#' @examples
#' brendaDb:::InitBrendaDeprecatedEntry("6.3.5.8", "Transferred to EC 2.6.1.85")
InitBrendaDeprecatedEntry <- function(EC, msg) {
  return(structure(
    list(
      nomenclature = list(
        ec = EC
      ),
      msg = msg
    ),
    class = c("brenda.deprecated.entry", "brenda.entry")
  ))
}


#' @rdname InitBrendaEntry
#' @param x Any object.
#' @importFrom purrr map_lgl
#' @export
is.brenda.entry <- function(x) {
  if (inherits(x, "brenda.entries")) {
    res <- map_lgl(x, function(x) inherits(x, "brenda.entry"))
    if (all(res)) {
      message("You might need \"is.brenda.deprecated.entry()\" ",
              "to check for transferred or deleted entries.")
    }
  } else {
    res <- inherits(x, "brenda.entry")
  }
  return(res)
}


#' @rdname InitBrendaDeprecatedEntry
#' @param x Any object.
#' @importFrom purrr map_lgl
#' @export
is.brenda.deprecated.entry <- function(x) {
  if (inherits(x, "brenda.entries")) {
    return(map_lgl(x, function(x) inherits(x, "brenda.deprecated.entry")))
  } else {
    return(inherits(x, "brenda.deprecated.entry"))
  }
}
