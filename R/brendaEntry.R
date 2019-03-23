#' @title Create a `brendaEntry` object.
#'
#' @description The list should contain 6 sublists: `nomenclature`,
#' `interactions`, `parameters`, `molecular`, `stability`, and `bibliography`.
#' All sublists should be empty apart from the `EC` term in `nomenclature`.
#'
#' @param EC A string indicating EC number of the enzyme.
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
                            ph.optimum = NA, ph.range = NA, temperature.optimum = NA,
                            temperature.range = NA, specific.activity = NA,
                            ic50 = NA, source.tissue = NA, localization = NA,
                            general.stability = NA, storage.stability = NA,
                            ph.stability = NA, organic.solvent.stability = NA,
                            oxidation.stability = NA, temperature.stability = NA,
                            purification = NA, cloned = NA, engineering = NA,
                            renatured = NA, application = NA, molecular.weight = NA,
                            subunits = NA, posttranslational.modification = NA,
                            cystallization = NA, bibliography = NA) {
  if (missing(EC)) {
    stop("Missing parameter: EC.")
  }
  x <- structure(
    list(
      nomenclature = structure(
        list(
          ec = EC,
          protein = protein,
          systematic.name = systematic.name,
          recommended.name = recommended.name,
          synonyms = synonyms,
          reaction = reaction,
          reaction.type = reaction.type
        ),
        class = "brenda.nomenclature"
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
        class = "brenda.interactions"
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
        class = "brenda.parameters"
      ),

      organism = structure(
        list(
          source.tissue = source.tissue,
          localization = localization
        ),
        class = "brenda.organism"
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
            class = "brenda.stability"
          ),
          purification = purification,
          cloned = cloned,
          engineering = engineering,
          renatured = renatured,
          application = application
        ),
        class = "brenda.molecular"
      ),

      structure = structure(
        list(
          molecular.weight = molecular.weight,
          subunits = subunits,
          posttranslational.modification = posttranslational.modification,
          cystallization = cystallization
        ),
        class = "brenda.structure"
      ),

      bibliography = structure(
        bibliography,
        class = "brenda.bibliography")
    ),
    class = "brenda.entry"
  )
  return(x)
}

#' @rdname InitBrendaEntry
#' @param x Any object.
#' @export
is.brenda.entry <- function(x) {
  return(inherits(x, "brenda.entry"))
}
