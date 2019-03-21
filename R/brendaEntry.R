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
InitBrendaEntry <- function(EC) {
  if (missing(EC)) {
    stop("Missing parameter: EC.")
  }
  x <- structure(
    list(
      nomenclature = structure(
        list(
          ec = EC,
          protein = NA,
          systematic.name = NA,
          recommended.name = NA,
          synonyms = NA,
          reaction = NA,
          reaction.type = NA
        ),
        class = "brenda.nomenclature"
      ),

      interactions = structure(
        list(
          substrate.product = NA,
          natural.substrate.product = NA,
          cofactor = NA,
          metals.ions = NA,
          inhibitors = NA,
          activating.compound = NA
        ),
        class = "brenda.interactions"
      ),

      parameters = structure(
        list(
          km.value = NA,
          turnover.number = NA,
          ki.value = NA,
          pi.value = NA,
          ph.optimum = NA,
          ph.range = NA,
          temperature.optimum = NA,
          temperature.range = NA,
          specific.activity = NA,
          ic50 = NA
        ),
        class = "brenda.parameters"
      ),

      organism = structure(
        list(
          source.tissue = NA,
          localization = NA

        ),
        class = "brenda.organism"
      ),

      molecular = structure(
        list(
          stability = structure(
            list(
              general.stability = NA,
              storage.stability = NA,
              ph.stability = NA,
              organic.solvent.stability = NA,
              oxidation.stability = NA,
              temperature.stability = NA
            ),
            class = "brenda.stability"
          ),
          purification = NA,
          cloned = NA,
          engineering = NA,
          renatured = NA,
          application = NA
        ),
        class = "brenda.molecular"
      ),

      structure = structure(
        list(
          molecular.weight = NA,
          subunits = NA,
          posttranslational.modification = NA,
          cystallization = NA
        ),
        class = "brenda.structure"
      ),

      bibliography = structure(
        NA,
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
