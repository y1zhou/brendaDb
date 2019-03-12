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
          synonyms = NA
        ),
        class = "brenda.nomenclature"
      ),

      interactions = structure(
        list(
          reaction = NA,
          reaction.type = NA,
          substrate.product = NA,
          natural.substrate.product = NA,
          activating.compound = NA,
          inhibitors = NA,
          cofactor = NA,
          metals.ions = NA
        ),
        class = "brenda.interactions"
      ),

      parameters = structure(
        list(
          km.value = NA,
          turnover.number = NA,
          ki.value = NA,
          pi.values = NA,
          ph.optimum = NA,
          ph.range = NA,
          temperature.optimum = NA,
          temperature.range = NA,
          specific.activity = NA
        ),
        class = "brenda.parameters"
      ),

      molecular = structure(
        list(
          cloned = NA,
          purification = NA,
          subunits = NA,
          localization = NA,
          source.tissue = NA,
          molecular.weight = NA,
          posttranslational.modification = NA,
          cystallization = NA,
          renatured = NA,
          engineering = NA
        ),
        class = "brenda.molecular"
      ),

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

      bibliography = structure(list(
        reference = NA,
        application = NA
      ), class = "brenda.bibliography")
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
