#' @title Create a `brendaEntry` object.
#'
#' @description The list should contain 6 sublists: `nomenclature`,
#' `interactions`, `parameters`, `molecular`, `stability`, and `bibliography`.
#' All sublists should be empty apart from the `EC` term in `nomenclature`.
#'
#' @param EC A string indicating EC number of the enzyme.
#'
#'
#' @examples
#' InitBrendaEntry("1.1.1.100")
#'
#' @export
InitBrendaEntry <- function(EC) {
  if (missing(EC)) {
    stop("Missing parameter: EC.")
  }
  x <- structure(list(
    nomenclature = structure(list(
      ec = EC,
      protein = list(),
      systematic.name = NA,
      recommended.name = NA,
      synonyms = list()
    ), class = "brenda.nomenclature"),

    interactions = structure(list(
      reaction = list(),
      reaction.type = list(),
      substrate.product = list(),
      natural.substrate.product = list(),
      activating.compound = list(),
      inhibitors = list(),
      cofactor = list(),
      metals.ions = list()
    ), class = "brenda.interactions"),

    parameters = structure(list(
      km.value = list(),
      turnover.number = list(),
      ki.value = list(),
      pi.values = list(),
      ph.optimum = list(),
      ph.range = list(),
      temperature.optimum = list(),
      temperature.range = list(),
      specific.activity = list()
    ), class = "brenda.parameters"),

    molecular = structure(list(
      cloned = list(),
      purification = list(),
      subunits = list(),
      localization = list(),
      source.tissue = list(),
      molecular.weight = list(),
      posttranslational.modification = list(),
      cystallization = list(),
      renatured = list(),
      engineering = list()
    ), class = "brenda.molecular"),

    stability = structure(list(
      general.stability = list(),
      storage.stability = list(),
      ph.stability = list(),
      organic.solvent.stability = list(),
      oxidation.stability = list(),
      temperature.stability = list()
    ), class = "brenda.stability"),

    bibliography = structure(list(
      reference = list(),
      application = list()
    ), class = "brenda.bibliography")
  ), class = "brenda.entry")
  return(x)
}

#' @rdname InitBrendaEntry
#' @export
is.brenda.entry <- function(x) {
  return(inherits(x, "brenda.entry"))
}