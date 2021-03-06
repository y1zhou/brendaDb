% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brendaEntry.R
\name{InitBrendaEntry}
\alias{InitBrendaEntry}
\alias{is.brenda.entry}
\title{Create a \code{brenda.entry} object.}
\usage{
InitBrendaEntry(
  EC,
  protein = NA,
  systematic.name = NA,
  recommended.name = NA,
  synonyms = NA,
  reaction = NA,
  reaction.type = NA,
  substrate.product = NA,
  natural.substrate.product = NA,
  cofactor = NA,
  metals.ions = NA,
  inhibitors = NA,
  activating.compound = NA,
  km.value = NA,
  turnover.number = NA,
  ki.value = NA,
  pi.value = NA,
  ph.optimum = NA,
  ph.range = NA,
  temperature.optimum = NA,
  temperature.range = NA,
  specific.activity = NA,
  ic50 = NA,
  source.tissue = NA,
  localization = NA,
  general.stability = NA,
  storage.stability = NA,
  ph.stability = NA,
  organic.solvent.stability = NA,
  oxidation.stability = NA,
  temperature.stability = NA,
  purification = NA,
  cloned = NA,
  engineering = NA,
  renatured = NA,
  application = NA,
  molecular.weight = NA,
  subunits = NA,
  posttranslational.modification = NA,
  crystallization = NA,
  bibliography = NA
)

is.brenda.entry(x, verbose = FALSE)
}
\arguments{
\item{EC}{A string indicating EC number of the enzyme.}

\item{protein}{The description string of a PR field.}

\item{systematic.name}{The description string of a SN field.}

\item{recommended.name}{The description string of a RN field.}

\item{synonyms}{The description string of a SY field.}

\item{reaction}{The description string of a RE field.}

\item{reaction.type}{The description string of a RT field.}

\item{substrate.product}{The description string of a SP field.}

\item{natural.substrate.product}{The description string of a NSP field.}

\item{cofactor}{The description string of a CF field.}

\item{metals.ions}{The description string of a ME field.}

\item{inhibitors}{The description string of a IN field.}

\item{activating.compound}{The description string of a AC field.}

\item{km.value}{The description string of a KM field.}

\item{turnover.number}{The description string of a TN field.}

\item{ki.value}{The description string of a KI field.}

\item{pi.value}{The description string of a PI field.}

\item{ph.optimum}{The description string of a PHO field.}

\item{ph.range}{The description string of a PHR field.}

\item{temperature.optimum}{The description string of a TO field.}

\item{temperature.range}{The description string of a TR field.}

\item{specific.activity}{The description string of a SA field.}

\item{ic50}{The description string of a IC50 field.}

\item{source.tissue}{The description string of a ST field.}

\item{localization}{The description string of a LO field.}

\item{general.stability}{The description string of a GS field.}

\item{storage.stability}{The description string of a SS field.}

\item{ph.stability}{The description string of a PHS field.}

\item{organic.solvent.stability}{The description string of a OSS field.}

\item{oxidation.stability}{The description string of a OS field.}

\item{temperature.stability}{The description string of a TS field.}

\item{purification}{The description string of a PU field.}

\item{cloned}{The description string of a CL field.}

\item{engineering}{The description string of a EN field.}

\item{renatured}{The description string of a REN field.}

\item{application}{The description string of a AP field.}

\item{molecular.weight}{The description string of a MW field.}

\item{subunits}{The description string of a SU field.}

\item{posttranslational.modification}{The description string of a PM field.}

\item{crystallization}{The description string of a CR field.}

\item{bibliography}{The description string of a RF field.}

\item{x}{Any object.}

\item{verbose}{Boolean value default to FALSE. If TRUE, prints message when
all elements are brenda.entries.}
}
\value{
A \code{brenda.entry} object with all fields other than nomenclature$ec
being NA.

A boolean vector of the same length as \code{x}.
}
\description{
The list should contain 6 sublists: \code{nomenclature},
\code{interactions}, \code{parameters}, \code{molecular}, \code{stability}, and \code{bibliography}.
All sublists should be empty by default apart from \code{EC} under \code{nomenclature}.
}
\details{
\code{is.brenda.entry} checks if an object is a \code{brenda.entry} object. If
the input is a \code{brenda.entries} object, check all items in the list to see if
they are \code{brenda.entry} objects and return a boolean vector of the same
length.
}
\examples{
brendaDb:::InitBrendaEntry("1.1.1.100")
df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
                          package = "brendaDb"))
is.brenda.entry(QueryBrenda(df, "6.3.5.8"))

}
\keyword{internal}
