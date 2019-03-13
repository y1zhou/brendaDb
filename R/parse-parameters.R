#' @title Parse a "KM_VALUE" entry.
#'
#' @description Expand the string into a `data.table`.
#'
#' @param description The description string in a "KM_VALUE" entry.
#'
#' @return A `data.table` with columns: proteinID, description, fieldInfo,
#' commentary and refID. The description column is the extracted Km value,
#' and the fieldInfo column is the corresponding substrate.
#'
#' @examples
#' x <- paste0(
#' "KM\t#1# -999 {more}  (#1# in phosphate buffer, enzyme shows\n\t",
#' "marked cooperativity with respect to NAD+ binding. <15>) <15>\n",
#' "KM\t#1# 0.045 {GDP-D-mannose} (#1# pH 8.0, 5°C, recombinant mutant C268A\n\t",
#' "<17>) <17>\n",
#' "KM\t#1# 0.1 {NAD+(test)}  (#1# pH 8.0, phosphate buffer <15>) <15>\n"
#' )
#' brendaDb:::ParseKmValue(x)
ParseKmValue <- function(description) {
  x <- SeparateSubentries(description, acronym = "KM")
  res <- ParseGeneric(x)
  res$description[res$description == "-999"] <- "additional_information"
  return(res)
}


#' @title Parse a "PH_OPTIMUM" entry.
#'
#' @description Expand the string into a `data.table`.
#'
#' @param description The description string in a "PH_OPTIMUM" entry.
#'
#' @return A `data.table` with columns: proteinID, description, fieldInfo,
#' commentary and refID. The description column is the extracted pH optimum
#' value/range, and the fieldInfo column should all be NAs.
#'
#' @examples
#' x <- paste0(
#' "PHO\t#10# 8.3 (#10# alcohol dehydrogenase IV <87>) <87>\n",
#' "PHO\t#9,68,78,132# 11 (#78# oxidation of ethanol, pyrazole-insensitive\n\t",
#' "enzyme <24>; #68# ethanol oxidation, enzyme form ADH-2 and ADH-3 <60>;\n\t",
#' "#9# oxidation of octanol <49>; #132# optimally active with ethanol and\n\t",
#' "1-propanol at pH 11.0 with 3 M KCl <237>) <24,49,60,237>\n")
#' brendaDb:::ParsePhOptimum(x)
ParsePhOptimum <- function(description) {
  x <- SeparateSubentries(description, acronym = "PHO")
  res <- ParseGeneric(x)

  # https://www.brenda-enzymes.org/enzyme.php?ecno=1.1.1.100&organism%5B%5D=Mycobacterium+tuberculosis#pH%20OPTIMUM
  res$description[res$description == "-999"] <- "additional_information"
  return(res)
}


#' @title Parse a "PH_RANGE" entry.
#'
#' @description Expand the string into a `data.table`.
#'
#' @param description The description string in a "PH_RANGE" entry.
#'
#' @return A `data.table` with columns: proteinID, description, fieldInfo,
#' commentary and refID. The description column is the extracted pH range,
#' and the fieldInfo should be all NAs.
ParsePhRange <- function(description) {
  x <- SeparateSubentries(description, acronym = "PHR")
  res <- ParseGeneric(x)
  res$description[res$description == "-999"] <- "additional_information"
  # TODO: parse commentary text and extract a finer pH range profile.
  return(res)
}
