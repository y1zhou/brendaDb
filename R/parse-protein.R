#' @title Parse a "PROTEIN" entry into list of lists.
#'
#' @description Split an entry string into a list of lists. Elements could
#' be accessed with `Find`.
#'
#' @param description The description string in a "PROTEIN" entry.
#'
#' @return A list of lists with sublists having three elements:
#' id, organism and reference.
#'
#' @examples
#' x <- paste0(
#' "PR\t#1# Cavia porcellus  (#1# SULT1A2 <1,2,6,7>) <1,2,6,7>\n",
#' "PR\t#2# Mus musculus <11,18,19>\n")
#' ParseProtein(x)
#'
#' @export
ParseProtein <- function(description) {
  # Separate experiments, and strip unnecessary whitespace -------------------
  x <- strsplit(description, "\nPR\t")[[1]]
  x <- sub("^PR\t", "", trimws(x))
  x <- gsub("\n\t", " ", x)  # some refs will be delimited by " " instead of ,

  # Split protein numbers, organisms and references --------------------------
  protein.num <- sub("^(#\\d+#).*$", "\\1", x)
  protein.num <- sapply(protein.num, function(x)
    ParseProteinNum(x, type = "protein"), USE.NAMES = F)
  # TODO: protein.org string may still contain commentaries wrapped in ()
  protein.org <- trimws(sub("^#\\d+#(.*)<[0-9, ]+>$", "\\1", x))

  ref.num <- sub(".*(<[0-9, ]+>)$", "\\1", x)
  ref.num <- gsub("\\s+", ",", ref.num)
  ref.num <- sapply(ref.num, function(x)
    ParseProteinNum(x, type = "reference"), USE.NAMES = F)

  res <- Map(list, id = protein.num, organism = protein.org,
                    reference = ref.num)
  return(res)
}