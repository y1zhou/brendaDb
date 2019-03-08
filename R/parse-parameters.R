#' @title Parse a "PH_OPTIMUM" entry.
#'
#' @description Expand the string into a `data.table`.
#'
#' @param description The description string in a "PH_OPTIMUM" entry.
#'
#' @return A `data.table` with four columns:
#' id, pH_optimum, commentary and references.
#'
#' @examples
#' x <- paste0(
#' "PHO\t#10# 8.3 (#10# alcohol dehydrogenase IV <87>) <87>\n",
#' "PHO\t#9,68,78,132# 11 (#78# oxidation of ethanol, pyrazole-insensitive\n\t",
#' "enzyme <24>; #68# ethanol oxidation, enzyme form ADH-2 and ADH-3 <60>;\n\t",
#' "#9# oxidation of octanol <49>; #132# optimally active with ethanol and\n\t",
#' "1-propanol at pH 11.0 with 3 M KCl <237>) <24,49,60,237>\n")
#' brendaDb:::ParsePhOptimum(x)
#'
#' @import stringr
#' @importFrom data.table data.table
ParsePhOptimum <- function(description) {
  x <- SeparateSubentries(description, acronym = "PHO")

  protein.num <- str_extract(x, "^#[0-9,]+#")
  protein.num <- lapply(protein.num, function(x)
    ParseProteinNum(x, type = "protein"))

  ref.num <- str_extract(x, "<[0-9, ]+>$")
  ref.num <- lapply(ref.num, function(x)
    ParseProteinNum(x, type = "reference"))

  ph.optimum <- x %>%
    str_remove_all("(^#[0-9,]+#)|(<[0-9, ]+>$)") %>%
    str_trim()

  commentary <- ph.optimum %>%
    str_extract("\\(.*\\)") %>%
    str_sub(2, -2)

  ph.optimum  <- ph.optimum %>%
    str_remove("\\(.*\\)") %>% # remove all commentaries
    str_trim()

  # https://www.brenda-enzymes.org/enzyme.php?ecno=1.1.1.100&organism%5B%5D=Mycobacterium+tuberculosis#pH%20OPTIMUM
  ph.optimum[ph.optimum == "-999"] <- "additional_information"

  res <- data.table(id = protein.num,
                    pH_optimum = ph.optimum,
                    commentary = commentary,
                    references = ref.num)
  return(res)
}


#' @title Parse a "PH_RANGE" entry.
#'
#' @description Expand the string into a `data.table`.
#'
#' @param description The description string in a "PH_RANGE" entry.
#'
#' @return A `data.table` with four columns:
#' id, pH_range, commentary and references.
#'
#' @import stringr
#' @importFrom data.table data.table
ParsePhRange <- function(description) {
  x <- SeparateSubentries(description, acronym = "PHR")

  protein.num <- str_extract(x, "^#[0-9,]+#")
  protein.num <- lapply(protein.num, function(x)
    ParseProteinNum(x, type = "protein"))

  ref.num <- str_extract(x, "<[0-9, ]+>$")
  ref.num <- lapply(ref.num, function(x)
    ParseProteinNum(x, type = "reference"))

  ph.range <- x %>%
    str_remove_all("(^#[0-9,]+#)|(<[0-9, ]+>$)") %>%
    str_trim()

  commentary <- ph.range %>%
    str_extract("\\(.*\\)") %>%
    str_sub(2, -2)

  ph.range  <- ph.range %>%
    str_remove("\\(.*\\)") %>% # remove all commentaries
    str_trim()
  ph.range[ph.range == "-999"] <- "additional_information"

  res <- data.table(id = protein.num,
                    pH_range = ph.range,
                    commentary = commentary,
                    references = ref.num)
  return(res)
}
