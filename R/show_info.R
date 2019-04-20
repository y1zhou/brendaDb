#' @title Show all unique BRENDA fields and their corresponding acronyms.
#'
#' @param df A data.frame with columns "field" and "description"
#'
#' @return A data.frame with columns "field" and "acronym".
#'
#' @importFrom dplyr filter distinct mutate select
#' @import stringr
#' @importFrom tibble as_tibble
#' @export
ShowFields <- function(df) {
  if (missing(df)) {
    stop("Missing parameter df. If you want all the fields, please use data(acronyms).")
  } else {
    acronyms <- df %>%
      filter(field != "TRANSFERRED_DELETED") %>%
      distinct(field, .keep_all = T) %>%
      mutate(acronym = str_extract(description, "^[A-Z05]+")) %>%
      select(field, acronym) %>%
      as_tibble()
  }
  return(acronyms)
}


#' @title Show the number of regular and transferred/deleted brenda.entry objects
#' in the brenda.entries list.
#'
#' @param x A brenda.entries list returned by [QueryBrenda()].
#'
#' @import stringr
#' @importFrom purrr map
#' @export
summary.brenda.entries <- function(x, verbose = F) {
  print(str_glue(
    "A list of {length(x)} brenda.entry object(s) with:\n",
    "- {length(x[!is.brenda.deprecated.entry(x)])} regular brenda.entry object(s)\n  ",
    paste(names(x)[!is.brenda.deprecated.entry(x)], collapse = ", "),
    "\n- {length(x[is.brenda.deprecated.entry(x)])} transferred or deleted object(s)\n  ",
    paste(names(x[is.brenda.deprecated.entry((x))]), collapse = ", ")))

  if (verbose) {
    invisible(map(x, summary))
  }
}


#' @title Show the non-empty fields in the query result.
#'
#' @param x A brenda.entry object (elements in the list returned by [QueryBrenda()]).
#'
#' @import stringr
#' @importFrom tibble is_tibble
#' @importFrom purrr pmap
#' @export
summary.brenda.entry <- function(x) {
  if (inherits(x, "brenda.deprecated.entry")) {
    print(str_glue(
      "Entry {x$nomenclature$ec}\n",
      "├── nomenclature\n",
      "|    └── ec: {make_style(rgb(0.58, 0.58, 0.58))(x$nomenclature$ec)}\n",
      "└── msg: {make_style(rgb(0.58, 0.58, 0.58))(x$msg)}"
    ))
  } else {
    cat("Entry", x$nomenclature$ec)
    invisible(pmap(list(x = x, i = names(x), tail.i = tail(names(x), 1)),
         function(x, i, tail.i) PrettyPrintBrendaEntry(x, i, tail.i, 0)))
    cat("\n")
  }
}

#' @importFrom crayon make_style red
PrettyPrintBrendaEntry <- function(x, index, tail.idx, depth) {
  if(index == tail.idx) {
    cat("\n", rep("|    ", depth), "└── ", index, sep = "")
  } else {
    cat("\n", rep("|    ", depth), "├── ", index, sep = "")
  }

  if(inherits(x, "brenda.sublist")) {
    pmap(list(x = x, i = names(x), tail.i = tail(names(x), 1)),
         function(x, i, tail.i) PrettyPrintBrendaEntry(x, i, tail.i, depth+1))
  } else if(is_tibble(x)) {
    cat(":", make_style(rgb(0.58, 0.58, 0.58))("A tibble with", nrow(x), "rows"))
  } else if (is.na(x)) {
    cat(":", crayon::red("NA"))
  } else {
    cat(":", make_style(rgb(0.58, 0.58, 0.58))(x))
  }
}

