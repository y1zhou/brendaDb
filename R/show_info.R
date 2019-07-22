#' @title Show all unique BRENDA fields and their corresponding acronyms.
#'
#' @param df A data.frame with columns "field" and "description"
#'
#' @return A data.frame with columns "field" and "acronym".
#'
#' @importFrom dplyr filter distinct mutate select
#' @import stringr
#' @importFrom tibble as_tibble
#'
#' @examples
#' df <- ReadBrenda(system.file("extdata", "brenda_download_test.txt",
#'                           package = "brendaDb"))
#' ShowFields(df)
#'
#' @export
ShowFields <- function(df) {
  if (missing(df)) {
    stop("Missing parameter df. If you want all the fields, please use data(acronyms).")
  } else {
    acronyms <- df %>%
      filter(field != "TRANSFERRED_DELETED") %>%
      distinct(field, .keep_all = TRUE) %>%
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
#' @param verbose Boolean; if TRUE, print tree views of each brenda.query object.
#' @param ... Other arguments passed to the generic function.
#'
#' @return Nothing; print summary information to the terminal.
#'
#' @importFrom purrr map
#' @export
print.brenda.entries <- function(x, ..., verbose = FALSE) {
  cat(
    "A list of", length(x), "brenda.entry object(s) with:\n",
    "-", length(x[!is.brenda.deprecated.entry(x)]),
    "regular brenda.entry object(s)\n  ",
    paste(names(x)[!is.brenda.deprecated.entry(x)], collapse = ", "),
    "\n-", length(x[is.brenda.deprecated.entry(x)]),
    "transferred or deleted object(s)\n  ",
    paste(names(x[is.brenda.deprecated.entry((x))]), collapse = ", "),
    "\n"
    )

  if (verbose) {
    invisible(map(x, print))
  }
}


#' @title Show the non-empty fields in the query result.
#'
#' @description For details, see [PrettyPrintBrendaEntry()].
#'
#' @param x A brenda.entry object (elements in the list returned by [QueryBrenda()]).
#' @param ... Other arguments passed to the generic function.
#'
#' @return Nothing; print object information to the terminal.
#'
#' @import stringr
#' @importFrom tibble is_tibble
#' @importFrom purrr pmap
#' @importFrom crayon make_style
#'
#' @export
print.brenda.entry <- function(x, ...) {
  if (inherits(x, "brenda.deprecated.entry")) {
    print(str_glue(
      "Entry {x$nomenclature$ec}\n",
      "\U251C\U2500\U2500 nomenclature\n",
      "|    \U2514\U2500\U2500 ec: {make_style(rgb(0.58, 0.58, 0.58))(x$nomenclature$ec)}\n",
      "\U2514\U2500\U2500 msg: {make_style(rgb(0.28, 0.28, 0.28))(x$msg)}"
    ))
  } else {
    cat("Entry", x$nomenclature$ec)
    invisible(pmap(list(x = x, i = names(x), tail.i = tail(names(x), 1)),
         function(x, i, tail.i) PrettyPrintBrendaEntry(x, i, tail.i, 0)))
    cat("\n")
  }
}

#' @title Print a brenda.entry in a tree view.
#'
#' @param x A `brenda.entry` object.
#' @param index A string of the name of the sublist.
#' @param tail.idx A string showing the last element in the entry. This is for
#' printing a different character in the tree.
#' @param depth Int, showing the depth of the element.
#'
#' @return Nothing; prints object tree-view to the terminal.
#'
#' @importFrom crayon make_style red
#' @importFrom grDevices rgb
#' @importFrom utils tail
PrettyPrintBrendaEntry <- function(x, index, tail.idx, depth) {
  if(index == tail.idx) {
    cat("\n", rep("|    ", depth), "\U2514\U2500\U2500 ", index, sep = "")
  } else {
    cat("\n", rep("|    ", depth), "\U251C\U2500\U2500 ", index, sep = "")
  }

  if(inherits(x, "brenda.sublist")) {
    pmap(list(x = x, i = names(x), tail.i = tail(names(x), 1)),
         function(x, i, tail.i) PrettyPrintBrendaEntry(x, i, tail.i, depth+1))
  } else if(is_tibble(x)) {
    if (nrow(x) == 0) {
      cat(":", make_style(rgb(0.58, 0.58, 0.58))(
        "A tibble with", crayon::red("0"), "rows"))
    } else {
      cat(":", make_style(rgb(0.58, 0.58, 0.58))(
        "A tibble with", nrow(x), "rows"))
    }
  } else if (is.na(x)) {
    cat(":", crayon::red("NA"))
  } else {
    cat(":", make_style(rgb(0.28, 0.28, 0.28))(x))
  }
}

