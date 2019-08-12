#' @title Show all unique BRENDA fields and their corresponding acronyms.
#'
#' @param df A data.frame with columns "field" and "description"
#'
#' @return A data.frame with columns "field" and "acronym".
#'
#' @importFrom dplyr filter distinct mutate select
#' @importFrom rlang .data
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
    stop(
      "Missing parameter df. If you want all the fields, please use \n",
      "data(acronyms)."
    )
  } else {
    acronyms <- df %>%
      filter(.data$field != "TRANSFERRED_DELETED") %>%
      distinct(.data$field, .keep_all = TRUE) %>%
      mutate(acronym = str_extract(.data$description, "^[A-Z05]+")) %>%
      select(.data$field, .data$acronym) %>%
      as_tibble()
  }
  return(acronyms)
}


#' @title Show the number of regular and transferred/deleted brenda.entry
#' objects in the brenda.entries list.
#'
#' @param x A brenda.entries list returned by [QueryBrenda()].
#' @param verbose Boolean; if TRUE, print tree views of brenda.query objects.
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
#' @param x A brenda.entry object (elements in the list returned by the function
#' [QueryBrenda()]).
#' @param full.output A boolean default to FALSE. If TRUE, include all entries
#' even if they are empty (NA or 0 rows).
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
print.brenda.entry <- function(x, full.output = FALSE, ...) {
  if (inherits(x, "brenda.deprecated.entry")) {
    print(str_glue(
      "Entry {x$nomenclature$ec}\n",
      "\U251C\U2500\U2500 nomenclature\n",
      "|    \U2514\U2500\U2500 ec: {make_style(rgb(0.58, 0.58, 0.58))(x$nomenclature$ec)}\n",
      "\U2514\U2500\U2500 msg: {make_style(rgb(0.28, 0.28, 0.28))(x$msg)}"
    ))
  } else {
    cat("Entry", x$nomenclature$ec)
    invisible(
      pmap(
        list(x = x, i = names(x), tail.i = tail(names(x), 1)),
        function(x, i, tail.i) PrettyPrintBrendaEntry(x, i, tail.i,
                                                      0, full.output)
      )
    )
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
#' @param full.output A boolean default to FALSE. If TRUE, print entry even if
#' it's empty (NA or 0 rows).
#'
#' @return Nothing; prints object tree-view to the terminal.
#' @keywords internal
#'
#' @importFrom crayon make_style red
#' @importFrom grDevices rgb
#' @importFrom utils tail
PrettyPrintBrendaEntry <- function(x, index, tail.idx, depth, full.output) {
  if(inherits(x, "brenda.sublist")) {
    # Always print the top-level entry names
    PrintTreeHelper(index, tail.idx, depth)

    # Drop empty entries by default
    if(!full.output) {
      x <- x[!is.na(x)]
      drop.zero.rows <- map(x, function(x) ifelse(is_tibble(x), nrow(x), 1))
      x <- x[drop.zero.rows > 0]
    }

    if (length(x) > 0) {
      pmap(
        list(x = x, i = names(x), tail.i = tail(names(x), 1)),
        function(x, i, tail.i) PrettyPrintBrendaEntry(x, i, tail.i,
                                                      depth+1, full.output)
      )
    } else {
      cat(":", crayon::red("All NA"))
    }
  } else {
    PrintTreeHelper(index, tail.idx, depth)
    if(is_tibble(x)) {
      if (nrow(x) != 0) {
        cat(":", make_style(rgb(0.58, 0.58, 0.58))(
          "A tibble with", nrow(x), "rows"))
      } else {
        cat(":", make_style(rgb(0.58, 0.58, 0.58))(
          "A tibble with", crayon::red("0"), "rows"))
      }
    } else if (is.na(x)) {
      cat(":", crayon::red("NA"))
    } else {
      cat(":", make_style(rgb(0.28, 0.28, 0.28))(x))
    }
  }
}


#' @title Print the tree structure with correct whitespace.
#'
#' @param index A string of the name of the sublist.
#' @param tail.idx A string showing the last element in the entry. This is for
#' printing a different character in the tree.
#' @param depth Int, showing the depth of the element.
#'
#' @return Nothing; prints tree structure to the terminal.
#' @keywords internal
PrintTreeHelper <- function(index, tail.idx, depth) {
  if(index == tail.idx) {
    cat("\n", rep("|    ", depth), "\U2514\U2500\U2500 ", index, sep = "")
  } else {
    cat("\n", rep("|    ", depth), "\U251C\U2500\U2500 ", index, sep = "")
  }
}
