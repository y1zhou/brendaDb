#' @title Get all EC numbers involved in a BioCyc pathway.
#'
#' @param org.id The identifier for the organism database in BioCyc, e.g. ECOLI,
#' HUMAN, META, AFER243159
#' @param pathway A case-sensitive pathway object identifier,
#' e.g. PWY66-400, LYSINE-DEG1-PWY.
#' @param sleep Optional, default is 0. If set to a non-zero value, pause for
#' the specified seconds after retrieving each reaction. Increase this if the
#' default fails (probably limited on the BioCyc API side).
#'
#' @return If the pathway is found, returns a tibble with columns
#'     - `RxnID`: reaction IDs found in the pathway.
#'     - `EC`: EC number of the enzyme catalyzing the reaction.
#'     - `ReactionDirection`: direction of the reaction.
#'     - `LHS`: left-hand-side of the reaction.
#'     - `RHS`: right-hand-side of the reaction.
#' Different compounds in the reactions are separated by " + " in columns `LHS`
#' and `RHS`. The function returns NULL if the pathway ID is not found.
#' @export
#'
#' @examples \donttest{BiocycPathwayEnzymes("HUMAN", "PWY66-400")}
#'
#' @import stringr
#' @importFrom curl curl
#' @importFrom xml2 read_xml xml_find_all xml_text xml_remove
#' @importFrom dplyr progress_estimated
#' @importFrom purrr map
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom rlang .data
BiocycPathwayEnzymes <- function(org.id = "HUMAN", pathway, sleep = 0) {
  if (missing(pathway) | is.na(pathway)) {
    stop("Missing parameter \"pathway\"")
  }
  # https://biocyc.org/web-services.shtml#R2.1
  # First use this API to get all reactions in the pathway
  reaction.ids <- tryCatch({
    con <- curl::curl(str_glue(
      "https://websvc.biocyc.org/getxml?{org.id}:{pathway}"))
    con %>%
      read_xml() %>%
      xml_find_all("//Pathway/reaction-list/Reaction/@frameid") %>%
      xml_text()
  },
  error = function(e) {
    close(con)
    message(str_glue(
      "Message from curl: {e}Check if the pathway ID is correct."
    ))
    return(NA)
  })
  if (all(is.na(reaction.ids))) {
    return(invisible(NULL))
  }

  message(str_glue(
    "Found {length(reaction.ids)} reactions for {org.id} pathway {pathway}."
  ))
  # For each reaction, get the EC number(s) of the enzyme catalyzing it
  pb <- progress_estimated(length(reaction.ids))
  res <- map_dfr(reaction.ids, function(rxn) {
    pb$tick()$print()
    # Get EC number of the enzyme, and metabolites in the reaction
    x <- read_xml(str_glue("https://websvc.biocyc.org/getxml?{org.id}:{rxn}"))

    ec.number <- x %>%
      xml_find_all("//ec-number")

    # There's an "official" node inside "ec-number" that is useless in this case
    ec.number %>%
      xml_find_all("//official") %>%
      xml_remove()
    ec.number <- ec.number %>%
      xml_text() %>%
      str_trim() %>%
      str_remove("^EC-")

    rxn.direction <- x %>%
      xml_find_all("//Reaction/reaction-direction") %>%
      xml_text()

    lhs <- x %>%
      xml_find_all("//Reaction/left/Compound/@frameid") %>%
      xml_text() %>%
      paste(collapse = " + ")
    # TODO: //reaction/left/coefficient
    rhs <- x %>%
      xml_find_all("//Reaction/right/Compound/@frameid") %>%
      xml_text() %>%
      paste(collapse = " + ")

    Sys.sleep(sleep)

    tibble(
      RxnID = rxn,
      EC = ec.number,
      ReactionDirection = rxn.direction,
      LHS = lhs,
      RHS = rhs
    )
  })

  res
}

#' @title Get all genes involved in a BioCyc pathway.
#'
#' @inheritParams BiocycPathwayEnzymes
#'
#' @return If the pathway is found, returns a tibble with columns `BiocycGene`,
#' `BiocycProtein`, `Symbol` and `Ensembl`, where `BiocycGene` and
#' `BiocycProtein` are the gene and protein IDs in the BioCyc database,
#' respectively. Returns NULL if the pathway ID is not found.
#' @export
#'
#' @examples \donttest{BiocycPathwayGenes("HUMAN", "PWY66-400")}
#' \donttest{BiocycPathwayGenes("HUMAN", "TRYPTOPHAN-DEGRADATION-1")}
#'
#' @import stringr
#' @importFrom curl curl
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom purrr map
#' @importFrom tibble tibble
BiocycPathwayGenes <- function(org.id = "HUMAN", pathway) {
  if (missing(pathway) | is.na(pathway)) {
    stop("Missing parameter \"pathway\"")
  }
  gene.nodes <- tryCatch({
    con <- curl::curl(str_glue(
      "https://websvc.biocyc.org/apixml?fn=genes-of-pathway&id={org.id}:{pathway}&detail=full"
    ))
    read_xml(con)
  },
  error = function(e) {
    close(con)
    message(str_glue(
      "Message from curl: {e}Check if the pathway ID is correct."
    ))
    return(NA)
  })

  if (is.na(gene.nodes)) {
    return(invisible(NULL))
  }

  num.result <- gene.nodes %>%
    xml_find_all("//metadata/num_results") %>%
    xml_text()
  message(str_glue(
    "Found {num.result} genes in {org.id} pathway {pathway}."
  ))
  biocyc.gene <- gene.nodes %>%
    xml_find_all("/ptools-xml/Gene/@frameid") %>%
    xml_text()
  biocyc.protein <- gene.nodes %>%
    xml_find_all("/ptools-xml/Gene/product/Protein/@frameid") %>%
    xml_text()
  gene.common.name <- gene.nodes %>%
    xml_find_all("/ptools-xml/Gene/common-name") %>%
    xml_text()
  gene.ensembl <- map(gene.common.name, function(x)
    gene.nodes %>%
      xml_find_all(str_glue("/ptools-xml/Gene[./common-name = '{x}']",
                            "/dblink[./dblink-db = 'ENSEMBL']/dblink-oid")) %>%
      xml_text() %>%
      str_flatten(",")
  ) %>%
    as.character()
  tibble(
    BiocycGene = biocyc.gene,
    BiocycProtein = biocyc.protein,
    Symbol = gene.common.name,
    Ensembl = gene.ensembl
  )
}
