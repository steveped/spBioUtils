#' @title Form a tidygraph object
#' @description Form a tidygraph from a list of genesets
#' @details
#' Taking a list of genesets as the primary input form a network. Names of each
#' list element are assumed to be the names of genesets, with the values within
#' each element assumed to be gene identifiers. Choosing actual gene names may
#' be the most useful option.
#'
#' If provided, the topTable will be joined onto the nodes allowing all columns
#' to be used for modifying the final plot.
#' The most useful columns may be either logFC or the PValue.
#'
#' By default, all nodes (i.e. gene-sets and gene names) will be contained in a
#' column named 'label'. Setting the correct column from the topTable to join
#' on can be performed using the .by argument
#' @param gene_sets A list of gene-sets as described in the details section
#' @param top_table An optional topTable as output by a function such as
#' limma::topTable(), or edgeR::topTags()$table
#' @param .by If providing the topTable, this will be
#' passed to dplyr::left_join()
#'
#' @return A tbl_graph
#'
#' @importFrom tibble tibble rowid_to_column
#' @importFrom dplyr left_join
#' @importFrom tidygraph tbl_graph activate
#'
#' @export
make_geneset_network <- function(gene_sets, top_table, .by = c("label" = "gene_name")){

  ## Check all genesets are characters
  stopifnot(
    all(
      vapply(gene_sets, is.character, logical(1))
    )
  )
  # Make sure the gensets are a named list
  stopifnot(length(names(gene_sets)) == length(gene_sets))

  ## Create a node list
  nodes <- tibble(label = c(names(gene_sets), unlist(gene_sets)))
  nodes <- dplyr::distinct(nodes, label)
  nodes <- rowid_to_column(nodes, "id")

  ## Then create an edge list connecting each gene to it's geneset
  edges <- lapply(
    names(gene_sets),
    function(x){
      tibble(pathway = x, gene = gene_sets[[x]])
    }
  )
  edges <- bind_rows(edges)
  edges <- left_join(edges, nodes, by = c("pathway" = "label"))
  edges <- dplyr::rename(edges, from = id)
  edges <- left_join(edges, nodes, by = c("gene" = "label"))
  edges <- dplyr::rename(edges, to = id)
  edges <- dplyr::select(edges, from, to)

  ## Add the topTable
  if (!missing(top_table)){
   nodes <- left_join(nodes, top_table, by = .by)
  }

  g <- tbl_graph(
    nodes = nodes,
    edges = edges,
    directed = FALSE
  )

  g

}
