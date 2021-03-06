#' @title Form a tidygraph object
#' @description Form a tidygraph from a list of gene-sets
#' @details
#' Taking a list of gene-sets as the primary input form a network. Names of each
#' list element are assumed to be the names of gene-sets, with the values within
#' each element assumed to be gene identifiers. Choosing actual gene names may
#' be the most useful option. All genes provided will be plotted, so removing
#' uninteresting genes may be helpful before passing to this function
#'
#' If provided, the topTable will be joined onto the nodes allowing all columns
#' to be used for modifying the final plot.
#' The most useful columns may be either logFC or the PValue.
#'
#' By default, all nodes (i.e. gene-sets and gene names) will be contained in a
#' column named 'label'. Setting the correct column from the topTable to join
#' on can be performed using the .by argument.
#'
#' It should also be noted that these networks become very cumbersome, very
#' quickly and as such, supplying a relatively small number (e.g. 10) of nodes
#' (i.e. gene-sets) may be the most viable approach
#'
#' @param gene_sets A list of gene-sets as described in the details section
#' @param top_table An optional topTable as output by a function such as
#' limma::topTable(), or edgeR::topTags()$table
#' @param .by If providing the topTable, this will be
#' passed to dplyr::left_join()
#'
#' @return A tbl_graph
#'
#' @importFrom tibble tibble rowid_to_column
#' @importFrom dplyr left_join bind_rows
#' @importFrom tidygraph tbl_graph activate
#' @importFrom tidyselect one_of
#'
#' @examples
#' set.seed(100)
#' geneSets <- list(
#'   a = as.character(1:3), b = as.character(3:5), c = as.character((1:3)*2)
#'   )
#' topTable <- tibble(
#'   gene_name = as.character(1:6), PValue = runif(6), logFC = runif(6, -3, 3)
#'   )
#' tg <- make_gs_network(gene_sets = geneSets, top_table = topTable)
#'
#' @export
make_gs_network <- function(gene_sets, top_table, .by = c("label" = "gene_name")){

  ## Check all genesets are characters
  stopifnot(
    all(
      vapply(gene_sets, is.character, logical(1))
    )
  )
  # Make sure the gensets are a named list
  stopifnot(length(names(gene_sets)) == length(gene_sets))

  ## Create a node list
  label <- c() # Dummy for R CMD check
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
  edges <- dplyr::rename(edges, "from" = "id")
  edges <- left_join(edges, nodes, by = c("gene" = "label"))
  edges <- dplyr::rename(edges, "to" = "id")
  edges <- dplyr::select(edges, one_of(c("from", "to")))

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
