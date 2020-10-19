#' @title Remove a single node from a snakemake dag
#'
#' @description Remove a single node from a snakemake dag
#'
#' @details Takes a dot file as a character vector, such as would be obtained
#' using readLines, and removes a single node.
#' Commonly this is the 'all' node as might be produced from a snakemake
#' rulegraph
#'
#' The resultant graph can then be visualised using \code{grViz} from the
#' package DiagrammeR, or passed into any function which takes dot format as a
#' character vector
#'
#' @param x dot file as a character vector
#' @param node The name of node to drop
#'
#' @return A character vector
#'
#' @importFrom stringr str_replace_all
#'
#' @export
rm_dot_node <- function(x, node = "all"){

  stopifnot(.is_valid_snakemake_digraph(x))
  if (all(!grepl(node, x))) message("Couldn't detect node: ", node)
  if (sum(grepl(node, x)) > 1) warning("Multiple nodes match the request")

  nd <- x[grepl(node, x)]
  id <- str_replace_all(nd, "\\t([0-9]*).label.+", "\\1")
  pat <- paste(c("(^\\t", id,  "| -> ", id, "$)"), collapse = "")
  x[!grepl(pat, x)]
}
