#' @title Add an input node to a dot graph
#'
#' @description Add an additional input node to a dot graph
#'
#' @details Takes a dot file as a character vector, such as would be obtained
#' using readLines, and adds a node to any existing nodes with no parental node.
#' Orphan nodes which should be ignored can be specified using the pattern in
#' the \code{ignore} argument.
#'
#' @param x dot file as a character vector
#' @param node The name of the node to add
#' @param ignore Regular expression used to define any orphan nodes which
#' should not be added to
#' @param col The colour for the input node
#' @param style The shape of the node
#'
#' @return A character vector
#'
#' @importFrom stringr str_subset str_replace_all str_extract str_detect
#'
#' @export
add_input_node <- function(
  x, node = "raw_data", ignore = "(get|build|make)", col = "#000000",
  style = "rectangle"
){

  stopifnot(.is_valid_snakemake_digraph(x))

  ## Find the nodes with no input
  all_id <- str_subset(x, "label")
  all_id <- str_replace_all(all_id, "\\t([0-9]*).label.+", "\\1")
  has_parent <- str_subset(x, " [0-9]+$")
  has_parent <- str_extract(has_parent, "[0-9]*$")
  to_ignore <- str_subset(x[-1], ignore)
  to_ignore <- str_replace_all(to_ignore, "\\t([0-9]*).label.+", "\\1")
  no_parent <- setdiff(all_id, c(has_parent, to_ignore))
  if (length(no_parent) == 0){
    warning(
      "All specified nodes have parents. The original graph will be returned"
    )
    return(x)
  }

  ## Check the colour is in RGB format
  if (!grepl("^#[0-9A-F]{6}", col) | is.numeric(col)) {
    col <- col2rgb(col)[,1]
    if (max(col) > 1) col <- col/255
    col <- do.call(rgb, as.list(col))
  }
  if (!grepl("^#[0-9A-F]{6}", col)) stop("Invalid colour specification")

  ## Find the position to insert the node (i.e. last in the dot file)
  new_id <- as.character(max(as.numeric(all_id)) +1)
  new_text <- c(
    "\t", new_id,
    "[label = \"", node,
    "\", color = \"", col,
    "\", style=\"", style,
    "\"];"
  )
  new_text <- paste(new_text, collapse = "")
  last_label_line <- which(str_detect(x, "label"))
  last_label_line <- max(last_label_line)

  ## Add the node
  x <- c(
    x[seq_len(last_label_line)],
    new_text,
    x[seq(last_label_line + 1, length(x))]
  )
  new_edges <- paste0("\t", paste(new_id, no_parent, sep = " -> "))
  c(
    x[seq(1, length(x) - 1)],
    new_edges,
    x[length(x)]
  )
}
