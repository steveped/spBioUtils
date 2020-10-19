#' @title Change the colour of a given node
#'
#' @description Change the colour of any nodes matching a pattern
#'
#' @details Finds nodes matching the regular expression provided and sets their
#' colour as provided
#'
#' @param x dot file as a character vector
#' @param pattern Regular expression used to define one or more nodes
#' @param col The new node colour
#'
#' @return A character vector
#'
#' @importFrom stringr str_replace_all
#' @export
change_node_colour <- function(x, pattern, col){

  stopifnot(.is_valid_snakemake_digraph(x))

  ## Check the colour is in RGB format
  if (!grepl("^#[0-9A-F]{6}", col) | is.numeric(col)) {
    col <- col2rgb(col)[,1]
    if (max(col) > 1) col <- col/255
    col <- do.call(rgb, as.list(col))
  }
  if (!grepl("^#[0-9A-F]{6}", col)) stop("Invalid colour specification")

  nodes <- which(grepl(pattern, x))
  x[nodes] <- str_replace_all(
    string = x[nodes],
    pattern = "(color = \"[#0-9A-F\\. ]+\")",
    replacement = paste0("color = \"", col, "\"")
  )
  x

}
