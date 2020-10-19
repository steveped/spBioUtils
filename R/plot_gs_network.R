#' @title Visualise a network from a tidygraph object
#' @description Visualise a gene-expression network
#' @details
#' Draws a network graph of genes connected to gene-sets. By default,
#' up-regulated genes will be coloured red, whlst down-regulated genes will be
#' coloured blue. This requires a column labelled 'logFC' to be included in the
#' nodes component of the tidygraph.
#'
#' It is also assumed that an \code{NA} value for logFC will be provided for the
#' gene-set nodes. This is used to generate colours for each node, with edges
#' from each node being drawn in the same colours.
#'
#' Available palettes for nodes can be found using
#' \code{rownames(RColorBrewer::brewer.pal.info)} for the RColorBrewer palettes,
#' or \code{hcl.pals()} for the grDevices palettes
#'
#' @param tg The gene-set network as a tidygraph, as output by make_gs_network
#' @param layout The layout algorithm to apply
#' @param up_col The colour to label up-regulated genes
#' @param down_col The colour to label down-regulated genes
#' @param palette The name of the palette to use. Can be drawn from those
#' provided in RColorBrewer::brewer.pal, or grDevices::hcl.colors
#' @param palette_type Choose either RColorBrewer or grDevices palettes
#' @param gs_label_size Size of gene-set labels
#' @param gs_point_size Size of gene-set nodes
#' @param gs_shape Shape to use for gene-set nodes
#' @param gs_label_repel logical(1). Should the gene-set labels repel away from
#' the points
#' @param gs_label_padding Set the padding within each label around the text
#' @param gs_text_col Text colour for node labels
#' @param gene_label_repel logical(1). Should the gene labels repel away from
#' the points
#' @param gene_shape Shape to use for gene-level nodes
#' @param gene_alpha Transparency of gene-level nodes
#' @param gene_text_col Colour of the gene names
#' @param stroke Controls thickness of the coloured region inside any point
#' with a fill attribute
#' @param scale_size_trans Passed to
#' scale_size_continuous(trans = scale_size_trans)
#' @param scale_size_range Passed to
#' scale_size_continuous(range = scale_size_range)
#' @param na_col The colour for any lines and points with missing values
#'
#' @return A ggplot2 object
#'
#' @examples
#' set.seed(100)
#' geneSets <- list(
#'   a = as.character(1:3), b = as.character(3:5), c = as.character((1:3)*2)
#' )
#' topTable <- tibble(
#'   gene_name = as.character(1:6), PValue = runif(6), logFC = runif(6, -3, 3)
#' )
#' tg <- make_gs_network(gene_sets = geneSets, top_table = topTable)
#' plot_gs_network(tg)
#' ## Plots can be easily customised
#' suppressWarnings(
#'   plot_gs_network(tg, gs_shape = NA, gs_label_repel = FALSE,
#'   gs_label_size = 10, gs_label_padding = unit(0.5, "line")
#'   )
#' )
#'
#' @importFrom dplyr filter
#' @importFrom tibble as_tibble
#' @importFrom tidygraph activate
#' @importFrom RColorBrewer brewer.pal.info brewer.pal
#' @importFrom grDevices colorRampPalette hcl.pals hcl.colors
#' @importFrom ggraph ggraph geom_edge_arc geom_node_point geom_node_label
#' geom_node_text scale_edge_colour_manual theme_graph
#' @importFrom ggplot2 aes scale_fill_manual scale_size_continuous theme
#' @importFrom magrittr %>%
#' @importFrom grid unit
#' @importFrom methods is
#'
#' @export
plot_gs_network <- function(
  tg, layout = "fr", up_col = "#FF000033", down_col = "#0000FF33",
  palette = "Dark2", palette_type = c("RColorBrewer", "grDevices"),
  gs_label_size = 3, gs_point_size = 10, gs_shape = 21, gs_label_repel = TRUE,
  gs_label_padding = unit(0.15, "lines"), gs_text_col = "black",
  gene_label_repel = TRUE, gene_shape = 21, gene_alpha = 0.7,
  gene_text_col = "black", stroke = 0.5, scale_size_trans = "sqrt",
  scale_size_range = c(1, 3.5), na_col = "grey80"
){

  stopifnot(is(tg, "tbl_graph"))
  stopifnot("logFC" %in% names(as_tibble(activate(tg, nodes))))
  ## Dummy variables for RMD check
  logFC <- . <- nodes <- from <- id <- label <- c()

  palette_type <- match.arg(palette_type)

  # Define the palette
  n_nodes <- activate(tg, nodes) %>%
    dplyr::filter(is.na(logFC)) %>%
    as_tibble() %>%
    nrow()

  if (palette_type == "RColorBrewer"){
    info <- brewer.pal.info
    palette <- match.arg(palette, rownames(info))
    max_cols <- info[palette, "maxcolors"]
    n <- min(max_cols, n_nodes)
    node_pal <- brewer.pal(n, palette)
    node_pal <- colorRampPalette(node_pal)(n_nodes)
  }
  if (palette_type == "grDevices"){
    palette <- match.arg(palette, hcl.pals())
    node_pal <- hcl.colors(n_nodes, palette)
  }

  ggraph(tg, layout = layout) +
    # Add the edges, using the source node for colours
    geom_edge_arc(
      aes(color = as.character(from)),
      alpha = 0.5,
      show.legend = FALSE,
      strength = 0.5
    ) +
    # Add the gene-set nodes
    geom_node_point(
      aes(fill = as.character(id)),
      data = . %>% dplyr::filter(is.na(logFC)),
      size = gs_point_size,
      shape = gs_shape,
      stroke = stroke,
      show.legend = FALSE
    ) +
    # Labels for gene sets
    geom_node_label(
      aes(label = label, fill = as.character(id)),
      data = . %>% dplyr::filter(is.na(logFC)),
      repel = gs_label_repel,
      size = gs_label_size,
      force = 0.2,
      label.padding = gs_label_padding,
      colour = gs_text_col
    ) +
    # Upregulated genes
    geom_node_point(
      aes(size = abs(logFC)),
      data = . %>% dplyr::filter(logFC > 0),
      fill = up_col,
      shape = gene_shape,
      stroke = stroke,
      show.legend = FALSE,
      alpha = gene_alpha
    ) +
    # Down-regulated genes
    geom_node_point(
      aes(size = abs(logFC)),
      data = . %>% dplyr::filter(logFC < 0),
      fill = down_col,
      shape = gene_shape,
      stroke = stroke,
      show.legend = FALSE,
      alpha = gene_alpha
    ) +
    # Gene labels
    geom_node_text(
      aes(label = label, size = abs(logFC)),
      data = . %>% dplyr::filter(!is.na(logFC)),
      repel = gene_label_repel,
      colour = gene_text_col
    ) +
    scale_fill_manual(
      values = node_pal,
      na.value = na_col
    ) +
    scale_edge_colour_manual(
      values = node_pal,
      na.value = na_col
    ) +
    scale_size_continuous(trans = scale_size_trans, range = scale_size_range) +
    theme_graph() +
    theme(legend.position = "none")

}
