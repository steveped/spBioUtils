#' @title Create a PCA plot from a DGEList object
#'
#' @description Plot the PCA results from a DGEList
#'
#' @param object An object containing expression values (i.e. counts)
#' @param ... Used for passing parameters to autoplot.prcomp. These should be
#' column names and can be passed to plotting aesthetics such as colour, shape
#' and size.
#' @param .center passed to \code{prcomp}
#' @param .scale passed to \code{prcomp}
#'
#' @return A ggplot2 object
#'
#'
#' @examples
#' library(edgeR)
#' y <- matrix(rnbinom(10000,mu=5,size=2), ncol=4)
#' d <- DGEList(counts=y, group=rep(1:2, each=2))
#' plotExpressionPCA(d, colour = "group")
#'
#' library(SummarizedExperiment)
#' nrows <- 200; ncols <- 6
#' counts <- matrix(runif(nrows * ncols, 1, 1e4), nrows)
#' colData <- DataFrame(Treatment=rep(c("ChIP", "Input"), 3),
#'                      row.names=LETTERS[1:6])
#' se0 <- SummarizedExperiment(assays=SimpleList(counts=counts),
#'                             colData=colData)
#' plotExpressionPCA(se0, colour = "Treatment")
#'
#' @import ggfortify
#' @importFrom edgeR cpm DGEList
#' @importFrom stats prcomp
#' @importFrom ggplot2 autoplot
#' @importFrom SummarizedExperiment SummarizedExperiment colData
#' @importFrom S4Vectors DataFrame
#' @importClassesFrom edgeR DGEList
#' @importClassesFrom SummarizedExperiment SummarizedExperiment
#' @importClassesFrom S4Vectors DataFrame
#'
#' @export
#' @rdname plotExpressionPCA-methods
setGeneric("plotExpressionPCA", function(object, ...){
  standardGeneric("plotExpressionPCA")
})
#' @export
#' @rdname plotExpressionPCA-methods
setMethod("plotExpressionPCA", "DGEList", function(
  object, ..., .center = TRUE, .scale = FALSE
){
  cpm <- cpm(object, log = TRUE)
  pca <- prcomp(t(cpm), center = .center, scale. = .scale)
  autoplot(pca, data = object$samples, ...)
}
)
#' @export
#' @rdname plotExpressionPCA-methods
setMethod("plotExpressionPCA", "SummarizedExperiment", function(
  object, ..., .center = TRUE, .scale = FALSE
){
  cpm <- cpm(object, log = TRUE)
  pca <- prcomp(t(cpm), center = .center, scale. = .scale)
  df <- colData(object)
  autoplot(pca, data = as.data.frame(df), ...)
}
)
