set.seed(100)
geneSets <- list(
  a = as.character(1:3), b = as.character(3:5), c = as.character((1:3)*2)
)
n <- max(as.integer(unlist(geneSets)))
topTable <- tibble(
  gene_name = as.character(1:n), PValue = runif(n), logFC = runif(n, -3, 3)
)
tg <- make_gs_network(gene_sets = geneSets, top_table = topTable)

test_that("Output from make_gs_network is correct when expected to be", {
  expect_s3_class(object = tg, class = "tbl_graph")
  expect_s3_class(object = tg, class = "igraph")
  expect_equal(dim(as_tibble(tidygraph::activate(tg, nodes))), c(9, 4))
  expect_equal(dim(as_tibble(tidygraph::activate(tg, edges))), c(9, 2))
})

test_that("Output from make_gs_network fails when expected", {
  expect_error(make_gs_network(list(a = 1:3, b = 3:5)))
  expect_error(make_gs_network(gene_sets = geneSet, top_table = topTable, .by = c("label" = "gene_id")))
})

test_that("plot_gs_network works", {
  expect_s3_class(plot_gs_network(tg), "ggraph")
  expect_error(plot_gs_network("x"))
})
