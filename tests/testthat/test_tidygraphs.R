set.seed(100)
geneSets <- list(a = as.character(1:3), b = as.character(3:5))
topTable <- tibble(
  gene_name = as.character(1:5), PValue = runif(5)
)
tg <- make_geneset_network(gene_sets = geneSets, top_table = topTable)

test_that("Output from make_geneset_network is correct when expected to be", {
  expect_s3_class(object = tg, class = "tbl_graph")
  expect_s3_class(object = tg, class = "igraph")
  expect_equal(dim(as_tibble(tidygraph::activate(tg, nodes))), c(7, 3))
  expect_equal(dim(as_tibble(tidygraph::activate(tg, edges))), c(6, 2))
})

test_that("Output from make_geneset_network fails when expected", {
  expect_error(make_geneset_network(list(a = 1:3, b = 3:5)))
  expect_error(make_geneset_network(gene_sets = geneSet, top_table = topTable, .by = c("label" = "gene_id")))
})
