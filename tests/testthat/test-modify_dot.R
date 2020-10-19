# An invalid snakemake structure
x <- c(
  "digraph no_id {",
  "}"
)
# A valid snakemake structure
y <- c(
  "digraph snakemake_dag {",
  "\t0[label = \"all\", color = \"0.41 0.6 0.85\", style=\"rounded\"];",
  "\t1[label = \"some_rule\", color = \"0.0 0.0 0.0\", style=\"rounded\"];",
  "\t1 -> 0",
  "}"
)

test_that("dot checks pass", {
  expect_true(.is_dot(x))
  expect_false(.is_dot(x[[1]]))
  expect_false(.is_dot(x[[2]]))
})

test_that("snakemake checks pass", {
  expect_warning(.is_valid_snakemake_digraph(x))
  expect_false(suppressWarnings(.is_valid_snakemake_digraph(x)))
  expect_true(.is_valid_snakemake_digraph(y))
})

test_that("Adding nodes works", {
  y_add <- add_input_node(y, node = "test_node")
  expect_equal(length(y_add), length(y) + 2)
  expect_true(any(grepl("\t2 -> 1", y_add)))
})

test_that("Removing nodes works", {
  y_rm <- rm_dot_node(y, "all")
  expect_equal(length(y_rm), length(y) - 2)
  expect_false(any(grepl("\t0", y_rm)))
})
