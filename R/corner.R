#' @title Display one corner of a large matrix-like object
#'
#' @description Display one corner of a large matrix-like object
#'
#' @details
#' This provides an alternative to head for matrix-like objects with many
#' columns
#' @param x A matrix or data.frame
#' @param corner The corner to show
#' @param n The number of rows and columns to show
#' @param ... arguments to be passed to or from other methods.
#'
#' @importFrom utils head
#'
#' @export
corner <- function(x, ...) UseMethod("corner")

#' @describeIn corner Display one corner of a large matrix-like object
#' @export
corner.matrix <- function(
  x, corner = c("topleft", "topright", "bottomleft", "bottomright"), n = 6L,
  ...
){

  corner <- match.arg(corner)
  n <- as.integer(n)
  stopifnot(n > 0)
  nr <- nrow(x)
  nc <- ncol(x)
  if (nc <= n) return(head(x, n, ...))

  if (corner == "topleft"){
    i <- seq_len(min(n, nc))
    j <- seq_len(min(n, nr))
  }

  if (corner == "topright"){
    i <- seq(nc - n + 1, nc)
    j <- seq_len(min(n, nr))
  }

  if (corner == "bottomleft"){
    i <- seq_len(min(n, nc))
    st <- max(1, nr - n + 1)
    j <- seq(st, nr)
  }

  if (corner == "topright"){
    i <- seq(nc - n + 1, nc)
    st <- max(1, nr - n + 1)
    j <- seq(st, nr)
  }

  x[i, j]

}

#' @describeIn corner Display one corner of a large matrix-like object
#' @export
corner.data.frame <- function(
  x, corner = c("topleft", "topright", "bottomleft", "bottomright"), n = 6L,
  ...
){

  corner <- match.arg(corner)
  n <- as.integer(n)
  stopifnot(n > 0)
  nr <- nrow(x)
  nc <- ncol(x)
  if (nc <= n) return(head(x, n, ...))

  if (corner == "topleft"){
    i <- seq_len(min(n, nr))
    j <- seq_len(min(n, nc))
  }

  if (corner == "topright"){
    i <- seq_len(min(n, nr))
    j <- seq(nc - n + 1, nc)
  }

  if (corner == "bottomleft"){
    st <- max(1, nr - n + 1)
    i <- seq(st, nr)
    j <- seq_len(min(n, nc))
  }

  if (corner == "bottomright"){
    st <- max(1, nr - n + 1)
    i <- seq(st, nr)
    j <- seq(nc - n + 1, nc)
  }

  x[i, j]

}
