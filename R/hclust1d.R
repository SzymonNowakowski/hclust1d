#' @title Hierarchical CLUSTering for 1D
#'
#' @description 1 dimensional hierarchical clustering  with a few possible choices of a linkage function.
#'
#' @param points set of 1D points to be clustered
#' @param method linkage method
#'
#' @details 1 dimensional hierarchical clustering  with a few possible choices of a linkage function.
#'
#' @return An object with S3 class \code{"hclust"}, compatible with a regular \code{stats::hclust} output:
#'
#' @examples
#' dendrogram <- hclust1d(rnorm(100))
#' plot(dendrogram)
#'
#' @export hclust1d

hclust1d <- function(x, distance = FALSE, method = "single") {
  #dispatch is written in R, because I don't know how to execute do.call() from Rcpp

  if (!is.numeric(x))
    stop("x must be numeric vector or distance matrix")

  if (!is.logical(distance) | length(distance)!=1)
    stop("distance must be a logical scalar")

  if (method == "single") {
    ret <- .hclust1d_single(x, dist)
    ret$call <- match.call()
    return(ret)
  } else {
    stop(paste("linkage", method, "not supported in the current version of hclust1d."))
  }

}
