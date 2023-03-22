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

hclust1d <- function(points, method = "single") {
  #dispatch is written in R, because I don't know how to execute do.call() from Rcpp

  if (method == "single") {
    ret <- .hclust1d_single(points)
    ret$call <- match.call()
    return(ret)
  } else {
    stop(paste("linkage", method, "not supported in the current version of hclust1d."))
  }

}
