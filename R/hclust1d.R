#' @title Hierarchical CLUSTering for 1D
#'
#' @description Univariational hierarchical agglomerative clustering  with a few possible choices of a linkage function.
#'
#' @param x a vector of 1D points to be clustered, or a distance structure as produced by \code{dist}.
#' @param distance a logical value indicating, whether \code{x} is a vector of 1D points to be clustered (\code{distance=FALSE}, the default), or a distance structure (\code{distance=TRUE}, the default).
#' @param method linkage method, one of "single"
#'
#' @details If \code{x} is a distance matrix, at first the vector of 1D points is computed (with an arbitrary shift). Univariational hierarchical clustering is performed
#' for the provided vector of points: initially, each point is assigned its own \emph{singleton} cluster, and
#' then the clusters get iteratively merged.
#'
#' For \code{method="single"} there is no need to recompute distances,
#' as the original inter-point distances are also the inter cluster distances, so the algorithm requires
#' only sorting the original points and then sorting the distances.
#'
#' For other linkage methods, two distances (between the merged cluster and the preceding and the following clusters) get recomputed at each merge, and the resulting
#' distance structure gets updated in an effciently implemented heap providing a priority queue functionality (the access to the current minimum distance) in O(log n) time at each step.
#' The resulting algorithm has O(n*log n) time complexity.
#'
#' @return An object with S3 class \code{"hclust"}, compatible with a regular \code{stats::hclust} output:
#' \item{merge}{a matrix with $n-1$ rows and 2 columns. Each $i$-th row of the matrix details merging performed at the stage $i$. If the \emph{singleton} cluster was merged
#' at this step, the value of the element is negative and its absolute value indicates the index of this point.
#' Otherwise, a positive value, say $j$, of an element in $i$-th row indicate that at the stage $i$ a cluster created at a previous stage $j$ was merged.}
#' \item{height}{a vector with $n-1$ values, with the $i$-th value indicating the distance between the two clusters merged at the stage $i$.}
#' \item{order}{a permutation of the original points sorting them in an increasing order.}
#' \item{labels}{either point names, if available, or point values.}
#' \item{call}{the call which produced the results.}
#' \item{method}{the linkage method used for clustering.}
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
    ret <- .hclust1d_single(x)
    ret$call <- match.call()
    return(ret)
  } else {
    stop(paste("linkage", method, "not supported in the current version of hclust1d."))
  }

}
