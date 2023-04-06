#' @title Hierarchical Clustering for 1D
#'
#' @description Univariate hierarchical agglomerative clustering  with a few possible choices of a linkage function.
#'
#' @param x a vector of 1D points to be clustered, or a distance structure as produced by \code{dist}.
#' @param distance a logical value indicating, whether \code{x} is a vector of 1D points to be clustered (\code{distance=FALSE}, the default), or a distance structure (\code{distance=TRUE}, the default).
#' @param method linkage method, currently only "single" is supported.
#'
#' @details If \code{x} is a distance matrix, at first a conforming vector of 1D points is computed (with arbitrary shift and sign choices). The supported methods for \code{dist} are
#' the following: \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"}, \code{"minkowski"}.
#'
#' Univariate hierarchical clustering is performed
#' for the provided or calculated vector of points: initially, each point is assigned its own \emph{singleton} cluster, and
#' then the clusters get merged with their nearest neighbours, two at a time.
#'
#' For \code{method="single"} there is no need to recompute distances,
#' as the original inter-point distances are also the inter cluster distances, so the algorithm requires
#' only sorting the original points and then sorting the distances.
#'
#' For other linkage methods, two distances (between the merged cluster and the preceding and the following clusters) get recomputed at each merge, and the resulting
#' distance structure gets updated in an efficiently implemented heap providing a priority queue functionality (the access to the current minimum distance) in O(log n) time at each step.
#' The resulting algorithm has O(n*log n) time complexity.
#'
#' @return An object with S3 class \code{"hclust"}, compatible with a regular \code{stats::hclust} output:
#' \item{merge}{a matrix with $n-1$ rows and 2 columns. Each $i$-th row of the matrix details merging performed at the stage $i$. If the \emph{singleton} cluster was merged
#' at this step, the value of the element is negative and its absolute value indicates the index of this point.
#' Otherwise, a positive value, say $j$, of an element in $i$-th row indicate that at the stage $i$ a cluster created at a previous stage $j$ was merged.}
#' \item{height}{a vector with $n-1$ values, with the $i$-th value indicating the distance between the two clusters merged at the stage $i$.}
#' \item{order}{a permutation of the original points sorting them in an increasing order.}
#' \item{labels}{either point names, or point values, or point indices, in the order of availability.}
#' \item{call}{the call which produced the results.}
#' \item{method}{the linkage method used for clustering.}
#' \item{dist.method}{the distance method used in building the distance matrix, or \code{"euclidean"} if \code{x} is a vector of 1D points}
#' @examples
#' dendrogram <- hclust1d(rnorm(100))
#' plot(dendrogram)
#'
#' @export hclust1d

hclust1d <- function(x, distance = FALSE, method = "single") {
  #dispatch is written in R, because I don't know how to execute do.call() from Rcpp

  error_2_points<- "at least two objects are needed to analyse clusters with hclust1d"

  if (!is.numeric(x)) {
    stop("x must be numeric vector or distance matrix")
  }

  if (!is.logical(distance) | length(distance)!=1) {
    stop("distance must be a logical scalar")
  }

  if (distance) {

    if (!inherits(x, "dist")) {
      stop("x must inherit from S3 class dist for distance-based computation")
    }

    points_size <- attr(x, "Size")

    if (points_size < 2)
      stop(error_2_points);

    if (points_size*(points_size - 1) / 2L != length(x)) {   #read more at https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist
      stop("nonconforming shape of the provided distance matrix with a dist-typed S3 object")
    }

    dist_method <- attr(x, "method")

    supported_dist_methods <- c("euclidean", "maximum", "manhattan", "minkowski")
    if (!(dist_method %in% supported_dist_methods)) {
      stop(paste(c("only those distance methods are supported in dist:", paste(supported_dist_methods, sep=", "))))
    }

    x <- .dedistance(x, points_size)
  }

  if (method == "single") {

    if (length(x) < 2)
      stop(error_2_points);

    ret <- .hclust1d_single(x)
    ret$call <- match.call()

  } else {
    stop(paste("linkage", method, "not supported in the current version of hclust1d."))
  }

  if (distance)  #override the dist.method for distance-based computations
    ret$dist.method <- dist_method

  return(ret)

}
