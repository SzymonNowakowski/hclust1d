#' @title Hierarchical Clustering for 1D
#'
#' @description Univariate hierarchical agglomerative clustering  with a few possible choices of a linkage function.
#'
#' @param x a vector of 1D points to be clustered, or a distance structure as produced by \code{dist}.
#' @param distance a logical value indicating, whether \code{x} is a vector of 1D points to be clustered (\code{distance = FALSE}, the default), or a distance structure (\code{distance = TRUE}).
#' @param squared a logical value indicating, whether \code{distance} is squared (\code{squared = TRUE}) or not (\code{squared = FALSE}, the default). Its value is irrelevant for \code{distance = FALSE} setting.
#' @param method linkage method, with \code{"complete"} as a default.
#'
#' @details If \code{x} is a distance matrix, at first a conforming vector of 1D points is computed (with arbitrary shift and sign choices).
#'
#' Univariate hierarchical clustering is performed
#' for the provided or calculated vector of points: initially, each point is assigned its own \emph{singleton} cluster, and
#' then the clusters get merged with their nearest neighbours, two at a time.
#'
#' For \code{method = "single"} there is no need to recompute distances,
#' as the original inter-point distances are also the inter cluster distances, so the algorithm requires
#' only sorting the original points and then sorting the distances.
#'
#' For other linkage methods, two distances (between the merged cluster and the preceding and the following clusters) get recomputed at each merge, and the resulting
#' distance structure gets updated in an efficiently implemented heap providing a priority queue functionality (the access to the current minimum distance) in O(log n) time at each step.
#' The resulting algorithm has O(n*log n) time complexity.
#'
#' Please note that (following \code{stats::hclust} implementation for \code{method = "centroid"}), the inter-cluster distances for centroid linkage (returned as a \code{height})
#' are \emph{squared} euclidean distances
#' between the relevant clusters' centroids. This behaviour is in odds with other linkage methods, for which \emph{unsquared} euclidean distances are returned. Also,
#' for full compatibility with \code{stats::hclust} for \code{method="centroid"}, the squared euclidean distance can be fed into
#' \code{hlust1d::hclust1d} with \code{distance} and \code{squared} arguments both set to \code{TRUE}, but also note that
#' \code{hlust1d::hclust1d} returns the same heights for unsquared distances in \code{x} (with the default \code{squared=FALSE} argument)
#' and for \code{x} in a form of a vector of 1D points (with the default \code{distance=FALSE} argument).
#'
#' @return A list object with S3 class \code{"hclust"}, compatible with a regular \code{stats::hclust} output:
#' \item{merge}{a matrix with $n-1$ rows and 2 columns. Each $i$-th row of the matrix details merging performed at the stage $i$. If the \emph{singleton} cluster was merged
#' at this step, the value of the element is negative and its absolute value indicates the index of this point.
#' Otherwise, a positive value, say $j$, of an element in $i$-th row indicate that at the stage $i$ a cluster created at a previous stage $j$ was merged.}
#' \item{height}{a vector with $n-1$ values, with the $i$-th value indicating the distance between the two clusters merged at the stage $i$.}
#' \item{order}{a permutation of the original points sorting them in an increasing order.}
#' \item{labels}{either point names, or point values, or point indices, in the order of availability.}
#' \item{call}{the call which produced the results.}
#' \item{method}{the linkage method used for clustering.}
#' \item{dist.method}{the distance method used in building the distance matrix, or \code{"euclidean"} if \code{x} is a vector of 1D points}
#'
#' @seealso [supported_methods()] for listing of all currently supported linkage methods, [supported_dist.methods()] for listing of all currently supported distance methods.
#'
#' @examples
#'
#' dendrogram <- hclust1d(rnorm(100)) # a faster replacement for
#'               # stats::hclust(dist(rnorm(100))) with a default complete linkage
#'
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE) # a faster replacement for
#'               # stats::hclust(dist(rnorm(100))) with a default complete linkage
#'
#' dendrogram <- hclust1d(rnorm(100), method = "average") # a faster replacement for
#'               # stats::hclust(dist(rnorm(100)), method = "average")
#'
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE, method = "average") # a faster replacement for
#'               # stats::hclust(dist(rnorm(100)), method = "average")
#'
#' dendrogram <- hclust1d(rnorm(100), method = "centroid") # a faster replacement for
#'               # stats::hclust(dist(rnorm(100))^2, method = "centroid")
#'
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE, method = "centroid") # a faster replacement for
#'               # stats::hclust(dist(rnorm(100))^2, method = "centroid")
#'
#' dendrogram <- hclust1d(dist(rnorm(100))^2, distance = TRUE, squared = TRUE, method = "centroid") # a faster replacement for
#'               # stats::hclust(dist(rnorm(100))^2, method = "centroid")
#'
#' dendrogram <- hclust1d(rnorm(100), method = "single") # a faster replacement for
#'               # stats::hclust(dist(rnorm(100)), method = "single")
#'
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE, method = "single") # a faster replacement for
#'               # stats::hclust(dist(rnorm(100)), method = "single")
#'
#' plot(dendrogram) # plots the resulting dendrogram
#'
#' @export
hclust1d <- function(x, distance = FALSE, squared = FALSE, method = "complete") {
  #dispatch is written in R, because I don't know how to execute do.call() from Rcpp

  error_2_points<- "at least two objects are needed to analyse clusters with hclust1d"

  if (!is.numeric(x)) {
    stop("x must be numeric vector or distance matrix")
  }

  if (!is.logical(distance) | length(distance)!=1) {
    stop("distance must be a logical scalar")
  }

  if (!is.logical(squared) | length(squared)!=1) {
    stop("squared must be a logical scalar")
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

    if (squared == TRUE) {
      x <- sqrt(x)   # hclust1d has no need for squared distances
    }

    x <- .dedistance(x, points_size)
  }

  if (length(x) < 2)
    stop(error_2_points);

  if (method == "single") {

    ret <- .hclust1d_single(x)
    ret$call <- match.call()

  } else if (method %in% supported_methods()) {

    ret <- .hclust1d_heapbased(x, pmatch(method, supported_methods()))
    ret$call <- match.call()
    ret$method <- method

  } else if (method == "single_implemented_by_heap") {  # intentionally undocumented behaviour
      # intended for efficiency tests
      # DO NOT USE as it may be dropped in future versions without notice
      #
    ret <- .hclust1d_heapbased(x, 0)
    ret$call <- match.call()
    ret$method <- method

  } else {
    stop(paste("linkage", method, "not supported in the current version of hclust1d. See supported_methods() for more information"))
  }

  if (distance)  #override the dist.method for distance-based computations
    ret$dist.method <- dist_method

  return(ret)

}
