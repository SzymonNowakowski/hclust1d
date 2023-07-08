#' @title Hierarchical Clustering for 1D
#'
#' @description Univariate hierarchical agglomerative clustering routine with a few possible choices of a linkage function.
#'
#' @param x a vector of 1D points to be clustered, or a distance structure as produced by \code{dist}.
#' @param distance a logical value indicating, whether \code{x} is a vector of 1D points to be clustered (\code{distance = FALSE}, the default), or a distance structure (\code{distance = TRUE}).
#' @param squared a logical value indicating, whether \code{distance} is squared (\code{squared = TRUE}) or not (\code{squared = FALSE}, the default). Its value is irrelevant for \code{distance = FALSE} setting.
#' @param method linkage method, with \code{"complete"} as a default. See \code{\link{supported_methods}} for the complete list.
#'
#' @details If \code{x} is a distance matrix, the first step of the algorithm is computing a conforming vector of 1D points (with arbitrary shift and sign choices).
#'
#' Univariate hierarchical clustering is performed
#' for the provided or calculated vector of points: initially, each point is assigned its own \emph{singleton} cluster, and
#' then the clusters get merged with their nearest neighbours, two at a time.
#'
#' For \code{method = "single"}, there is no need to recompute distances,
#' since the original inter-point distances are also the inter-cluster distances, so the algorithm requires
#' only sorting the original points and then sorting the distances.
#'
#' For other linkage methods, two distances (between the merged cluster and the preceding and the following clusters) get recomputed at each merge, and the resulting
#' distance structure gets updated in an efficiently implemented heap providing a priority queue functionality (the access to the current minimum distance) in O(log n) time at each step.
#' The resulting algorithm has O(n*log n) time complexity.
#'
#' @note Please note that in \code{stats::hclust}, the inter-cluster distances for ward.D, centroid and median linkages (returned as \code{height})
#' are \emph{squared} euclidean distances
#' between the relevant clusters' centroids, although that behaviour is not well documented. This behaviour is also in odds with other linkage methods, for which \emph{unsquared} euclidean distances are returned.
#' The implementation in \code{hclust1d::hclust1d} follows that behaviour in full.
#'
#' Also,
#' \code{stats::hclust} expects \emph{squared} euclidean distance structure as input for \code{method="ward.D"}, \code{method="centroid"} and \code{method="median"}, although the latter is not well documented, either. Squared
#' distance is not a proper distance (a triangle inequality may not be maintained), so it should be considered \emph{dissimilarity} instead.
#'
#' To retain compatibility, \code{hlust1d::hclust1d} accepts \code{x} in a form of a squared euclidean distance structure between points as well
#' (indicated by both \code{distance} and \code{squared} arguments set to \code{TRUE}). Also, note that
#' \code{hlust1d::hclust1d} returns the same heights for unsquared proper distances in \code{x} (with \code{distance=TRUE} setting and the default \code{squared=FALSE} argument)
#' and for \code{x} in a form of a vector of 1D points (with the default \code{distance=FALSE} argument). Please consult the \code{Examples} section below for further reference on that behaviour.
#'
#' @return A list object with S3 class \code{"hclust"}, compatible with a regular \code{stats::hclust} output:
#' \item{merge}{a matrix with n-1 rows and 2 columns. Each i-th row of the matrix details merging performed at the i-th step of the algorithm. If the \emph{singleton} cluster was merged
#' at this step, the value of the element is negative and its absolute value equals the index of this point.
#' Otherwise, a positive value, say j, of an element in i-th row, indicates that at the stage i a cluster created at a previous stage j was merged.}
#' \item{height}{a vector with n-1 values, with the i-th value indicating the distance between the two clusters merged at the i-th step of the algorithm.}
#' \item{order}{a permutation of the input points sorting them in an increasing order. Since the sign of points computed from the distance structure can be arbitrarily chosen, in the case of a distance structure input, the order can be increasing or decreasing.}
#' \item{labels}{either point names, or point values, or point indices, in the order of availability.}
#' \item{call}{the call which produced the results.}
#' \item{method}{the linkage method used for clustering.}
#' \item{dist.method}{the distance method used in building the distance matrix; or \code{"euclidean"}, if \code{x} is a vector of 1D points}
#'
#' @seealso \code{\link{supported_methods}} for listing of all currently supported linkage methods, \code{\link{supported_dist.methods}} for listing of all currently supported distance methods.
#'
#' @examples
#'
#' # Faster replacements for
#' # stats::hclust(dist(rnorm(100))) with a default complete linkage
#' dendrogram <- hclust1d(rnorm(100))
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE)
#'
#' # Faster replacements for
#' # stats::hclust(dist(rnorm(100)), method = "average")
#' dendrogram <- hclust1d(rnorm(100), method = "average")
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE, method = "average")
#'
#' # Faster replacements for
#' # stats::hclust(dist(rnorm(100))^2, method = "centroid")
#' # Note that stats::hclust expects squared euclidean distance input for centroid linkage
#' # While in case of hclust1d, 3 below calls result in the equivalent output
#' dendrogram <- hclust1d(rnorm(100), method = "centroid")
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE, method = "centroid")
#' dendrogram <- hclust1d(dist(rnorm(100))^2, distance = TRUE, squared = TRUE, method = "centroid")
#'
#' # Faster replacements for
#' # stats::hclust(dist(rnorm(100))^2, method = "median")
#' # Note that stats::hclust expects squared euclidean distance input for median linkage
#' # While in case of hclust1d, 3 below calls result in the equivalent output
#' dendrogram <- hclust1d(rnorm(100), method = "median")
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE, method = "median")
#' dendrogram <- hclust1d(dist(rnorm(100))^2, distance = TRUE, squared = TRUE, method = "median")
#'
#' # Faster replacements for
#' # stats::hclust(dist(rnorm(100)), method = "mcquitty")
#' dendrogram <- hclust1d(rnorm(100), method = "mcquitty")
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE, method = "mcquitty")
#'
#' # Faster replacements for
#' # stats::hclust(dist(rnorm(100))^2, method = "ward.D")
#' # Note that stats::hclust expects squared euclidean distance input for ward.D linkage
#' # While in case of hclust1d, 3 below calls result in the equivalent output
#' dendrogram <- hclust1d(rnorm(100), method = "ward.D")
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE, method = "ward.D")
#' dendrogram <- hclust1d(dist(rnorm(100))^2, distance = TRUE, squared = TRUE, method = "ward.D")
#'
#' # Faster replacements for
#' # stats::hclust(dist(rnorm(100)), method = "ward.D2")
#' dendrogram <- hclust1d(rnorm(100), method = "ward.D2")
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE, method = "ward.D2")
#'
#' # Faster replacements for
#' # stats::hclust(dist(rnorm(100)), method = "single")
#' dendrogram <- hclust1d(rnorm(100), method = "single")
#' dendrogram <- hclust1d(dist(rnorm(100)), distance = TRUE, method = "single")
#'
#' # A 1D-specific true median linkage
#' dendrogram <- hclust1d(rnorm(100), method = "true_median")
#'
#' # Plotting the resulting dendrogram
#' plot(dendrogram)
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
      x <- .sqrt(x)   # hclust1d has no need for squared distances, we sqrt them
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
