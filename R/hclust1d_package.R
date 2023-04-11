#' hclust1d-package
#'
#' @name hclust1d-package
#' @docType package
#' @description \code{hclust1d} (Hierarchical CLUSTering for 1D) is a suit of algorithms for univariate agglomerative hierarchical clustering  (with a few possible choices of a linkage function) in O(n*log n) time. The better algorithmic time complexity is paired with an efficient \code{C++} implementation.
#' @section All \code{hclust1d} Functions:
#'
#' Similar in use to \code{stats::hclust}. It consists of the following functions:
#'
#' \code{\link{hclust1d}} - univariate agglomerative hierarchical clustering routine.
#'
#' \code{\link{supported_methods}} - lists all currently supported linkage methods.
#'
#' \code{\link{supported_dist.methods}} - lists all currently supported distance methods.
#'
#' For more information see a friendly "Getting started" vignette:
#' @examples
#' \dontrun{
#' vignette("getting-started", package="hclust1d")
#' }
#'
#' @author Maintainer: Szymon Nowakowski <s.nowakowski@mimuw.edu.pl>
#'
#' @importFrom Rcpp evalCpp
#' @useDynLib hclust1d, .registration=TRUE
#' @exportPattern "^[[:alpha:]]+"
NULL
