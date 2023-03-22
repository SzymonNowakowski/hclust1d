#' hclust1d: hclust1d (Hierarchical CLUSTering for 1D) is a suit of algorithms for 1 dimensional hierarchical clustering  (with a few possible choices of a linkage function) in O(n*log n) time. The better algorithmic time complexity is paired with an efficient C++ implementation.
#'
#' @docType package
#' @name hclust1d
#' @importFrom Rcpp evalCpp
#' @useDynLib hclust1d, .registration=TRUE
#' @exportPattern "^[[:alpha:]]+"
NULL
