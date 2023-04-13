#' @title Supported Linkage Methods
#'
#' @description Lists all choices of a linkage method currently supported in \code{hclust1d}.
#'
#' @return A character vector with currently supported linkage methods.
#'
#' @export
supported_methods <- function() c("complete", "average", "centroid", "true_median", "median", "single")
