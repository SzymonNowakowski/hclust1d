#' @title Supported Distance Methods
#'
#' @description Lists all choices of a distance method currently supported in \code{hclust1d} via \code{dist} call.
#'
#' @return A character vector with currently supported distance methods.
#'
#' @export
supported_dist.methods <- function() c("euclidean", "maximum", "manhattan", "minkowski")
