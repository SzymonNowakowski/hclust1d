#' @title Supported Distance Methods
#'
#' @description Lists all choices of a distance method currently supported in \code{hclust1d} via \code{dist} call.
#'
#' @return A character vector with currently supported distance methods.
#'
#' @examples
#'
#' if ("minkowski" %in% supported_dist.methods()) {    # the condition under if evaluates to TRUE
#'    dendrogram <- hclust1d(dist(rnorm(100), method = "minkowski"))
#'    plot(dendrogram)
#' } else {
#'    stop("Error: minkowski distance method not supported in hclust1d")
#' }
#'
#' @export
supported_dist.methods <- function() c("euclidean", "maximum", "manhattan", "minkowski")
