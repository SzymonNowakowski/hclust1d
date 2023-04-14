#' @title Supported Linkage Methods
#'
#' @description Lists all choices of a linkage method currently supported in \code{hclust1d}.
#'
#' @return A character vector with currently supported linkage methods.
#'
#' @examples
#'
#' if ("median" %in% supported_methods()) {    # the condition under if evaluates to TRUE
#'    dendrogram <- hclust1d(rnorm(100), method = "median")
#'    plot(dendrogram)
#' } else {
#'    stop("Error: median linkage method not supported in hclust1d")
#' }
#'
#' @export
supported_methods <- function() c("complete", "average", "centroid", "true_median", "median", "single")
