
repetitions <- 1:5

expect_some_equalities <- function(res_1, res_2) {
  if (res_1$method == "single_implemented_by_heap") {
    res_1$method <- "single"
  }
  if (res_2$method == "single_implemented_by_heap") {
    res_2$method <- "single"
  }

  expect_equal(res_1$dist.method, res_2$dist.method)
  expect_equal(res_1$method, res_2$method)
  expect_equal(nrow(res_1$merge), nrow(res_2$merge))
  expect_equal(res_1$height, res_2$height)

  expect_s3_class(res_1, class(res_2))
}

expect_equal_merges <- function(res_1, res_2) {
  for (i in 1:nrow(res_1$merge)) {
    expect_setequal(res_1$merge[i, ], res_2$merge[i, ])
  }
}

range <- 2:100
test_that("equality of results with stats::hclust, a vector without repetitions", {
  for (tested_method in c(supported_methods(), "single_implemented_by_heap")[-4]) {  #without a test for true_median
    stats_hlust_method <- tested_method
    if (tested_method == "single_implemented_by_heap")
      stats_hlust_method <- "single"
    for (len in range) {
      for (j in repetitions) {
        x <- rnorm(len)
        res_1d <- hclust1d(x, method=tested_method)

        squared <- FALSE
        distance <- dist(x)
        if (tested_method %in% c("centroid", "median")) {
          squared <- TRUE
          distance <- distance^2
        }

        res_1d_dist <- hclust1d(distance, distance = TRUE, squared = squared, method=tested_method)
        res_1d_alt <- hclust1d(-x, method=tested_method)
        res_md <- stats::hclust(distance, method=stats_hlust_method)

       # expect_all_equalities(res_1d, res_1d_alt, res_1d_dist)

        expect_some_equalities(res_1d, res_md)
        expect_equal_merges(res_1d, res_md)

      }
    }
  }
})

