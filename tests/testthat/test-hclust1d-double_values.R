
repetitions <- 1:1

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

expect_all_equalities <- function(res_1d, res_1d_alt, res_1d_dist) {
  expect_some_equalities(res_1d, res_1d_dist)
  expect_some_equalities(res_1d_alt, res_1d_dist)

  compare_alt <- TRUE
  if (nrow(res_1d$merge) == 1 & res_1d$merge[1,1] == res_1d_dist$merge[1,1] & res_1d$merge[1,2] == res_1d_dist$merge[1,2]) {
    compare_alt <- FALSE
  }
  if (nrow(res_1d$merge) == 2) if (res_1d$merge[1,1] == res_1d_dist$merge[1,1] & res_1d$merge[1,2] == res_1d_dist$merge[1,2] &
                                   res_1d$merge[2,1] == res_1d_dist$merge[2,1] & res_1d$merge[2,2] == res_1d_dist$merge[2,2]) {
    compare_alt <- FALSE
  }
  if (nrow(res_1d$merge) == 3) if (res_1d$merge[1,1] == res_1d_dist$merge[1,1] & res_1d$merge[1,2] == res_1d_dist$merge[1,2] &
                                   res_1d$merge[2,1] == res_1d_dist$merge[2,1] & res_1d$merge[2,2] == res_1d_dist$merge[2,2] &
                                   res_1d$merge[3,1] == res_1d_dist$merge[3,1] & res_1d$merge[3,2] == res_1d_dist$merge[3,2]) {
    compare_alt <- FALSE
  }
  if (nrow(res_1d$merge) >= 4) if (res_1d$merge[1,1] == res_1d_dist$merge[1,1] & res_1d$merge[1,2] == res_1d_dist$merge[1,2] &
                                   res_1d$merge[2,1] == res_1d_dist$merge[2,1] & res_1d$merge[2,2] == res_1d_dist$merge[2,2] &
                                   res_1d$merge[3,1] == res_1d_dist$merge[3,1] & res_1d$merge[3,2] == res_1d_dist$merge[3,2] &
                                   res_1d$merge[4,1] == res_1d_dist$merge[4,1] & res_1d$merge[4,2] == res_1d_dist$merge[4,2]) {
    compare_alt <- FALSE
  }


  if (!compare_alt) {
    #wild swing of luck here!
    expect_equal(res_1d$merge, res_1d_dist$merge)

  } else {
    expect_equal(res_1d_alt$merge, res_1d_dist$merge)
  }
}

get_order_double <- function(res) order(vapply(1:nrow(res$merge), function(i) if (min(res$merge[i, ]) < 0) min(res$merge[i, ]) else i, numeric(1)))

range <- 1:50    #1:80
percent <- 0.25
test_that("equality of results with stats::hclust, a vector with double repetitions", {
  # with double repetitions the clusters comprising of two singletons should be the same as in stats::hclust
  # but may be differently ordered
  for (tested_method in c( supported_methods(), "single_implemented_by_heap")[-4]) {  #without a test for true_median
    stats_hlust_method <- tested_method
    if (tested_method == "single_implemented_by_heap")
      stats_hlust_method <- "single"
    for (len in range) {
      for (j in repetitions) {
        x <- rnorm(len)
        cnt_to_add <- max(1, len * percent)

        new_x_indices <- c(1:length(x), 1:cnt_to_add)
        new_x <- x[new_x_indices]

        shuffle_vector <- sample(1:length(new_x), length(new_x), replace = FALSE) #random permutation of new_x

        x <- new_x[shuffle_vector]

        res_1d <- hclust1d(x, method=tested_method)

        squared <- FALSE
        distance <- dist(x)
        if (tested_method %in% c("centroid", "median", "ward.D")) {
          squared <- TRUE
          distance <- distance^2
        }

        res_1d_dist <- hclust1d(distance, distance = TRUE, squared = squared, method=tested_method)
        res_1d_alt <- hclust1d(-x, method=tested_method)
        res_md <- stats::hclust(distance, method=stats_hlust_method)

        expect_all_equalities(res_1d, res_1d_alt, res_1d_dist)
        expect_some_equalities(res_1d, res_md)

        #because of many heights==0 merge levels, we don't know the order of singleton merge. Fix it
        order_1d <- get_order_double(res_1d)
        order_md <- get_order_double(res_md)

      #   ALWAYS x == x[order(x)][order(order(x))] , order(order(x)) is the inverse permutation of order(x)

        for (i in 1:nrow(res_1d$merge)) {  #the condition is more complex here
          # the corresponding steps are order_1d[i] and order_md[i]
          for (j in 1:2) {
            if (res_1d$merge[order_1d[i], j] >= 1) {  # a cluster merged in a previous algorithm step

              found <- FALSE   #seek the the step number within the hclust results for the corresponding step
              for (k in 1:2) {
                if (res_md$merge[order_md[i], k] >= 1) {
                  if (order(order_1d)[res_1d$merge[order_1d[i], j]] == order(order_md)[res_md$merge[order_md[i], k]]) {
                    found <-TRUE   # must compare the correspondence of the previous step in between methods
                    # seek the correspondence in inverse permutation of corresponding  orders
                  }
                }
              }

              expect_true(found)
            } else {  # a singleton

              expect_true(res_1d$merge[order_1d[i], j] %in% res_md$merge[order_md[i], ])
            }
          }
        }

      }
    }
  }
})

