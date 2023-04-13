
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

get_links_triple <- function(res) {
  # finds links between 0-height singleton-only clusters and the second cluster in which a third singleton is added with height 0
  links <- vapply(1:nrow(res$merge), function(i) if ((res$height[i] == 0) & (res$merge[i, 1] * res$merge[i, 2] < 0)) max(res$merge[i, ]) else 0, numeric(1))
  #add inverse links
  for (i in 1:length(links)) {
    if (links[i] > 0) {
      links[links[i]] <- i
    }
  }
  return(links)
}

get_order_triple <- function(links, res) {
  #the way it works is the following:
  #for the two-singleton 0-height clusters it returns the smallest of 2 or 3 smallest involved singleton indices
  #for the singleton and normal cluster 0-height clusters, it returns the highest of 3 involved singletons
  vec <- 1:nrow(res$merge)
  for (i in 1:nrow(res$merge)) {

    second <- i
    if (links[i] > 0)
      second <- links[i]

    if (res$height[i] == 0) {
      if (res$merge[i, 1] * res$merge[i, 2] > 0) {   #two singletons
        vec[i] <- min(c(res$merge[i, ], res$merge[second, ]))   #possibly, second==i if there is no 3rd value
      } else for (j in 1:2) if (res$merge[i, j] < 0) {   #one singleton and a cluster, so for sure 3 values involves
        vec[i] <- max(c(res$merge[i, j], res$merge[second, ]))
      }
    }

  }
  return(order(vec))
}

range <- 1:50
percent_double <- 0.7
percent_triple <- 0.3
test_that("equality of results with stats::hclust, a vector with triple repetitions", {
  # with triple repetitions the clusters comprising of two singletons should be the same as in stats::hclust
  # but may be differently ordered
  # and in the case of a merge of those clusters with a singleton - this singleton must be
  # checked against the original index

  # with more than triple repetitions it would become intractable in principle,
  # what would be the order of creation of larger clusters
  for (tested_method in c(supported_methods(), "single_implemented_by_heap")[-4]) {  #without a test for true_median
    stats_hlust_method <- tested_method
    if (tested_method == "single_implemented_by_heap")
      stats_hlust_method <- "single"
    for (len in range) {
      for (j in repetitions) {
        x <- rnorm(len)
        cnt_double <- max(1, len * percent_double)
        cnt_triple <- max(1, len * percent_triple)
        new_x_indices <- c(1:length(x), 1:cnt_double, 1:cnt_triple)

        new_x <- x[new_x_indices]

        shuffle_vector <- sample(1:length(new_x), length(new_x), replace = FALSE) #random permutation of new_x

        x <- new_x[shuffle_vector]
        x_indices <- new_x_indices[shuffle_vector]

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

        expect_all_equalities(res_1d, res_1d_alt, res_1d_dist)
        expect_some_equalities(res_1d, res_md)


        #because of many heights==0 merge levels, we don't know the order of singleton merge. Fix it
        #the merge of two singletons will get the (equal) triple minimal value
        #the merge of a singleton and a cluster will get the (equal) triple maximal value
        links_1d <- get_links_triple(res_1d)
        links_md <- get_links_triple(res_md)

        order_1d <- get_order_triple(links_1d, res_1d)
        order_md <- get_order_triple(links_md, res_md)

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
              found <- FALSE   #seek the the singleton within the hclust results
              for (k in 1:2)
                if (res_md$merge[order_md[i], k] < 0) {
                  if (x_indices[-res_1d$merge[order_1d[i], j]] == x_indices[-res_md$merge[order_md[i], k]]) { # but that this singleton value
                    # may be the same as other copied singleton values, so compare the original indices values
                    found <- TRUE
                  }
                }

              expect_true(found)
            }
          }
        }
      }
    }
  }
})

