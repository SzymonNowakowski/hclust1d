set.seed(0)

for (tested_method in rev(supported_methods())) {
  test_that("points  and distance length 0 and 1 should fail", {
    expect_error(hclust1d(numeric(0), method=tested_method))
    expect_error(hclust1d(1, method=tested_method))
    expect_error(hclust1d(dist(numeric(0)), distance = TRUE, method=tested_method))
    expect_error(hclust1d(dist(numeric(1)), distance = TRUE, method=tested_method))
  })

  test_that("non-numerical points should fail", {
    expect_error(hclust1d("x", method=tested_method))
  })

  test_that("distance not logical or not scalar should fail", {
    expect_error(hclust1d(c(1, 2, 3), distance="yes", method=tested_method))
    expect_error(hclust1d(c(1, 2, 3), distance=c(TRUE, TRUE), method=tested_method))
  })

  test_that("distance matrix not S3 dist class should fail", {
    expect_error(hclust1d(as.matrix(dist(c(1, 2, 3))), distance=TRUE, method=tested_method))
  })


  test_that("checking distance types", {
    #distance method should get carried over to hclust1d result
    for (dist_method in c("euclidean", "maximum", "manhattan", "minkowski"))
      expect_equal(hclust1d(dist(c(1, 2, 3), method=dist_method), distance=TRUE, method=tested_method)$dist.method, dist_method)

    #hclust1d should fail on those two dist_methods
    for (dist_method in c("canberra", "binary"))
      expect_error(hclust1d(dist(c(1, 2, 3), method=dist_method), distance=TRUE, method=tested_method))

  })

  test_that("should preserve names or values or indices of points", {
    expect_equal(hclust1d(c(one=1, two=2, three=-3), method=tested_method)$labels, c("one", "two", "three"))
    expect_equal(hclust1d(c(1.1, 2.3, -2.2), method=tested_method)$labels, c("1.1", "2.3", "-2.2"))
    expect_equal(hclust1d(dist(c(one=1, two=2, three=-3)), distance=TRUE, method=tested_method)$labels, c("one", "two", "three"))
    expect_equal(hclust1d(dist(c(1.1, 2.3, -2.2)), distance=TRUE, method=tested_method)$labels, c("1", "2", "3"))
  })

  expect_some_equalities <- function(res_1, res_2) {
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
    if (nrow(res_1d$merge) >= 3) if (res_1d$merge[1,1] == res_1d_dist$merge[1,1] & res_1d$merge[1,2] == res_1d_dist$merge[1,2] &
        res_1d$merge[2,1] == res_1d_dist$merge[2,1] & res_1d$merge[2,2] == res_1d_dist$merge[2,2] &
        res_1d$merge[3,1] == res_1d_dist$merge[3,1] & res_1d$merge[3,2] == res_1d_dist$merge[3,2]) {
      compare_alt <- FALSE
    }

    if (!compare_alt) {
      #wild swing of luck here!
      expect_equal(res_1d$merge, res_1d_dist$merge)
    } else {
      expect_equal(res_1d_alt$merge, res_1d_dist$merge)
    }

  }

  repetitions <- 1:5

  range <- 2:100
  test_that("equality of results with stats::hclust, a vector without repetitions", {
    for (len in range) {
      for (j in repetitions) {
        x <- rnorm(len)
        res_1d <- hclust1d(x, method=tested_method)
        res_1d_dist <- hclust1d(dist(x), distance = TRUE, method=tested_method)
        res_1d_alt <- hclust1d(-x, method=tested_method)
        res_md <- stats::hclust(dist(x), method=tested_method)

        expect_all_equalities(res_1d, res_1d_alt, res_1d_dist)
        expect_some_equalities(res_1d, res_md)

        for (i in 1:nrow(res_1d$merge))
          expect_setequal(res_1d$merge[i, ], res_md$merge[i, ])
      }
    }
  })

  get_order_double <- function(res) order(vapply(1:nrow(res$merge), function(i) if (min(res$merge[i, ]) < 0) min(res$merge[i, ]) else i, numeric(1)))

  range <- 1:80
  percent <- 0.25
  test_that("equality of results with stats::hclust, a vector with double repetitions", {
    # with double repetitions the clusters comprising of two singletons should be the same as in stats::hclust
    # but may be differently ordered
    for (len in range) {
      for (j in repetitions) {
        x <- rnorm(len)
        cnt_to_add <- max(1, len * percent)

        new_x_indices <- c(1:length(x), 1:cnt_to_add)
        new_x <- x[new_x_indices]

        shuffle_vector <- sample(1:length(new_x), length(new_x), replace = FALSE) #random permutation of new_x

        x <- new_x[shuffle_vector]

        res_1d <- hclust1d(x, method=tested_method)
        res_1d_dist <- hclust1d(dist(x), distance = TRUE, method=tested_method)
        res_1d_alt <- hclust1d(-x, method=tested_method)
        res_md <- stats::hclust(dist(x), method=tested_method)

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
  })

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
        res_1d_dist <- hclust1d(dist(x), distance = TRUE, method=tested_method)
        res_1d_alt <- hclust1d(-x, method=tested_method)
        res_md <- stats::hclust(dist(x), method=tested_method)

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
  })
}
