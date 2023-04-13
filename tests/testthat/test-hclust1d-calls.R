
test_that("points and distance length 0 and 1 should fail", {
  set.seed(0)
  for (tested_method in c(supported_methods(), "single_implemented_by_heap")) {
    expect_error(hclust1d(numeric(0), method = tested_method))
    expect_error(hclust1d(1, method = tested_method))
    expect_error(hclust1d(dist(numeric(0)), distance = TRUE, method = tested_method))
    expect_error(hclust1d(dist(numeric(1)), distance = TRUE, method = tested_method))
  }
})

test_that("non-numerical points should fail", {
  for (tested_method in c(supported_methods(), "single_implemented_by_heap")) {
    expect_error(hclust1d("x", method = tested_method))
  }
})

test_that("distance not logical or not scalar should fail", {
  for (tested_method in c(supported_methods(), "single_implemented_by_heap")) {
    expect_error(hclust1d(c(1, 2, 3), distance = "yes", method = tested_method))
    expect_error(hclust1d(c(1, 2, 3), distance = c(TRUE, TRUE), method = tested_method))
  }
})

test_that("squared not logical or not scalar should fail", {
  for (tested_method in c(supported_methods(), "single_implemented_by_heap")) {
    expect_error(hclust1d(c(1, 2, 3), distance = TRUE, squared="yes", method = tested_method))
    expect_error(hclust1d(c(1, 2, 3), distance = TRUE, squared=c(TRUE, TRUE), method = tested_method))
  }
})

test_that("distance matrix not S3 dist class should fail", {
  for (tested_method in c(supported_methods(), "single_implemented_by_heap")) {
    expect_error(hclust1d(as.matrix(dist(c(1, 2, 3))), distance = TRUE, method = tested_method))
  }
})


test_that("checking distance types", {
  for (tested_method in c(supported_methods(), "single_implemented_by_heap")) {
    #distance method should get carried over to hclust1d result
    for (dist_method in c("euclidean", "maximum", "manhattan", "minkowski"))
      expect_equal(hclust1d(dist(c(1, 2, 3), method = dist_method), distance = TRUE, method = tested_method)$dist.method, dist_method)

    #hclust1d should fail on those two dist_methods
    for (dist_method in c("canberra", "binary"))
      expect_error(hclust1d(dist(c(1, 2, 3), method = dist_method), distance = TRUE, method = tested_method))
  }
})

test_that("should preserve names or values or indices of points", {
  for (tested_method in c(supported_methods(), "single_implemented_by_heap")) {
    expect_equal(hclust1d(c(one=1, two=2, three=-3), method = tested_method)$labels, c("one", "two", "three"))
    expect_equal(hclust1d(c(1.1, 2.3, -2.2), method = tested_method)$labels, c("1.1", "2.3", "-2.2"))
    expect_equal(hclust1d(dist(c(one=1, two=2, three=-3)), distance = TRUE, method = tested_method)$labels, c("one", "two", "three"))
    expect_equal(hclust1d(dist(c(1.1, 2.3, -2.2)), distance = TRUE, method = tested_method)$labels, c("1", "2", "3"))
  }
})

test_that("should err on negative square distances", {
  dissimilarity <- dist(c(1, 2, -3))^2
  dissimilarity[2] <- -1
  for (tested_method in c(supported_methods(), "single_implemented_by_heap")) {
    expect_error(hclust1d(dissimilarity, distance = TRUE, squared = TRUE, method = tested_method))
  }
})
