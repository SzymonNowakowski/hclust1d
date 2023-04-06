test_that("points length 0 should fail", {
  expect_error(hclust1d(numeric(0), method="single"))
})

test_that("points length 1 should fail", {
  expect_error(hclust1d(1, method="single"))
})

test_that("non-numerical points should fail", {
  expect_error(hclust1d("x", method="single"))
})

test_that("distance not logical should fail", {
  expect_error(hclust1d(c(1, 2, 3), distance="yes", method="single"))
})

test_that("distance not scalar should fail", {
  expect_error(hclust1d(c(1, 2, 3), distance=c(T, T), method="single"))
})

repetitions <- 1:5
range <- 2:100
test_that("equality of results with hclust", {
  for (length in range) {
    for (j in repetitions) {
      x <- rnorm(length)
      res_1d <- hclust1d(x, method="single")
      res_md <- hclust(dist(x), method="single")

      expect_equal(res_1d$dist.method, res_md$dist.method)
      expect_equal(res_1d$method, res_md$method)
      expect_equal(nrow(res_1d$merge), nrow(res_md$merge))
      for (i in 1:nrow(res_1d$merge))
        expect_setequal(res_1d$merge[i, ], res_md$merge[i, ])
      expect_equal(res_1d$height, res_md$height)

      expect_s3_class(res_1d, class(res_md))
    }
  }
})
