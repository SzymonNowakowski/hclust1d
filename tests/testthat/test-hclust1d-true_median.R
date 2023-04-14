test_that("correct true_median linkage results", {
  expect_equal(hclust1d(c(1, 2, 4, 6, 11, -9, -4, -5), method="true_median")$height, c(1.0, 1.0, 2.0, 3.5, 4.5, 8.0, 10.0))
})
