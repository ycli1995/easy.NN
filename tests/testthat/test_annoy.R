test_that("future annoy works", {
  data(pca)

  nn1 <- annoyNN(pca, 9)
  nn2 <- annoyNN(pca, 9, n.threads = 4, chunk.size = 10)

  expect_equal(nn1$idx, nn2$idx)
  expect_equal(nn1$dist, nn2$dist)
})
