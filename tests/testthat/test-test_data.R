# Test test_data ---------------------------------------
test_that("generate test data", {
  n <- 200
  m <- 100
  k <- 3
  tau <- 0.01
  p1 <- 2
  p2 <- 2
  res <- test_data(n, m, k, p1, p2, tau, log_file = NULL)

  X <- res$X
  W <- res$W
  mus <- res$mus
  sigmas <- res$sigmas
  C1 <- res$C1
  C2 <- res$C2
  gammas <- res$gammas
  deltas <- res$deltas

  tca.mdl <- tca(X, W, C1 = C1, C2 = C2, refit_W = TRUE, refit_W.sparsity = nrow(X), parallel = FALSE, log_file = NULL)
  Z_hat <- tensor(X, tca.mdl, log_file = NULL)
  expect_true("matrix" %in% class(Z_hat[[1]]))
  expect_true("matrix" %in% class(Z_hat[[2]]))
  expect_true("matrix" %in% class(Z_hat[[3]]))
  
  expect_type(Z_hat[[1]], "double")
  expect_type(Z_hat[[2]], "double")
  expect_type(Z_hat[[3]], "double")
})

