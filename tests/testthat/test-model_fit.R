test_that("compare to TCA 1.2.1 fit, typical use", {
  n_test <- 2 # out of maximum number of simulation time
  data <- get_fixture("sim1_simdata.rds")[seq_len(n_test), ]
  exp_1_df <- get_fixture("sim1_exp_1_fit_1_2_1.rds")[seq_len(n_test), ]

  set.seed(unique(data$seed))

  exp_1_df$new_fit <- lapply(
    data$df,
    \(d) {
      tca(X = d$X, W = d$W, C1 = d$C1, C2 = d$C2)
    }
  )

  exp_1_df$results <- purrr::map2_lgl(exp_1_df$fit_1_2_1, exp_1_df$new_fit, compare_fit_exact)
  expect_true(all(exp_1_df$results))
})

test_that("compare to TCA 1.2.1 fit, vars.mle = TRUE", {
  n_test <- 2 # out of maximum number of simulation time
  data <- get_fixture("sim1_simdata.rds")[seq_len(n_test), ]
  exp_2_df <- get_fixture("sim1_exp_2_fit_1_2_1.rds")[seq_len(n_test), ]

  set.seed(unique(data$seed))
  exp_2_df$new_fit <- lapply(
    data$df,
    \(d) {
      tca(X = d$X, W = d$W, C1 = d$C1, C2 = d$C2, vars.mle = TRUE)
    }
  )

  exp_2_df$results <- purrr::map2_lgl(exp_2_df$fit_1_2_1, exp_2_df$new_fit, compare_fit_exact)
  expect_true(all(exp_2_df$results))
})

test_that("compare to TCA 1.2.1 fit, refit_W = TRUE", {
  n_test <- 2 # out of maximum number of simulation time
  data <- get_fixture("sim1_simdata.rds")[seq_len(n_test), ]
  exp_3_df <- get_fixture("sim1_exp_3_fit_1_2_1.rds")[seq_len(n_test), ]

  set.seed(unique(data$seed))
  exp_3_df$new_fit <- lapply(
    data$df,
    \(d) {
      tca(
        X = d$X, W = d$W, C1 = d$C1, C2 = d$C2, constrain_mu = TRUE,
        refit_W = TRUE, refit_W.features = rownames(d$X)
      )
    }
  )

  exp_3_df$results <- purrr::map2_lgl(exp_3_df$fit_1_2_1, exp_3_df$new_fit, compare_fit_exact)
  expect_true(all(exp_3_df$results))
})


test_that("Verify that p-values are not returned for gamma and delta of constrain_mu == TRUE", {
  skip_on_cran()

  n <- 20
  m <- 30
  k <- 3
  tau <- 0.01
  p1 <- 2
  p2 <- 2
  res <- test_data(n, m, k, p1, p2, tau, log_file = NULL)

  tca.mdl <- tca(X = res$X, W = res$W, C1 = res$C1, C2 = res$C2, refit_W = FALSE, parallel = FALSE, log_file = NULL, constrain_mu = TRUE)
  print(tca.mdl$gammas_hat_pvals)
  print(tca.mdl$deltas_hat_pvals)
  expect_equal(is.null(tca.mdl$gammas_hat_pvals), TRUE)
  expect_equal(is.null(tca.mdl$deltas_hat_pvals), TRUE)

  tca.mdl <- tca(X = res$X, W = res$W, C1 = res$C1, C2 = res$C2, refit_W = FALSE, parallel = FALSE, log_file = NULL, constrain_mu = FALSE)
  expect_equal(nrow(tca.mdl$gammas_hat_pvals) == m & ncol(tca.mdl$gammas_hat_pvals) == p1 * k, TRUE)
  expect_equal(nrow(tca.mdl$deltas_hat_pvals) == m & ncol(tca.mdl$deltas_hat_pvals) == p2, TRUE)
})

test_that("Verify that setting vars.mle to either TRUE or FALSE provides similar results", {
  skip_on_cran()

  n <- 500
  m <- 30
  k <- 3
  tau <- 0.01
  p1 <- 2
  p2 <- 2
  res <- test_data(n, m, k, p1, p2, tau, log_file = NULL)

  tca.mdl1 <- tca(X = res$X, W = res$W, C1 = res$C1, C2 = res$C2, refit_W = FALSE, parallel = FALSE, log_file = NULL, constrain_mu = TRUE, vars.mle = TRUE)
  tca.mdl2 <- tca(X = res$X, W = res$W, C1 = res$C1, C2 = res$C2, refit_W = FALSE, parallel = FALSE, log_file = NULL, constrain_mu = TRUE, vars.mle = FALSE)

  parameters <- c("mus_hat", "sigmas_hat", "deltas_hat", "gammas_hat")
  for (params in parameters) {
    expect_equal(cor(Reshape(tca.mdl1[[params]], ncol(tca.mdl1[[params]]) * nrow(tca.mdl1[[params]])), Reshape(tca.mdl2[[params]], ncol(tca.mdl2[[params]]) * nrow(tca.mdl2[[params]])))[1] > 0.9, TRUE)
  }

  # do not constrain mu this time
  tca.mdl1 <- tca(X = res$X, W = res$W, C1 = res$C1, C2 = res$C2, refit_W = FALSE, parallel = FALSE, log_file = NULL, constrain_mu = FALSE, vars.mle = TRUE)
  tca.mdl2 <- tca(X = res$X, W = res$W, C1 = res$C1, C2 = res$C2, refit_W = FALSE, parallel = FALSE, log_file = NULL, constrain_mu = FALSE, vars.mle = FALSE)

  for (params in parameters) {
    expect_equal(cor(Reshape(tca.mdl1[[params]], ncol(tca.mdl1[[params]]) * nrow(tca.mdl1[[params]])), Reshape(tca.mdl2[[params]], ncol(tca.mdl2[[params]]) * nrow(tca.mdl2[[params]])))[1] > 0.9, TRUE)
  }
})
