# Wrapper around the assets folder for matlab data.
assets_p <- function(...) {
  test_path("assets", ...)
}

# Wrapper for running ewas used in test-TCA.R
run_ewas <- function(X, W, tca.mdl, yfile, test, fast_mode = FALSE) {
  Y <- as.matrix(read.table(assets_p("ewas", yfile), header = FALSE, sep = ","))
  rownames(Y) <- 1:nrow(Y)
  m <- ncol(Y)
  k <- ncol(W)
  tca.mdl2 <- tca.mdl
  pvals <- matrix(0, m, 1)
  if (test == "marginal" | test == "marginal_conditional") pvals <- matrix(0, m, k)
  for (j in 1:m) {
    tca.mdl2[["mus_hat"]] <- t(as.matrix(tca.mdl[["mus_hat"]][j, ]))
    tca.mdl2[["sigmas_hat"]] <- t(as.matrix(tca.mdl[["sigmas_hat"]][j, ]))
    tca.mdl2[["deltas_hat"]] <- t(as.matrix(tca.mdl[["deltas_hat"]][j, ]))
    tca.mdl2[["gammas_hat"]] <- t(as.matrix(tca.mdl[["gammas_hat"]][j, ]))
    y <- Y[, j, drop = F]
    x <- t(X[, j, drop = F])
    rownames(y) <- 1:length(y)
    if (test == "custom") {
      res <- tcareg(X = x, tca.mdl = tca.mdl2, y = y, test = test, save_results = FALSE, null_model = c("3"), alternative_model = c("1", "2", "3"), log_file = NULL, fast_mode = fast_mode, verbose = FALSE)
    } else {
      res <- tcareg(X = x, tca.mdl = tca.mdl2, y = y, test = test, save_results = FALSE, log_file = NULL, fast_mode = fast_mode, verbose = FALSE)
    }
    if (test == "single_effect" | test == "custom" | test == "joint") pvals[j, ] <- res$pval[1]
    if (test == "marginal" | (fast_mode == FALSE & test == "marginal_conditional")) for (h in 1:k) pvals[j, h] <- res[[h]]$pval[1]
    if (fast_mode == TRUE & test == "marginal_conditional") pvals[j, ] <- res$pval
  }
  return(pvals)
}

# Check if values are close within tolerance
near <- function(x, y, tol = .Machine$double.eps^0.5) {
  # FROM dplyr PACKAGE
  abs(x - y) < tol
}

# Wrapper for which sim object to get for reproducing 1.2.1 fits
get_fixture <- function(sim) {
  readRDS(test_path("fixtures", sim))
}

# For comparing fitted values of tca runs
compare_fit_exact <- function(o, n) {
  all(sapply(names(o), \(x) {
    all(near(o[[x]], n[[x]]))
  }))
}

# compare_fit_corr <- 1 # Compare correlation of params instead of actual value
