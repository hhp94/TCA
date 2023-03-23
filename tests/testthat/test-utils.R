test_that("summary_fastLm ", {
  n <- 10000
  p <- 10
  p_null <- 5
  
  near <- function(x, y, tol = .Machine$double.eps^0.5) {
    abs(x - y) < tol
  }

  diff <- replicate(100, {
    y <- rnorm(n)
    X <- matrix(rnorm(n * p), ncol = p)
    X_null <- X[, sample.int(p, p_null)]
    
    df <- cbind(data.frame(y = y), X)
    df_null <- cbind(data.frame(y = y), X_null)
    
    lm_null <- lm(y ~ ., data = df)
    lm_full <- lm(y ~ ., data = df_null)
    lm_anova <- anova(lm_null, lm_full)

    lmF_null <- RcppEigen::fastLm(X = cbind(1, X_null), y = y)
    lmF_full <- RcppEigen::fastLm(X = cbind(1, X), y = y)
    lmF_anova <- fastLM_ftest(lmF_null, lmF_full)
    
    all(near(lm_anova$F[2], lmF_anova$F),
        near(lm_anova$`Pr(>F)`[2], lmF_anova$`Pr(>F)`))
  })
  expect_true(all(diff))

  # No intercept
  diff_2 <- replicate(100, {
    y <- rnorm(n)
    X <- matrix(rnorm(n * p), ncol = p)
    X_null <- X[, sample.int(p, p_null)]
    
    df <- cbind(data.frame(y = y), X)
    df_null <- cbind(data.frame(y = y), X_null)
    
    lm_null <- lm(y ~ 0 + ., data = df)
    lm_full <- lm(y ~ 0 + ., data = df_null)
    lm_anova <- anova(lm_null, lm_full)

    lmF_null <- RcppEigen::fastLm(X = X_null, y = y)
    lmF_full <- RcppEigen::fastLm(X = X, y = y)
    lmF_anova <- fastLM_ftest(lmF_null, lmF_full)
    
    all(near(lm_anova$F[2], lmF_anova$F),
        near(lm_anova$`Pr(>F)`[2], lmF_anova$`Pr(>F)`))
  })
  expect_true(all(diff_2))
})
