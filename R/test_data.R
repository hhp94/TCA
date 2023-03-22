#' @title Generate test data
#'
#' @description Generates simple test data following the TCA model.
#'
#' @param n The number of observations to simulate.
#' @param m The number of features to simulate.
#' @param k The number of sources to simulate.
#' @param p1 The number of covariates that affect the source-specific values to simulate.
#' @param p2 The number of covariates that affect the mixture values to simulate.
#' @param tau The variance of the i.i.d. component of variation to add on top of the simulated mixture values.
#' @param log_file A path to an output log file. Note that if the file \code{log_file} already exists then logs will be appended to the end of the file. Set \code{log_file} to \code{NULL} to prevent output from being saved into a file; note that if \code{verbose == FALSE} then no output file will be generated regardless of the value of \code{log_file}.
#' @param verbose A logical value indicating whether to print logs.
#'
#' @importFrom futile.logger flog.info
#' @importFrom pracma Reshape
#'
#' @details See \link{tca} for details about the TCA model.
#'
#' @return A list with the simulated data and parameters.
#' \item{X}{An \code{m} by \code{n} matrix of simulated data with \code{m} features for \code{n} observations.}
#' \item{Z}{A list with the simulated source-specific values, where the first element in the list is an \code{m} by \code{n} matrix (features by observations) corresponding to the values coming from the first source, the second element in the list is another \code{m} by \code{n} matrix (features by observations) corresponding to the values coming from the second source and so on.}
#' \item{W}{An \code{n} by \code{k} matrix of simulated weights - the weights of the \code{k} sources for each of the \code{n} mixtures (observations).}
#' \item{mus}{An \code{m} by \code{k} matrix of the mean of each of the \code{m} features for each of the \code{k} sources.}
#' \item{sigmas}{An \code{m} by \code{k} matrix of the standard variation of each of the \code{m} features for each of the \code{k} sources.}
#' \item{C1}{ An \code{n} by \code{p1} design matrix of simulated covariates that affect the hidden source-specific values.}
#' \item{C2}{ An \code{n} by \code{p2} design matrix of simulated covariates that affect the mixture.}
#' \item{gammas}{An \code{m} by \code{k*p1} matrix of the effects of the \code{p1} covariates in \code{C1} on each of the \code{m} features in \code{X}, where the first \code{p1} columns are the source-specific effects of the \code{p1} covariates on the first source, the following \code{p1} columns are the source-specific effects on the second source and so on.}
#' \item{deltas}{An \code{m} by \code{p2} matrix of the effects of the \code{p2} covariates in \code{C2} on the mixture values of each of the \code{m} features in \code{X}.}
#'
#' @examples
#' data <- test_data(100, 50, 3, 2, 2, 0.01)
#'
#' @export test_data
#'
test_data <- function(n, m, k, p1, p2, tau, log_file = "TCA.log", verbose = TRUE) {
  start_logger(log_file, FALSE, verbose)

  flog.info("Starting test_data...")

  mus <- Reshape(runif(m * k, min = 0, max = 1), m, k)
  sigmas <- Reshape(runif(m * k, min = 0, max = 0.2), m, k)
  W <- Reshape(runif(n * k, min = 0, max = 1), n, k)
  W <- W / t(repmat(rowSums(W), k, 1)) # normalize so the proportions are in the range [0,1] and sum up to 1 for each observation.
  C1 <- Reshape(rnorm(n * p1, mean = 0, sd = 1), n, p1)
  C2 <- Reshape(rnorm(n * p2, mean = 0, sd = 1), n, p2)
  gammas <- Reshape(rnorm(m * p1 * k, mean = 0, sd = 0.1), m, p1 * k)
  deltas <- Reshape(rnorm(m * p2, mean = 0, sd = 0.1), m, p2)
  Z <- list()
  for (h in 1:k) {
    Z[[h]] <- matrix(0, n, m)
  }
  for (i in 1:n) {
    for (h in 1:k) {
      Z[[h]][i, ] <- rnorm(m, mean = mus[, h], sd = sigmas[, h])
      if (p1) {
        Z[[h]][i, ] <- Z[[h]][i, ] + C1[i, ] %*% t(gammas[, ((h - 1) * p1 + 1):(p1 * h)])
      }
    }
  }
  X <- matrix(0, n, m)
  for (h in 1:k) {
    X <- X + Z[[h]] * t(repmat(W[, h], m, 1))
  }
  if (p2) {
    X <- X + C2 %*% t(deltas)
  }
  # add i.i.d. component of variation
  X <- X + Reshape(rnorm(n * m, mean = 0, sd = tau), n, m)
  X <- t(X)
  for (h in 1:k) {
    Z[[h]] <- t(Z[[h]])
    rownames(Z[[h]]) <- 1:m
    colnames(Z[[h]]) <- 1:n
  }
  colnames(X) <- 1:n
  rownames(X) <- 1:m
  colnames(W) <- 1:k
  rownames(W) <- 1:n
  colnames(mus) <- colnames(W)
  colnames(sigmas) <- colnames(W)
  rownames(mus) <- 1:m
  rownames(sigmas) <- 1:m
  if (p1) {
    rownames(C1) <- 1:n
    colnames(C1) <- 1:dim(C1)[2]
    rownames(gammas) <- 1:m
    colnames(gammas) <- 1:dim(gammas)[2]
  }
  if (p2) {
    rownames(C2) <- 1:n
    colnames(C2) <- 1:dim(C2)[2]
    rownames(deltas) <- 1:m
    colnames(deltas) <- 1:dim(deltas)[2]
  }

  flog.info("Finished test_data.")
  return(list("X" = X, "Z" = Z, "W" = W, "mus" = mus, "sigmas" = sigmas, "C1" = C1, "C2" = C2, "gammas" = gammas, "deltas" = deltas))
}
