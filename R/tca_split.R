#' Split [tca()] Input into Chunks by X
#'
#' Split the X matrix input into row chunks with equal number of features to
#' facilitate parallel processing over X for [tca()]. Does not support
#' `refit_W = TRUE`.
#'
#' @inheritParams tca
#' @param n_chunks number of chunks to split X into. See Details.
#' @param shuffle randomly shuffle rows of X. This is used to make sure estimates
#' of tau from the data if requested is not confounded by the order of the features
#' if the data is run in chunks of X. Picking number of chunks equal to the
#' number of cores to parallel over is recommended.
#'
#' @details
#' Because of the assumption of independence between features of [tca()], splitting
#' the input of [tca()] into chunks of features can potentially speed up the fit.
#' This is because we would only need to spin up the clusters once, run over all
#' chunks, and combine the result at the end.
#'
#' @return a split_input object that contains chunks of the X matrix by row.
#' @export
#'
#' @examples
#' data <- test_data(30, 1000, 6, 1, 1, 0.01)
#' split_X <- split_input(X = data$X, n_chunks = 10)
#' split_X
#' dim(split_X)
split_input <- function(X, n_chunks = 1, shuffle = TRUE) {
  assert(is.matrix(X), "X must be a matrix")
  n_features <- nrow(X)
  assert(n_features >= n_chunks, "Number of features must be greater than number of chunks")
  feat_names <- row.names(X)
  assert(!is.null(feat_names), "X must have row names")
  assert(length(feat_names) == length(unique(feat_names)), "Row names of X must be unique")

  if (n_features <= ncol(X)) {
    warning("Typically, there would be more features than observations")
  }

  id <- parallel::splitIndices(nrow(X), n_chunks)

  assert(length(unique(purrr::reduce(id, c))) == length(feat_names), "parallel::splitIndices error")

  res <- lapply(id, \(r) {
    if (shuffle) {
      return(X[sample.int(n_features, n_features), ][r, , drop = FALSE])
    } else {
      return(X[r, , drop = FALSE])
    }
    stop("Unexpected error")
  })

  return(structure(res, class = c("split_input", "list")))
}

#' Print a [split_input()] Object
#'
#' This function helps preventing Rstudio from printing out huge matrices
#'
#' @param x a [split_input()] object
#' @param n number of rows to print
#' @param k number of columns to print
#'
#' @export
print.split_input <- function(x, n = 6L, k = 6L) {
  nrows <- sapply(x, nrow)
  ncols <- unique(sapply(x, ncol))
  min_r <- min(c(min(nrows), n))
  min_c <- min(c(ncols, k))
  print(lapply(x, \(x) {
    x[seq_len(min_r), seq_len(min_c)]
  }))
  message(
    paste(
      length(x),
      "chunks of between",
      min(nrows),
      "and",
      max(nrows),
      "features *",
      unique(sapply(x, ncol)),
      "observations"
    )
  )
}

#' Return Dimensions of each Chunk
#'
#' Wrapper for `lapply(x, dim)`
#'
#' @inheritParams print.split_input
#'
#' @return a list of dimensions of each chunk
#'
#' @export
dim.split_input <- function(x) {
  lapply(x, dim)
}

#' Run [tca()] over Chunks of Features
#'
#' Apply [tca()] to chunks of feature X in parallel. `refit_W = TRUE` is not
#' supported.
#'
#' @param X_split output of [split_input()] function.
#' @param furrr_opt list of arguments passed to [furrr::furrr_options()]
#' @param ... arguments passed to [furrr::future_map()]
#' @inheritParams tca
#'
#' @inherit tca return details
#' @export
#'
#' @examples
#' data <- test_data(30, 1000, 6, 1, 1, 0.01)
#' split_X <- split_input(X = data$X, n_chunks = 10)
#' tca.mdl <- tca_split(X = split_X, W = data$W, C1 = data$C1, C2 = data$C2)
tca_split <- function(
    X_split, W, C1 = NULL, C1.map = NULL, C2 = NULL, tau = NULL, vars.mle = FALSE,
    constrain_mu = FALSE, max_iters = 10, log_file = "TCA_split.log", debug = FALSE,
    verbose = TRUE, furrr_opt = list(seed = TRUE, packages = "TCA"), ...) {
  start_logger(log_file, debug, verbose)

  futile.logger::flog.info(
    paste("Starting tca over", length(X_split), "chunks...", sep = " ")
  )

  config <- config::get(file = system.file("extdata", "config.yml", package = "TCA"), use_parent = FALSE)

  assert(
    "split_input" %in% class(X_split),
    "X_split can be obtained with split_input()"
  )

  assert(is.list(furrr_opt), "furrr_opt must be a list")
  assert(isTRUE(furrr_opt$seed), "furrr_opt must have seed = TRUE")
  assert("TCA" %in% c(furrr_opt$packages), "furrr_opt must have packages = c('TCA')")

  fit_fns <- purrr::partial(
    tca,
    W = W, C1 = C1, C1.map = C1.map, C2 = C2, tau = tau,
    vars.mle = vars.mle, constrain_mu = constrain_mu, max_iters = max_iters,
    log_file = NULL, debug = FALSE, verbose = FALSE, W_C1_C2 = FALSE
  )

  res <- furrr::future_map(
    X_split,
    \(d) {
      fit_fns(X = d)
    },
    .options = do.call(eval(parse(text = "furrr::furrr_options")), furrr_opt),
    ...
  )

  res <- purrr::reduce(
    res, \(x, y) {
      purrr::map2(x, y, \(a, b) {
        rbind(a, b)
      })
    },
    .dir = "forward"
  ) |>
    c(list(W = W, C1 = C1, C2 = C2))

  res$tau_hat <- mean(res$tau_hat)

  return(res)
}
