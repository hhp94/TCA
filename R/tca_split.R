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
split_input <- function(X, n_chunks = 2, shuffle = TRUE) {
  assert(is.matrix(X), "X must be a matrix")
  assert(n_chunks > 1, "Must split into more than 1 chunk")
  n_features <- nrow(X)
  assert(n_features >= n_chunks, "Number of features must be greater than number of chunks")
  feat_names <- row.names(X)
  assert(!is.null(feat_names), "X must have row names")
  assert(length(feat_names) == length(unique(feat_names)), "Row names of X must be unique")

  if (n_features <= ncol(X)) {
    warning("Typically, there would be more features than observations")
  }

  if (shuffle) {
    feat_names <- sample(feat_names, n_features)
  }
  
  id <- lapply(parallel::splitIndices(n_features, n_chunks), \(x) {
      feat_names[x]
    })
  
  assert(all(sort(feat_names) == sort(unlist(id))), "Unexpected splitting or row.names")

  res <- lapply(id, \(r) { X[r, , drop = FALSE] })

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
#' @param log_file_prefix prefix of the logs of tca() for each chunk of `X_split`.
#' For example "f1/log" would store the logs in a folder called "f1" with the
#' prefix "log". NULL to suppress logging
#' @param furrr_opt list of arguments passed to [furrr::furrr_options()]
#' @param ... arguments passed to [furrr::future_map()]
#' @inheritParams tca
#'
#' @inherit tca return details
#' @export
#'
#' @examples
#' data <- test_data(30, 1000, 6, 1, 1, 0.01)
#' split_X <- split_input(X = data$X, n_chunks = 5) # split data into 5 chunks
#' # library(furrr)
#' # plan(multisession, workers = 5) # use 5 cores in a multiple session scheme
# tca.mdl <- tca_split(
#   X = split_X, W = data$W, C1 = data$C1, C2 = data$C2, log_file_prefix = "log/log"
# )
#' # plan(sequential)
tca_split <- function(
    X_split, W, C1 = NULL, C1.map = NULL, C2 = NULL, tau = NULL, vars.mle = FALSE,
    constrain_mu = FALSE, max_iters = 10, log_file_prefix = "tca_split", debug = FALSE,
    verbose = TRUE, furrr_opt = list(seed = TRUE, packages = "TCA", stdout = FALSE), ...) {
  # Input validation
  assert(
    "split_input" %in% class(X_split), "X_split can be obtained with split_input()"
  )
  assert(length(X_split) > 1, "Must have more than 1 chunk")
  assert(is.list(furrr_opt), "furrr_opt must be a list")
  assert(isTRUE(furrr_opt$seed), "furrr_opt must have `seed = TRUE`")
  assert("TCA" %in% furrr_opt$packages, 'furrr_opt must have `packages = c("TCA")`')

  # Pre-fill arguments into tca()
  tca_fns <- purrr::partial(
    tca,
    W = W, C1 = C1, C1.map = C1.map, C2 = C2, tau = tau,
    vars.mle = vars.mle, constrain_mu = constrain_mu, max_iters = max_iters,
    debug = debug, verbose = verbose, W_C1_C2 = FALSE
  )

  # Handling log, try to write to folder in `log_file_prefix`
  if (!is.null(log_file_prefix)) {
    write_test <- paste(log_file_prefix, "writetest", sep = ".")
    tryCatch(
      writeLines("", write_test),
      error = function(cond) { # Error if folder doesn't exist
        stop("Folder for storing logs doesn't exists")
      },
      finally = { # Remove write_test file.
        unlink(write_test)
      }
    ) # Create list of log name for each of the chunk.
    log_list <- sapply(
      seq_along(X_split),
      \(x) {
        paste(log_file_prefix, x, "log", sep = ".")
      }
    )
  } else {
    log_list <- list(NULL)
  }

  # Perform tca fit over chunks
  res <- furrr::future_map2(
    X_split,
    log_list,
    \(d, l) {
      tca_fns(X = d, log_file = l)
    },
    .options = do.call(eval(parse(text = "furrr::furrr_options")), furrr_opt),
    ...
  )

  # Combining the fit results
  res <- purrr::reduce(
    res, \(x, y) {
      purrr::map2(x, y, \(a, b) {
        rbind(a, b)
      })
    },
    .dir = "forward"
  ) |>
    c(list(W = W, C1 = C1, C2 = C2))

  res$tau_hat <- as.numeric(res$tau_hat)

  return(res)
}
