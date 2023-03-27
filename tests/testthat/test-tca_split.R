test_that("split_input", {
  n_features <- 100
  data <- test_data(50, n_features, 3, 2, 2, 0.01)
  split_X_no_shuffle <- split_input(X = data$X, n_chunks = 10, shuffle = FALSE)
  split_X_with_shuffle <- split_input(X = data$X, n_chunks = 10, shuffle = TRUE)
  expect_true(length(split_X_no_shuffle) == 10)

  # Concatenate the row names of all the chunks
  get_row_names <- function(d) {
    purrr::reduce(sapply(d, \(x) { row.names(x) }), c)
  }
  expect_error(split_input(X = data$X, n_chunks = 1, shuffle = FALSE))
  expect_error(split_input(X = data$X, n_chunks = 10000, shuffle = FALSE))
  
  # Without shuffling, the features are chunked with order retained
  expect_true(all(row.names(data$X) == get_row_names(split_X_no_shuffle)))
  # With shuffling, the features are changed in chunks
  expect_true(any(row.names(data$X) != get_row_names(split_X_with_shuffle)))
  
  # With shuffling, the features are changed in chunks, but row names the same
  expect_true(all(
    sort(row.names(data$X)) == sort(get_row_names(split_X_with_shuffle)))
  )
})

test_that("tca_split works", {
  data <- test_data(30, 500, 6, 1, 1, 0.01)
  split_X <- split_input(X = data$X, n_chunks = 2, shuffle = TRUE)

  tca_par <- tca_split(
    X = split_X, W = data$W, C1 = data$C1, C2 = data$C2, log_file_prefix = NULL
  )

  tca_seq <- tca(
    X = data$X, W = data$W, C1 = data$C1, C2 = data$C2,
    log_file = NULL, verbose = FALSE, debug = FALSE
  )

  print(compare_fit_corr(tca_par, tca_seq))

  # Might fail by chance
  ## Expect that correlation of estimates between sequential and tca_split to be
  ## correlated at at least 0.99
  expect_true(all(unlist(sapply(compare_fit_corr(tca_par, tca_seq), \(x) {
    x > 0.99
  }))))

  ## Expect that the tau_hat estimation difference is at least < 0.01
  expect_true(abs(mean(tca_par$tau_hat) - tca_seq$tau_hat) < 0.001)
})

test_that("tca_split logging", {
  data <- test_data(30, 100, 6, 1, 1, 0.01)
  split_X <- split_input(X = data$X, n_chunks = 2, shuffle = FALSE)

  temp <- tempdir()
  prefix <- paste(sample(letters, size = 15), collapse = "")

  tca_split_fit <- purrr::partial(
    tca_split,
    X = split_X, W = data$W, C1 = data$C1, C2 = data$C2
  )

  # Test if setting log_file_prefix to NULL stops printing out logs
  tca.mdl <- tca_split_fit(log_file_prefix = NULL)
  expect_true(all(!grepl(prefix, list.files(temp))))

  # Test if setting log_file_prefix works
  tca.mdl <- tca_split_fit(log_file_prefix = paste(temp, prefix, sep = "/"))
  expect_true(any(grepl(prefix, list.files(temp))))

  unlink(list.files(temp, full.names = TRUE)[grepl(prefix, list.files(temp))])
})
