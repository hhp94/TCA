test_that("split_input", {
  data <- test_data(10, 100, 6, 1, 1, 0.01)
  split_X_no_shuffle <- split_input(X = data$X, n_chunks = 10, shuffle = FALSE)
  split_X_with_shuffle <- split_input(X = data$X, n_chunks = 10, shuffle = TRUE)
  expect_true(length(split_X_no_shuffle) == 10)

  # Concatenate the row names of all the chunks
  get_row_names <- function(d) {
    purrr::reduce(sapply(d, \(x) {
      row.names(x)
    }), c)
  }

  expect_error(split_input(X = data$X, n_chunks = 10000, shuffle = FALSE))
  # Without shuffling, the features are chunked with order retained
  expect_true(all(rownames(data$X) == get_row_names(split_X_no_shuffle)))
  # With shuffling, the features are changed in chunks
  expect_true(any(rownames(data$X) != get_row_names(split_X_with_shuffle)))
})
