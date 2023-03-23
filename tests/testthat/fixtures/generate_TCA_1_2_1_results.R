# Generate the results created by TCA 1_2_1.
## Create data n times per scenario. Fit the results with TCA 1_2_1 algorithm.
## This result will be used to check if future changes modifies anything
## unexpectedly.

warning("This shouldn't run in build test")
use_package("furrr")
use_package("tibble", type = "Suggests")

n_dat <- 5
my_seed <- 1234
data <- tibble::tibble(id = seq_len(n_dat))
fitted <- data[, which(names(data) == "id")]

# Scenario 1, typical use ----------------------------------------------

set.seed(my_seed)
hashes <- rep(1, times = n_dat)
while (length(hashes) != length(unique(hashes))) {
  data$df <- lapply(data$id, \(x) {
    TCA::test_data(35, 3000, 3, 2, 2, 0.01, log_file = NULL)
  })
  hashes <- sapply(data$df, rlang::hash)
}
data$seed <- my_seed

set.seed(my_seed)
library(furrr)
plan(multisession, workers = 5)

fitted$fit_1_2_1 <- future_map(
  data$df,
  \(d) {
    tca(
      X = d$X,
      W = d$W,
      C1 = d$C1,
      C2 = d$C2
    )
  },
  .options = furrr_options(seed = TRUE, packages = "TCA"),
  .progress = TRUE
)

plan(sequential)

saveRDS(data, test_path("fixtures", "exp1_simdata.rds"))
saveRDS(fitted, test_path("fixtures", "exp1_simdata_fit_1_2_1.rds"))
# Scenario 2, something else ----------------------------------------------
