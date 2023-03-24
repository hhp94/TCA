# Generate the results created by TCA 1_2_1.
## Create data n times per scenario. Fit the results with TCA 1_2_1 algorithm.
## This result will be used to check if future changes modifies anything
## unexpectedly.

warning("This shouldn't run in build test")
use_package("furrr")
use_package("dplyr", type = "Suggests")

n_dat <- 5 # N data set per scenario
my_seed <- 1234
set.seed(my_seed)
data <- dplyr::tibble(id = seq_len(n_dat)) # Store generated data

# Simulation 1 ---------------------------------------------------------
data$df <-
  list(
    TCA::test_data(30, 2000, 3, 2, 2, 0.01, log_file = NULL),
    TCA::test_data(30, 2000, 4, 1, 2, 0.01, log_file = NULL),
    TCA::test_data(30, 2000, 4, 3, 2, 0.01, log_file = NULL),
    TCA::test_data(30, 2000, 3, 2, 1, 0.01, log_file = NULL),
    TCA::test_data(30, 2000, 3, 2, 1, 0.02, log_file = NULL)
  )

data$seed <- my_seed
saveRDS(data, test_path("fixtures", "sim1_simdata.rds"))

# Scenario 1, typical use ----------------------------------------------
fitted <- data[, which(names(data) == "id")] # Store fitted object

set.seed(my_seed)
library(furrr)
plan(multisession, workers = 5)

fitted$fit_1_2_1 <- future_map(
  data$df,
  \(d) {tca( X = d$X, W = d$W, C1 = d$C1, C2 = d$C2, verbose = FALSE) },
  .options = furrr_options(seed = TRUE, packages = "TCA"),
  .progress = TRUE
)

plan(sequential)
saveRDS(fitted, test_path("fixtures", "sim1_exp1_fit_1_2_1.rds"))

# Scenario 2, vars.mle = TRUE ----------------------------------------------
fitted_mle <- data[, which(names(data) == "id")] # Store fitted object

set.seed(my_seed)
plan(multisession, workers = 5)

fitted_mle$fit_1_2_1 <- future_map(
  data$df,
  \(d) {tca( X = d$X, W = d$W, C1 = d$C1, C2 = d$C2, vars.mle = TRUE, verbose = FALSE) },
  .options = furrr_options(seed = TRUE, packages = "TCA"),
  .progress = TRUE
)

plan(sequential)
saveRDS(fitted_mle, test_path("fixtures", "sim1_exp2_fit_1_2_1.rds"))
