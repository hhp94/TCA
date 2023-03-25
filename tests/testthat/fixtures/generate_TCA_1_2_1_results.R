# Generate the results created by TCA 1_2_1.
## Create data n times per scenario. Fit the results with TCA 1_2_1 algorithm.
## This result will be used to check if future changes modifies anything
## unexpectedly.

library(furrr)
library(TCA)
warning("This shouldn't run in build test")
use_package("furrr")
use_package("dplyr", type = "Suggests")

n_dat <- 5 # N data set per scenario
my_seed <- 1234
set.seed(my_seed)
data <- dplyr::tibble(id = seq_len(n_dat)) # Store generated data

# Simulation 1 ---------------------------------------------------------
## Make sure c1 >= 2 to also be able to test tcareg
data$df <-
  list(
    TCA::test_data(25, 1000, 3, 2, 2, 0.01, log_file = NULL),
    TCA::test_data(25, 1000, 4, 2, 2, 0.01, log_file = NULL),
    TCA::test_data(25, 1000, 4, 3, 2, 0.01, log_file = NULL),
    TCA::test_data(25, 1000, 3, 3, 3, 0.01, log_file = NULL),
    TCA::test_data(25, 1000, 3, 4, 4, 0.02, log_file = NULL)
  )

data$seed <- my_seed
saveRDS(data, test_path("fixtures", "sim1_simdata.rds"))

# Scenario 1, typical use ----------------------------------------------
data <- readRDS(test_path("fixtures", "sim1_simdata.rds"))
fitted <- data[, which(names(data) == "id")] # Store fitted object

set.seed(unique(data$seed))
plan(multisession, workers = 5)

fitted$fit_1_2_1 <- future_map(
  data$df,
  \(d) {
    tca(X = d$X, W = d$W, C1 = d$C1, C2 = d$C2, verbose = FALSE)
  },
  .options = furrr_options(seed = TRUE, packages = "TCA")
)

plan(sequential)
saveRDS(fitted, test_path("fixtures", "sim1_exp_1_fit_1_2_1.rds"))

# Scenario 2, vars.mle = TRUE ----------------------------------------------
data <- readRDS(test_path("fixtures", "sim1_simdata.rds"))
fitted_mle <- data[, which(names(data) == "id")] # Store fitted object

set.seed(unique(data$seed))
plan(multisession, workers = 5)

fitted_mle$fit_1_2_1 <- future_map(
  data$df,
  \(d) {
    tca(X = d$X, W = d$W, C1 = d$C1, C2 = d$C2, vars.mle = TRUE, verbose = FALSE)
  },
  .options = furrr_options(seed = TRUE, packages = "TCA")
)

plan(sequential)
saveRDS(fitted_mle, test_path("fixtures", "sim1_exp_2_fit_1_2_1.rds"))

# Scenario 3, refit_W = TRUE ----------------------------------------------
data <- readRDS(test_path("fixtures", "sim1_simdata.rds"))
fitted_refit_W <- data[, which(names(data) == "id")] # Store fitted object

set.seed(unique(data$seed))
plan(multisession, workers = 5)

fitted_refit_W$fit_1_2_1 <- future_map(
  data$df,
  \(d) {
    tca(
      X = d$X, W = d$W, C1 = d$C1, C2 = d$C2, constrain_mu = TRUE,
      refit_W = TRUE, refit_W.features = rownames(d$X), verbose = FALSE
    )
  },
  .options = furrr_options(seed = TRUE, packages = "TCA")
)

plan(sequential)
saveRDS(fitted_refit_W, test_path("fixtures", "sim1_exp_3_fit_1_2_1.rds"))

# Scenario 4, tcareg fit ----------------------------------------------
data <- readRDS(test_path("fixtures", "sim1_simdata.rds"))
tcareg_fit <- data[, which(names(data) == "id")] # Store fitted object

set.seed(unique(data$seed))
plan(multisession, workers = 5)

tcareg_fit$fit_1_2_1 <- future_map(
  data$df,
  \(x) {test_tcareg(x, test = "joint", fast_mode = FALSE)},
  .options = furrr_options(seed = TRUE, packages = "TCA")
)

plan(sequential)
saveRDS(tcareg_fit, test_path("fixtures", "sim1_exp_4_fit_1_2_1.rds"))

# Scenario 5 -----------------------------
