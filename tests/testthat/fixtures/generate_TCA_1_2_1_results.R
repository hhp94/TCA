# generate the results created by TCA 1_2_1.

set.seed(1234)
warning("This shouldn't run in build test")
data <- test_data(5000, 50, 3, 2, 2, 0.01)
set.seed(1234)
tca.mdl <- tca(X = data$X, W = data$W, C1 = data$C1, C2 = data$C2)

saveRDS(data, test_path("fixtures", "simdata.rds"))
saveRDS(tca.mdl, test_path("fixtures", "simdata_fit_1_2_1.rds"))
