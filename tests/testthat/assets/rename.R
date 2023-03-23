# # Rename the ewas file names
# df <- data.frame(
#   old_name = list.files(test_path("assets", "ewas")),
#   old_path = list.files(test_path("assets", "ewas"), full.names = TRUE)
# )
#
# df$new_name <- sub("TCA_methylation_", "", df$old_name)
# df$new_name <- sub("effect_size", "effs", df$new_name)
# df$new_name <- sub("marginal", "marg", df$new_name)
# df$new_name <- sub("_level", "", df$new_name)
# df$new_name <- sub("k_6", "k6", df$new_name)
# df$new_name <- sub("n_500", "n500", df$new_name)
# df$new_path <- stringr::str_replace_all(df$old_path, df$old_name, df$new_name)
#
# stopifnot(length(df$new_path) == length(unique(df$new_path)))
#
# fs::file_move(df$old_path, df$new_path)
# #rgr rpwf_sim .
#
# saveRDS(df, test_path("assets", "ewas", "rename.rds"))
