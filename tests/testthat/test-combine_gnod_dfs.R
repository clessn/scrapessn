test_that("combine_gnod_dfs combines dataframes correctly", {
  skip_on_cran()

  # Example base and new dataframes
  df_base <- data.frame(item_a = c("A", "B"), item_b = c("C", "D"), closeness = c(0.5, 0.6))
  df_new <- data.frame(item_a = c("A", "E"), item_b = c("C", "F"), closeness = c(0.5, 0.7))

  combined_df <- combine_gnod_dfs(df_base, df_new)

  expect_equal(nrow(combined_df), 3)
  expect_true(all(c("A", "B", "E") %in% combined_df$item_a))
})
