test_that("extract_gnod_items_df extracts expected data frame", {
  skip_on_cran()

  # Suppose you have a saved HTML file or a mock object for testing
  mock_page <- fetch_webpage("https://www.music-map.com/the+beatles")
  test_df <- extract_gnod_items_df(mock_page, "the+beatles")

  # Test the structure of the returned dataframe
  expect_true(is.data.frame(test_df))
  expect_equal(names(test_df), c("id", "name", "item_url_id", "numeric_id_in_loop"))

  # More specific tests can include checking the contents of the dataframe
  # This depends on the mock HTML content you use for testing
})
