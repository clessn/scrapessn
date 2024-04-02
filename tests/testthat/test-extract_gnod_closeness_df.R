test_that("extract_gnod_closeness_df returns correct structure and data", {
  skip_on_cran()

  # Similar setup with a mock page
  mock_page <- fetch_webpage("https://www.music-map.com/the+beatles")
  test_df <- extract_gnod_closeness_df(mock_page, "the+beatles")

  expect_true(is.data.frame(test_df))
  expect_equal(names(test_df), c("item_a", "item_b", "closeness"))

  # Insert specific content tests as needed
})
