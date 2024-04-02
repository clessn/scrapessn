library(testthat)
library(rvest)

test_that("fetch_webpage returns an HTML document object", {
  base_url <- "https://www.gnod.com/"
  result <- fetch_webpage(base_url)

  expect_true(inherits(result, "xml_document"))
})

test_that("fetch_webpage handles NULL item_url_id correctly", {
  base_url <- "https://www.gnod.com/"
  result_with_null <- fetch_webpage(base_url, NULL)

  expect_true(inherits(result_with_null, "xml_document"))
})

test_that("fetch_webpage constructs URL correctly with item_url_id", {
  base_url <- "https://www.music-map.com/"
  item_url_id <- "the+beatles"
  result <- fetch_webpage(base_url, item_url_id)
  # Use any specific checks relevant to your 'test' page if possible.
  # This might be challenging for a generic test, but you could check for the
  # presence of expected content if the page
  expect_true(inherits(result, "xml_document"))
})
