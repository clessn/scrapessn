#' Clean URL to ID
#'
#' This function takes a vector of URLs as strings, cleans them by replacing specific characters with "_",
#' decodes percentages and some specific characters, and removes special characters by replacing them
#' with nothing to form clean identifiers. It is designed to convert URL slugs or query parameters into
#' more readable or database-friendly IDs.
#'
#' @param urls A character vector of URLs to be cleaned.
#' @return A character vector of cleaned IDs.
#' @examples
#' urls <- c("the+beatles", "queen", "pink+floyd", "simon+%26+garfunkel")
#' clean_url_to_id(urls)
#' @export
#'
#' @importFrom stringr str_replace_all
clean_url_to_id <- function(urls) {
  # Replace specific characters with "_"
  urls <- gsub("\\+", "_", urls)
  # Decode percentages and some specific characters
  urls <- sapply(urls, URLdecode)
  # Remove special characters and replace them with nothing
  urls <- gsub("[%&/'\":]", "", urls)
  urls <- gsub("__", "_", urls)
  urls <- gsub("[^A-Za-z0-9_]", "", urls)
  urls <- ifelse(grepl("^[0-9]+$", urls), paste0("i", urls), urls)
  return(urls)
}


#' Fetch Webpage Content
#'
#' @description Fetches the HTML content of a webpage. It constructs the URL by optionally appending
#' an item URL identifier to a base URL, and then uses `rvest::read_html()` to download and parse
#' the HTML content of the page. If the item URL identifier is not provided, it fetches the base URL.
#'
#' @param base_url A character string specifying the base URL of the website.
#' @param item_url_id An optional character string specifying the specific item's URL identifier
#' to be appended to the base URL for constructing the full URL. Default is NULL, indicating that
#' only the base URL is used.
#' @return An HTML document object representing the fetched webpage. This object can be further
#' processed using various `rvest` functions to extract specific data.
#' @examples
#' \dontrun{
#' base_url <- "https://www.example.com"
#' item_url_id <- "page.html"
#' page <- fetch_webpage(base_url, item_url_id)
#' print(page)
#'
#' # Fetching content from the base URL only
#' page_base <- fetch_webpage(base_url)
#' print(page_base)
#' }
#' @export
#' @importFrom rvest read_html
fetch_webpage <- function(base_url, item_url_id = NULL) {
  url <- if (!is.null(item_url_id)) paste0(base_url, item_url_id) else base_url
  page <- rvest::read_html(url)
  return(page)
}
