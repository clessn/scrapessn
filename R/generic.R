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
