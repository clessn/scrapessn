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


#' Fetch Closeness Matrix
#'
#' Retrieves and constructs a matrix representing the "closeness" of related items (such as artists, books, movies, or series)
#' based on a specified base URL. It scrapes the names, URLs, and their closeness scores from the HTML content of the page.
#'
#' @param url_id A string representing the base URL of the item's page to scrape from.
#' @return A square matrix where each row and column represents an item (artist, book, movie, or series),
#'         and the cell values represent the closeness scores between each pair of items.
#' @importFrom rvest read_html html_nodes html_text html_attr
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_replace str_detect
#' @examples
#' url_id <- "http://some-base-url.com/item-name"
#' closeness_matrix <- fetch_closeness_matrix(url_id)
#' @export
fetch_closeness_df <- function(item_url_id, base_url) {
  url <- paste0(base_url, item_url_id)
  # Fetch the HTML content from the page
  page <- rvest::read_html(url)
  # Extract relevant nodes and construct the dataframe
  item_nodes <- rvest::html_nodes(page, "a.S")
  n_items <- length(item_nodes)
  items_df <- data.frame(names = rvest::html_text(item_nodes),
                         urls = rvest::html_attr(item_nodes, "href"))
  items_df$urls[1] <- item_url_id
  items_df$id <- clean_url_to_id(items_df$urls)
  items_df$numeric_id_in_loop <- 0:(n_items - 1)
  script_content <- page %>%
    rvest::html_nodes("script") %>%
    rvest::html_text()
  content_as_string <- script_content[3]
  for (i in 0:(n_items - 1)){
    idi <- items_df$id[items_df$numeric_id_in_loop == i]
    pattern <- paste0("Aid\\[", i, "\\]=new Array\\((.*?)\\);")
    match <- regmatches(content_as_string, regexpr(pattern, content_as_string))[1]
    num_string <- gsub(paste0("Aid\\[", i, "\\]=new Array\\(|\\);"), "", match)
    numbers <- as.numeric(strsplit(num_string, ",")[[1]])
    numbers[numbers == -1] <- NA
    closeness_dfi <- data.frame(band_a = idi,
                                band_b = items_df$id,
                                closeness = numbers)
    if (i == 0){
      closeness_df <- closeness_dfi
    } else {
      closeness_df <- rbind(closeness_df, closeness_dfi)
    }
    message(paste(i, idi))
  }
  return(closeness_df)
}
