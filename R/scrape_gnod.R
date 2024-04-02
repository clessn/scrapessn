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


#' Extract Gnod Items Dataframe
#'
#' @description This function takes an HTML page as input and extracts
#' relevant items from it, constructing a dataframe. It specifically
#' looks for 'a.S' nodes within the page to gather names and URLs,
#' and assigns IDs and numeric IDs in a loop for later use.
#'
#' @param page An HTML document object, which can be obtained by using
#' `rvest::read_html()` on a URL. This is the page from which the
#' data will be extracted.
#' @return A dataframe containing the extracted items. Each row in the
#' dataframe represents an item, with columns for item names, URLs,
#' custom IDs, and numeric IDs within the loop.
#' @examples
#' \dontrun{
#' url <- "https://www.gnoosic.com/faves.php"
#' page <- rvest::read_html(url)
#' items_df <- extract_gnod_items_df(page)
#' print(items_df)
#' }
#' @export
#' @importFrom rvest html_nodes html_text html_attr
extract_gnod_items_df <- function(page, item_url_id){
  # Extract relevant nodes and construct the dataframe
  item_nodes <- rvest::html_nodes(page, "a.S")
  n_items <- length(item_nodes)
  items_df <- data.frame(names = rvest::html_text(item_nodes),
                         urls = rvest::html_attr(item_nodes, "href"))
  items_df$urls[1] <- item_url_id
  items_df$id <- clean_url_to_id(items_df$urls)
  items_df$numeric_id_in_loop <- 0:(n_items - 1)
  return(items_df)
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
extract_gnod_closeness_df <- function(page, item_url_id) {
  items_df <- extract_gnod_items_df(page, item_url_id)
  n_items <- nrows(items_df)
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
    closeness_dfi <- data.frame(item_a = idi,
                                item_b = items_df$id,
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


#' Combine Gnod Closeness Data Frames
#'
#' Takes a base data frame and a new data frame containing Gnod closeness data,
#' and combines them into a single data frame, removing any duplicate rows based
#' on the pairs of items. This is useful for aggregating closeness data from
#' multiple sources or for incremental updates.
#'
#' @param df_base A data frame serving as the base for combination.
#' Expected to contain closeness data between items with columns `item_a`
#' and `item_b` for the pair items, along with any closeness metrics.
#' @param df_new A data frame with new closeness data to be added to `df_base`.
#' Should have the same structure as `df_base`.
#'
#' @return A combined data frame with unique pairs of items based on `item_a`
#' and `item_b`, including closeness metrics from both `df_base` and `df_new`.
#'
#' @examples
#' # Assuming df_base and df_new are already defined data frames containing
#' # Gnod closeness data
#' combined_df <- combine_gnod_dfs(df_base, df_new)
#'
#' @export
combine_gnod_dfs <- function(df_base, df_new) {
  combined_df <- rbind(df_base, df_new) %>%
    distinct(item_a, item_b, .keep_all = TRUE)
  return(combined_df)
}




