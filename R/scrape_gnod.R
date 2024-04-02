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
  items_df <- data.frame(name = rvest::html_text(item_nodes),
                         item_url_id = rvest::html_attr(item_nodes, "href"))
  items_df$item_url_id[1] <- item_url_id
  items_df$id <- clean_url_to_id(items_df$item_url_id)
  items_df$numeric_id_in_loop <- 0:(n_items - 1)
  items_df <- items_df[, c("id", "name", "item_url_id", "numeric_id_in_loop")]
  return(items_df)
}


#' Extract Gnod Closeness Data Frame
#'
#' Extracts and constructs a data frame representing the "closeness" of related items (such as artists, books, movies, or series)
#' based on their interaction on the Gnod platform. It processes the HTML content to scrape names, URLs, and their closeness scores.
#'
#' @param page An HTML document object representing the webpage to be processed. This should be obtained
#' by calling the `fetch_webpage` function or similar `rvest::read_html` function calls.
#' @param item_url_id A character string representing the specific item's URL identifier used to match
#' and construct the closeness scores matrix. This is used to identify the base item in the closeness calculations.
#' @return A data frame where each row represents the closeness score between the base item (specified by `item_url_id`)
#' and another item. The data frame contains columns for the base item (`item_a`), the compared item (`item_b`),
#' and their closeness score (`closeness`).
#' @importFrom rvest html_nodes html_text html_attr
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_replace str_detect
#' @examples
#' \dontrun{
#' base_url <- "https://www.gnoosic.com/faves.php"
#' item_url_id <- "the+beatles"
#' url <- paste0(base_url, item_url_id)
#' page <- fetch_webpage(url)
#' closeness_df <- extract_gnod_closeness_df(page, item_url_id)
#' head(closeness_df)
#' }
#' @export
extract_gnod_closeness_df <- function(page, item_url_id) {
  items_df <- extract_gnod_items_df(page, item_url_id)
  n_items <- nrow(items_df)
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
    dplyr::distinct(item_a, item_b, .keep_all = TRUE)
  return(combined_df)
}




