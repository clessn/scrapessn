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
  urls <- gsub("[%&/'\"]", "", urls)
  urls <- gsub("__", "_", urls)
  return(urls)
}
