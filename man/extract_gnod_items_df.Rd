% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scrape_gnod.R
\name{extract_gnod_items_df}
\alias{extract_gnod_items_df}
\title{Extract Gnod Items Dataframe}
\usage{
extract_gnod_items_df(page, item_url_id)
}
\arguments{
\item{page}{An HTML document object, which can be obtained by using
`rvest::read_html()` on a URL. This is the page from which the
data will be extracted.}
}
\value{
A dataframe containing the extracted items. Each row in the
dataframe represents an item, with columns for item names, URLs,
custom IDs, and numeric IDs within the loop.
}
\description{
This function takes an HTML page as input and extracts
relevant items from it, constructing a dataframe. It specifically
looks for 'a.S' nodes within the page to gather names and URLs,
and assigns IDs and numeric IDs in a loop for later use.
}
\examples{
\dontrun{
url <- "https://www.gnoosic.com/faves.php"
page <- rvest::read_html(url)
items_df <- extract_gnod_items_df(page)
print(items_df)
}
}
