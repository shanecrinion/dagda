library(httr)
library(jsonlite)
library(dplyr)

api_key <- "" # got this from logainm / gaisc dev login
base_url <- "https://www.logainm.ie/api/v1.0/"
per_page <- 1000
page <- 1
all_results <- list()

repeat {
  message("Fetching page ", page)
  res <- GET(
    url = base_url,
    authenticate(user = api_key, password = ""),
    query = list(Page = page, PerPage = per_page)
  )

  stop_for_status(res)

  page_data <- content(res, as = "text", encoding = "UTF-8")
  page_json <- fromJSON(page_data, simplifyVector = FALSE)

  # If no results, break the loop
  if (length(page_json) == 0) {
    message("No more results. Ending.")
    break
  }

  all_results <- c(all_results, page_json)

  # If fewer results than per_page, we've reached the last page
  if (length(page_json) < per_page) {
    message("Last page reached.")
    break
  }

  page <- page + 1
}

message("Total places retrieved: ", length(all_results))

saveRDS(all_results, 'inst/extdata/placenames.rds')

# Optionally, save to JSON file
jsonlite::write_json(all_results, "logainm_all_places.json", pretty = TRUE, auto_unbox = TRUE)
