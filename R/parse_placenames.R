library(jsonlite)
library(dplyr)
library(tidyr)
library(purrr)

raw <- fromJSON("logainm_all_places.json")

# Extract main simple columns
main_df <- raw$results %>%
  select(id, dateCreated, dateModified, permalink)

# Extract cluster focusID (flatten)
cluster_focusID <- map_int(as.data.frame(raw$results$cluster), ~ .x$focusID)

head(as.data.frame(raw$results$cluster$focusID))

main_df$cluster_focusID <- cluster_focusID


# Extract categories (each row has 1 row data.frame with 5 columns)
categories_df <- map_dfr(raw$results$categories, ~ .x)

main_df <- bind_cols(main_df, categories_df)

# Extract placenames - this is tricky since each is a data.frame of multiple rows
# Unnest placenames into a separate df with a join key 'id'
placenames_list <- map2(raw$results$id, raw$results$placenames, ~ {
  df <- .y[[1]]
  if (is.null(df)) return(NULL)
  df$id <- .x
  df
})

placenames_df <- bind_rows(placenames_list)

# Similarly, you can extract `includes`, `images`, `geography` etc.

