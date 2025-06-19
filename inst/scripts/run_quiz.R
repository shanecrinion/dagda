# Load the data
load('data/test_data_clean.Rdata')

# Load the quiz CLI
source(here::here('R', "quiz_cli.R"))


### Testing out functions here

userscores_sc <- readRDS('user_data/word_scores_shanecrinion.rds')


for (col in names(column_filters)) {
  if (col %in% names(filtered)) {
    vals <- column_filters[[col]]
    if (length(vals) > 0) {
      filtered <- filtered %>% filter(.data[[col]] %in% vals)
    }
  }
}

