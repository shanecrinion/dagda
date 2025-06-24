#library(optparse)

# Load the quiz CLI
source(here::here('R', "quiz_cli.R"))

# Load your wordbank
wordbank <- readRDS(here::here('data', 'test_data.rds'))

run_quiz_cli(wordbank,
             n_questions = 10,rank_limit = c(10,20),
             selection_mode = "ordered")  # or "random" or "rank_range"

# # Run quiz - use source for now but hope to optparse
# run_quiz_cli(
#   wordbank = wordbank,
#   keyword_filter = opt$keyword,
#   rank_limit = rank_limit,
#   n_questions = opt$n_questions,
#   interactive_mode = interactive())

