# Load the data
load('data/test_data_clean.Rdata')
# Load the quiz CLI
source(here::here('R', "quiz_cli.R"))

run_quiz_cli(test_data.clean, rank_limit = c(40,90) , n_questions = 10)
