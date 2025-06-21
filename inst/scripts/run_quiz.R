# Load the data
load('data/test_data.Rdata')

# Load the quiz CLI
source(here::here('R', "quiz_cli.R"))

run_quiz_cli(test_data)
