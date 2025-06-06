#!/usr/bin/env Rscript

option_list <- list(
  make_option(c("--top"), type = "integer", help = "Show top N most frequent words"),
  make_option(c("--type"), type = "character", help = "Filter by word type (noun, verb, etc.)"),
  make_option(c("--random"), action = "store_true", help = "Show a random word"),
  make_option(c("--search"), type = "character", help = "Search for a specific word"),
  make_option(c("--quiz"), type = "integer", help = "Start a quiz with N words")
)

opt_parser <- OptionParser(option_list = option_list)
opt <- parse_args(opt_parser)

# Load your dataset (assumes it's loaded with the package)
data("word_list")

if (!is.null(opt$top)) {
  print(get_top_words(word_list, opt$top))
}

if (!is.null(opt$type)) {
  print(filter_by_type(word_list, opt$type))
}

if (opt$random) {
  print(random_word(word_list))
}

if (!is.null(opt$search)) {
  print(search_word(word_list, opt$search))
}

if (!is.null(opt$quiz)) {
  quiz_words(word_list, opt$quiz)
}
