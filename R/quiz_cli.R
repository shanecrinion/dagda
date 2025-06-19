library(dplyr)
library(tibble)


filter_words <- function(wordbank,
                         column_filters = list(),
                         dialect_filter = NULL,
                         rank_limit = NULL) {
  filtered <- dplyr::as_tibble(wordbank)

  for (col in names(column_filters)) {
    if (col %in% names(filtered)) {
      vals <- column_filters[[col]]
      if (length(vals) > 0) {
        filtered <- filtered %>% filter(.data[[col]] %in% vals)
      }
    }
  }

  if (!is.null(dialect_filter) && dialect_filter != "") {
    filtered <- filtered %>%
      filter(
        grepl(dialect_filter, en, ignore.case = TRUE) |
          grepl(dialect_filter, genitiveVN, ignore.case = TRUE)
      )
  }

  if (!is.null(rank_limit) && "rank" %in% names(filtered)) {
    if (is.character(rank_limit) && grepl("^\\d+%$", rank_limit)) {
      perc <- as.numeric(sub("%", "", rank_limit)) / 100
      n_top <- ceiling(nrow(filtered) * perc)
      filtered <- filtered %>% arrange(rank) %>% slice_head(n = n_top)
    } else if (suppressWarnings(!is.na(as.numeric(rank_limit)))) {
      rank_num <- as.numeric(rank_limit)
      filtered <- filtered %>% filter(rank <= rank_num)
    }
  }
  return(filtered)
}

# Function to prompt for username and load/create user scores
load_user_scores <- function(wordbank, save_dir = "user_data") {
  username <- readline(prompt = "Enter your username: ")

  if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
  score_file <- file.path(save_dir, paste0("word_scores_", username, ".rds"))

  if (file.exists(score_file)) {
    word_scores <- readRDS(score_file)
    message("Welcome back, ", username, "!")
  } else {
    word_scores <- wordbank %>%
      mutate(
        seen_count = 0,
        correct_count = 0,
        score = 0,
        last_seen = as.POSIXct(NA),
        override_history = "",
        skipped_count = 0,
        excluded = FALSE
      )
    message("Starting new session for ", username)
  }

  list(username = username, word_scores = word_scores, score_file = score_file)
}

# The main quiz CLI function
run_quiz_cli <- function(wordbank,
                         column_filters = list(),
                         dialect_filter = NULL,
                         max_questions = 10,
                         rank_limit = NULL) {

  # Load or create user-specific word_scores
  session <- load_user_scores(wordbank)
  word_scores <- session$word_scores
  score_file <- session$score_file

  # Apply filtering based on user's request
  quiz_data <- filter_words(word_scores, column_filters, dialect_filter, rank_limit)

  # Remove excluded words
  quiz_data <- quiz_data %>% filter(!excluded)

  if (nrow(quiz_data) == 0) {
    cat("No words available after filtering.\n")
    return(invisible(NULL))
  }

  # Limit number of questions to available words or max_questions
  n_questions <- min(max_questions, nrow(quiz_data))
  quiz_data <- quiz_data %>% slice_sample(n = n_questions)

  # Quiz loop
  for (i in seq_len(n_questions)) {
    word_row <- quiz_data[i, ]

    cat("\nQuestion ", i, " of ", n_questions, "\n", sep = "")
    cat("Irish word: ", word_row$ga, "\n")
    answer <- readline(prompt = "Type the English translation: ")

    if (tolower(answer) %in% c("skip", "s", '')) {
      cat("Skipped!\n")
      word_scores[word_scores$ga == word_row$ga, "skipped_count"] <-
        word_scores[word_scores$ga == word_row$ga, "skipped_count"] + 1
      next
    }

    correct <- tolower(answer) == tolower(word_row$en)

    # Show feedback
    if (correct) {
      cat("✅ Correct!\n")
    } else {
      cat("❌ Incorrect. Expected: ", word_row$en, "\n", sep = "")
    }

    # Give user a chance to override or exclude
    cat("Options: [m]ark correct, e[x]clude word, [Enter] to continue: ")
    action <- readline()

    if (tolower(action) == "m") {
      correct <- TRUE
      cat("✔ Marked as correct by user.\n")
    }

    if (tolower(action) == "x") {
      word_scores[word_scores$ga == word_row$ga, "excluded"] <- TRUE
      cat("🚫 Word excluded from future quizzes.\n")
    }

    # Update word_scores
    idx <- which(word_scores$ga == word_row$ga)
    if (length(idx) == 1) {
      word_scores$seen_count[idx] <- word_scores$seen_count[idx] + 1
      if (correct) {
        word_scores$correct_count[idx] <- word_scores$correct_count[idx] + 1
      }
      word_scores$score[idx] <- word_scores$correct_count[idx] / word_scores$seen_count[idx]
      word_scores$last_seen[idx] <- Sys.time()

      prev_hist <- word_scores$override_history[idx]
      new_entry <- ifelse(correct, "1", "0")
      if (prev_hist == "") {
        word_scores$override_history[idx] <- new_entry
      } else {
        word_scores$override_history[idx] <- paste0(prev_hist, ",", new_entry)
      }
    }
  }


  # Save updated scores
  saveRDS(word_scores, score_file)
  cat("\nQuiz complete! Your progress has been saved.\n")
}
