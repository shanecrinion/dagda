# Load libraries
library(dplyr)
library(tibble)
library(stringr)

# ────────────────────────────────────────────────────────────────────────────────
# 1. FILTER WORDS
# ────────────────────────────────────────────────────────────────────────────────

filter_words <- function(wordbank,
                         column_filters = list(),
                         dialect_filter = NULL) {

  filtered_data <- tibble::as_tibble(wordbank)

  # Apply column-based filters
  for (col in names(column_filters)) {
    if (col %in% names(filtered_data)) {
      vals <- column_filters[[col]]
      if (length(vals) > 0) {
        filtered_data <- filtered_data %>%
          filter(.data[[col]] %in% vals)
      }
    }
  }

  # Dialect-specific search in 'en' and 'genitiveVN'
  if (!is.null(dialect_filter) && dialect_filter %in% c("Munster", "Connacht", "Ulster")) {
    for (col in c("en", "genitiveVN")) {
      if (!col %in% names(filtered_data)) {
        filtered_data[[col]] <- ""
      }
    }
    filtered_data <- filtered_data %>%
      filter(
        grepl(dialect_filter, en, ignore.case = TRUE) |
          grepl(dialect_filter, genitiveVN, ignore.case = TRUE)
      )
  }

  return(filtered_data)
}

# ────────────────────────────────────────────────────────────────────────────────
# 2. UPDATE WORD SCORES
# ────────────────────────────────────────────────────────────────────────────────

update_word_scores <- function(word_scores, quiz_results, learning_rate = 0.1) {
  for (i in seq_len(nrow(quiz_results))) {
    word <- quiz_results$ga[i]
    correct <- quiz_results$correct[i]
    time <- quiz_results$response_time[i]
    now <- quiz_results$session_timestamp[i]

    if (!(word %in% word_scores$ga)) {
      word_scores <- word_scores %>%
        add_row(
          ga = word,
          score = 0.5,
          seen_count = 0,
          correct_count = 0,
          avg_time = time,
          last_seen = now,
          response_history = as.character(as.integer(correct))
        )
    }

    row_idx <- which(word_scores$ga == word)

    old_score <- word_scores$score[row_idx]
    new_score <- old_score + learning_rate * (correct - old_score)

    old_hist <- word_scores$response_history[row_idx]
    new_hist <- if (is.na(old_hist) || old_hist == "") {
      as.character(as.integer(correct))
    } else {
      paste0(old_hist, ",", as.integer(correct))
    }

    word_scores$score[row_idx] <- new_score
    word_scores$seen_count[row_idx] <- word_scores$seen_count[row_idx] + 1
    word_scores$correct_count[row_idx] <- word_scores$correct_count[row_idx] + as.integer(correct)
    word_scores$avg_time[row_idx] <- mean(c(word_scores$avg_time[row_idx], time), na.rm = TRUE)
    word_scores$last_seen[row_idx] <- now
    word_scores$response_history[row_idx] <- new_hist
  }

  return(word_scores)
}

# ────────────────────────────────────────────────────────────────────────────────
# 3. RUN QUIZ
# ────────────────────────────────────────────────────────────────────────────────

run_quiz <- function(wordbank,
                     word_scores = NULL,
                     column_filters = list(),
                     dialect_filter = NULL,
                     num_questions = 10,
                     learning_rate = 0.1,
                     save_path = "word_scores.rds") {

  # Step 1: Filter words
  quiz_data <- filter_words(wordbank, column_filters, dialect_filter)

  if (nrow(quiz_data) == 0) {
    cat("⚠️ No words found after filtering.\n")
    return(NULL)
  }

  quiz_data <- sample_n(quiz_data, size = min(num_questions, nrow(quiz_data)))
  session_time <- Sys.time()
  results <- list()

  cat("🔤 Starting your Irish quiz...\n")

  for (i in 1:nrow(quiz_data)) {
    word <- quiz_data$ga[i]
    correct_answer <- quiz_data$en[i]

    cat(paste0("\n(", i, "/", nrow(quiz_data), ") Translate: ", word, "\n"))
    start <- Sys.time()
    response <- readline("Your answer: ")
    end <- Sys.time()

    time_taken <- as.numeric(difftime(end, start, units = "secs"))
    correct <- tolower(trimws(response)) == tolower(trimws(correct_answer))
    cat(if (correct) "✅ Correct!\n" else paste0("❌ Incorrect. Answer: ", correct_answer, "\n"))

    results[[i]] <- tibble::tibble(
      ga = word,
      correct = correct,
      response_time = time_taken,
      session_timestamp = session_time
    )
  }

  quiz_results <- bind_rows(results)

  # Step 2: Init word_scores if needed
  if (is.null(word_scores)) {
    word_scores <- tibble::tibble(
      ga = unique(wordbank$ga),
      score = 0.5,
      seen_count = 0,
      correct_count = 0,
      avg_time = NA_real_,
      last_seen = as.POSIXct(NA),
      response_history = NA_character_
    )
  }

  # Step 3: Update and save
  word_scores <- update_word_scores(word_scores, quiz_results, learning_rate)
  saveRDS(word_scores, save_path)

  cat("\n📊 Quiz complete. Your progress has been saved!\n")
  return(invisible(list(results = quiz_results, word_scores = word_scores)))
}
