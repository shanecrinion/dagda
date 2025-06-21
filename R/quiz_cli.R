library(dplyr)
library(tibble)

#' Filter how to process HTML codes
#'
#' @export
display_explanation <- function(text, use_html = shiny::isRunning()) {
  if (use_html) {
    # Use HTML rendering in Shiny (with line breaks)
    HTML(gsub("\n", "<br>", text))
  } else {
    # Use CLI output (strip HTML tags)
    plain <- gsub("<[^>]+>", "", text)
    if(plain==''){cat(text)} else {
      cat(text, "\n")}
  }
}


#' Filter words based on user choices
#'
#' @export
filter_words <- function(wordbank,
                         column_filters = list(),
                         dialect_filter = NULL,
                         rank_limit = NULL) {
  filtered <- dplyr::as_tibble(wordbank)
  filtered$rank = as.numeric(filtered$rank)

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
      # Handle percentage
      perc <- as.numeric(sub("%", "", rank_limit)) / 100
      n_top <- ceiling(nrow(filtered) * perc)
      filtered <- filtered %>% arrange(rank) %>% slice_head(n = n_top)
    } else if (is.numeric(rank_limit) && length(rank_limit) == 1) {
      # Handle upper limit only
      filtered <- filtered %>% filter(rank <= rank_limit)
    } else if (is.numeric(rank_limit) && length(rank_limit) == 2) {
      # Handle rank range
      lower <- min(rank_limit)
      upper <- max(rank_limit)
      filtered <- filtered %>% filter(rank >= lower & rank <= upper)
    }
  }

  return(filtered)
}

#' Get the username entered
#'
#' @export
get_username <- function(username = NULL, interactive_mode = TRUE) {
  if (!is.null(username) && username != "") {
    return(username)
  }

  if (interactive_mode) {
    return(readline("Enter your username: "))
  } else {
    warning("No username supplied in non-interactive mode.")
    return('anon')
  }
}

#' Load previous scores to update memory
#'
#' @export
# Function to prompt for username and load/create user scores
load_user_scores <- function(wordbank, username = NULL, save_dir = "user_data", interactive_mode = TRUE) {
  username <- get_username(username, interactive_mode = interactive_mode)

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
        history_log = "",
        skipped_count = 0,
        excluded = FALSE
      )
    message("Starting new session for ", username)
  }

  list(username = username, word_scores = word_scores, score_file = score_file)
}

#' Run the quiz
#'
#' @export
run_quiz_cli <- function(wordbank,
                         column_filters = list(),
                         dialect_filter = NULL,
                         n_questions = 10,
                         rank_limit = NULL) {

  # Load or create user-specific word_scores
  session <- load_user_scores(wordbank, interactive_mode = TRUE)
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

  # Ensure the number of questions does not exceed available words
  n_questions <- min(n_questions, nrow(quiz_data))

  # Sample the quiz words
  # Remove problematic rows up front
  quiz_data <- quiz_data %>%
    filter(!excluded, !is.na(ga), !is.na(en)) %>%
    distinct(ga, .keep_all = TRUE)

  # Safety check: are there enough valid words?
  if (nrow(quiz_data) < n_questions) {
    cat("Only", nrow(quiz_data), "valid words available for the quiz.\n")
    return(invisible(NULL))
  }

  # Now randomly sample n_questions
  # Remove problematic rows up front
  quiz_data <- quiz_data %>%
    filter(!excluded, !is.na(ga), !is.na(en)) %>%
    distinct(ga, .keep_all = TRUE)

  # Safety check: are there enough valid words?
  if (nrow(quiz_data) < n_questions) {
    cat("Only", nrow(quiz_data), "valid words available for the quiz.\n")
    return(invisible(NULL))
  }

  # Now randomly sample n_questions
  quiz_data <- quiz_data %>% slice_sample(n = n_questions)

  quiz_data <- quiz_data %>% distinct(ga, .keep_all = TRUE)

  # Keep track of asked words
  asked_words <- character(0)

  # Quiz loop
  for (i in seq_len(n_questions)) {
    word_row <- quiz_data[i, ]
    if (is.na(word_row$ga) || is.na(word_row$en)) next
    # Safety check: skip if word already asked
    if (word_row$ga %in% asked_words) next
    asked_words <- c(asked_words, word_row$ga)

    cat("\nQuestion ", i, " of ", n_questions, "\n", sep = "")
    cat("Irish word: ", word_row$ga, "\n")
    answer <- readline(prompt = "Type the English translation: ")

    if (tolower(answer) %in% c("skip", "s", "")) {

      word_scores[word_scores$ga == word_row$ga, "skipped_count"] <-
        word_scores[word_scores$ga == word_row$ga, "skipped_count"] + 1
      cat("Skipped!\nExpected word was:\n", word_row$en)
      next
    }
    correct <- !is.na(word_row$en) && tolower(answer) == tolower(word_row$en)

    if (correct) {
      cat("✅ Correct!\n")
    } else {
      cat("❌ Incorrect. Expected: \n" ,
          display_explanation(word_row[1], use_html = FALSE),
           "\n", sep = "")
    }

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

      correct_flag <- ifelse(correct, "1", "0")

      # Update override_history (legacy)
      prev_hist <- word_scores$override_history[idx]
      word_scores$override_history[idx] <- ifelse(prev_hist == "", correct_flag, paste0(prev_hist, ",", correct_flag))

      # Update new timestamped history_log
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      entry <- paste0(now, "|", correct_flag)
      prev_log <- word_scores$history_log[idx]
      word_scores$history_log[idx] <- ifelse(prev_log == "", entry, paste(prev_log, entry, sep = ","))
    }
  }

  # Save updated scores
  saveRDS(word_scores, score_file)
  cat("\nQuiz complete! Your progress has been saved.\n")
}
