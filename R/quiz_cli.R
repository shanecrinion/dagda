library(dplyr,quietly = T)
library(tibble,quietly = T)

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
      print(text)}
  }
}


#' Filter words based on user choices
#'
#' @export
filter_words <- function(wordbank,
                         column_filters = list(),
                         keyword_filter = NULL,
                         rank_limit = NULL) {
#  filter(rank >= rank_limit[1], rank <= rank_limit[2]) # might need something like this
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

  if (!is.null(keyword_filter) && keyword_filter != "") {
    filtered <- filtered %>%
      filter(
        grepl(keyword_filter, en, ignore.case = TRUE) |
          grepl(keyword_filter, genitiveVN, ignore.case = TRUE)
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

#' @export
#' Run the quiz
#'
run_quiz_cli <- function(wordbank,
                         keyword_filter = NULL,
                         n_questions = 10,
                         rank_limit = NULL,
                         interactive_mode = TRUE,
                         selection_mode = "random",
                         start_rank = 1) {

  # Load or create user-specific word_scores
  session <- load_user_scores(wordbank, interactive_mode = interactive_mode)
  word_scores <- session$word_scores
  score_file <- session$score_file

  repeat_quiz <- TRUE
  quiz_offset <- start_rank

  while (repeat_quiz) {
    # Filtering
    quiz_data <- filter_words(word_scores,
                              column_filters = list(),
                              keyword_filter = keyword_filter,
                              rank_limit = rank_limit)

    quiz_data <- quiz_data %>%
      filter(!excluded, !is.na(ga), !is.na(en)) %>%
      distinct(ga, .keep_all = TRUE)

    if (nrow(quiz_data) == 0) {
      cat("No words available after filtering.\n")
      return(invisible(NULL))
    }

    # Apply selection mode
    if (selection_mode == "ordered") {
      quiz_data <- quiz_data %>%
        arrange(rank) %>%
        slice((quiz_offset):(quiz_offset + n_questions - 1))
    } else if (selection_mode == "random") {
      quiz_data <- quiz_data %>% slice_sample(n = min(n_questions, nrow(quiz_data)))
    } else if (selection_mode == "rank_range" && !is.null(rank_limit)) {
      quiz_data <- quiz_data %>% arrange(rank)
      quiz_data <- quiz_data %>% slice_head(n = min(n_questions, nrow(quiz_data)))
    }

    if (nrow(quiz_data) < n_questions) {
      cat("Only", nrow(quiz_data), "valid words available for the quiz.\n")
    }

    # Keep track of asked words
    asked_words <- character(0)

    for (i in seq_len(nrow(quiz_data))) {
      word_row <- quiz_data[i, ]
      if (is.na(word_row$ga) || is.na(word_row$en)) next
      if (word_row$ga %in% asked_words) next
      asked_words <- c(asked_words, word_row$ga)

      cat("\nQuestion ", i, " of ", nrow(quiz_data), "\n", sep = "")
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
        cat("❌ Incorrect. Expected: \n")
        print(display_explanation(word_row[1], use_html = FALSE))
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

      # Update scores
      idx <- which(word_scores$ga == word_row$ga)
      if (length(idx) == 1) {
        word_scores$seen_count[idx] <- word_scores$seen_count[idx] + 1
        if (correct) {
          word_scores$correct_count[idx] <- word_scores$correct_count[idx] + 1
        }
        word_scores$score[idx] <- word_scores$correct_count[idx] / word_scores$seen_count[idx]
        word_scores$last_seen[idx] <- Sys.time()

        correct_flag <- ifelse(correct, "1", "0")
        now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        entry <- paste0(now, "|", correct_flag)
        prev_log <- word_scores$history_log[idx]
        word_scores$history_log[idx] <- ifelse(prev_log == "", entry, paste(prev_log, entry, sep = ","))
        prev_hist <- word_scores$override_history[idx]
        word_scores$override_history[idx] <- ifelse(prev_hist == "", correct_flag, paste0(prev_hist, ",", correct_flag))
      }
    }

    saveRDS(word_scores, score_file)
    cat("\nQuiz complete! Your progress has been saved.\n")

    # Repeat same quiz?
    repeat_choice <- tolower(readline("Do you want to repeat the same quiz? (y/n): "))
    if (repeat_choice == "y") {
      next  # repeat the loop without changing anything
    }

    # For ordered mode only, ask to continue with next N
    if (selection_mode == "ordered") {
      next_choice <- tolower(readline(sprintf("Continue with next %d ranked words? (y/n): ", n_questions)))
      if (next_choice == "y") {
        quiz_offset <- quiz_offset + n_questions
        next
      }
    }

    repeat_quiz <- FALSE  # Exit the quiz loop
  }
}

