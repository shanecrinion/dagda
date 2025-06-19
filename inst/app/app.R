library(shiny)
library(dplyr)

source(here::here('R', 'quiz_cli.R'))

ui <- fluidPage(

  tags$script(HTML("
  $(document).on('keypress', function(e) {
    if(e.which == 13) {
      $('#submit_username').click();
    }
  });"
  )),
  titlePanel("Irish Vocabulary Quiz"),

  # 1. Username input + submit button
  textInput("username", "Enter your username:", ""),
  actionButton("submit_user", "Enter"),
  hr(),

  # 2. Conditional quiz setup inputs shown after username submitted
  uiOutput("quiz_setup_ui"),

  # 3. Quiz UI shown after quiz started
  uiOutput("quiz_ui"),

  # Notification
  verbatimTextOutput("quiz_feedback")
)

server <- function(input, output, session) {
  # Wordbank example (replace with your real data)
  wordbank <- tibble(
    ga = c("focal", "leabhar", "bád"),
    en = c("word", "book", "boat"),
    genitiveVN = c("focail", "leabhair", "báid"),
    rank = c(10, 20, 5),
    excluded = c(FALSE, FALSE, FALSE)
  )

  # Reactive values to store user session info and quiz state
  user_data <- reactiveValues(
    username = NULL,
    score_file = NULL,
    word_scores = NULL,
    quiz_data = NULL,
    current_question = 0,
    max_questions = 10,
    column_filters = list(),
    dialect_filter = NULL,
    rank_limit = NULL,
    quiz_active = FALSE,
    feedback = ""
  )

  # Load or create user scores on username submit
  observeEvent(input$submit_user, {
    req(input$username)
    user_data$username <- input$username

    save_dir <- "user_data"
    if (!dir.exists(save_dir)) dir.create(save_dir, recursive = TRUE)
    user_data$score_file <- file.path(save_dir, paste0("word_scores_", user_data$username, ".rds"))

    if (file.exists(user_data$score_file)) {
      user_data$word_scores <- readRDS(user_data$score_file)
      user_data$feedback <- paste0("Welcome back, ", user_data$username, "!")
    } else {
      user_data$word_scores <- wordbank %>%
        mutate(
          seen_count = 0,
          correct_count = 0,
          score = 0,
          last_seen = as.POSIXct(NA),
          override_history = "",
          skipped_count = 0,
          excluded = FALSE
        )
      user_data$feedback <- paste0("Starting new session for ", user_data$username)
    }

    user_data$quiz_active <- FALSE
    user_data$current_question <- 0
  })

  # Show quiz setup UI after username entered
  output$quiz_setup_ui <- renderUI({
    req(user_data$username)
    if (user_data$quiz_active) return(NULL)

    tagList(
      h4("Customize your quiz"),

      # Example: Max questions numeric input
      numericInput("max_questions", "Questions:", value = 10, min = 1, max = 100),

      # Example: Rank limit input (number or percentage string)
      textInput("rank_limit", "Word overall rank limit (e.g. enter number e.g. 10 of percentage of top Irish words to include):", value = "10"),

      # Example: Dialect filter text input
      textInput("dialect_filter", "Enter Munster, Commerara or Ulster (beta)"),

      # Column filters: for demo, assume column 'excluded' toggle (could extend)
      checkboxInput("exclude_words", "Exclude words marked as excluded", value = TRUE),

      actionButton("start_quiz", "Start Quiz")
    )
  })

  # When user clicks Start Quiz button
  observeEvent(input$start_quiz, {
    req(user_data$username)

    # Build filters from inputs
    col_filters <- list()
    if (input$exclude_words) {
      # We want to exclude excluded words, so actually filter excluded == FALSE
      col_filters$excluded <- FALSE
    }

    user_data$column_filters <- col_filters
    user_data$dialect_filter <- input$dialect_filter
    user_data$rank_limit <- ifelse(nchar(input$rank_limit) == 0, NULL, input$rank_limit)
    user_data$max_questions <- input$max_questions

    # Filter the word_scores to get quiz_data
    filtered <- filter_words(user_data$word_scores,
                             column_filters = user_data$column_filters,
                             dialect_filter = user_data$dialect_filter,
                             rank_limit = user_data$rank_limit)

    if (nrow(filtered) == 0) {
      user_data$feedback <- "No words available after filtering. Adjust your filters."
      return()
    }

    n_questions <- min(user_data$max_questions, nrow(filtered))
    user_data$quiz_data <- filtered %>% slice_sample(n = n_questions)

    user_data$current_question <- 1
    user_data$quiz_active <- TRUE
    user_data$feedback <- ""
  })

  # Quiz UI - shows current question and input for answer + options
  output$quiz_ui <- renderUI({
    req(user_data$quiz_active)
    qnum <- user_data$current_question
    total <- nrow(user_data$quiz_data)
    if (qnum > total) {
      return(tagList(
        h3("Quiz complete!"),
        p("Your progress has been saved."),
        actionButton("restart", "Restart quiz")
      ))
    }

    current_word <- user_data$quiz_data[qnum, ]

    tagList(
      h4(paste0("Question ", qnum, " of ", total)),
      strong("Irish word: "), current_word$ga,
      br(),
      textInput("answer", "Type the English translation:"),
      actionButton("submit_answer", "Submit Answer"),
      br(), br(),
      radioButtons("post_action", "Options after answer:",
                   choices = list(
                     "Continue" = "continue",
                     "Mark correct" = "mark_correct",
                     "Exclude word" = "exclude"
                   ),
                   selected = "continue"
      ),
      br(),
      verbatimTextOutput("current_feedback")
    )
  })

  # Display feedback after answer submit
  output$current_feedback <- renderText({
    user_data$feedback
  })

  # Process answer submit
  observeEvent(input$submit_answer, {
    req(user_data$quiz_active)
    qnum <- user_data$current_question
    current_word <- user_data$quiz_data[qnum, ]

    user_answer <- tolower(trimws(input$answer))

    if (user_answer %in% c("skip", "s", "")) {
      user_data$feedback <- "Skipped!"
      # Update skipped count in word_scores
      idx <- which(user_data$word_scores$ga == current_word$ga)
      if (length(idx) == 1) {
        user_data$word_scores$skipped_count[idx] <- user_data$word_scores$skipped_count[idx] + 1
      }

    } else {
      correct <- user_answer == tolower(current_word$en)

      # Post action overrides
      action <- input$post_action
      if (action == "mark_correct") {
        correct <- TRUE
        user_data$feedback <- "✔ Marked as correct by user."
      } else if (action == "exclude") {
        idx <- which(user_data$word_scores$ga == current_word$ga)
        if (length(idx) == 1) {
          user_data$word_scores$excluded[idx] <- TRUE
          user_data$feedback <- "🚫 Word excluded from future quizzes."
        }
        correct <- FALSE  # Don't count correct if excluded
      } else {
        # Normal feedback
        user_data$feedback <- if (correct) "✅ Correct!" else paste0("❌ Incorrect. Expected: ", current_word$en)
      }

      # Update word_scores stats if not excluded
      if (action != "exclude") {
        idx <- which(user_data$word_scores$ga == current_word$ga)
        if (length(idx) == 1) {
          user_data$word_scores$seen_count[idx] <- user_data$word_scores$seen_count[idx] + 1
          if (correct) {
            user_data$word_scores$correct_count[idx] <- user_data$word_scores$correct_count[idx] + 1
          }
          user_data$word_scores$score[idx] <- user_data$word_scores$correct_count[idx] / user_data$word_scores$seen_count[idx]
          user_data$word_scores$last_seen[idx] <- Sys.time()

          prev_hist <- user_data$word_scores$override_history[idx]
          new_entry <- ifelse(correct, "1", "0")
          if (prev_hist == "") {
            user_data$word_scores$override_history[idx] <- new_entry
          } else {
            user_data$word_scores$override_history[idx] <- paste0(prev_hist, ",", new_entry)
          }
        }
      }
    }

    # Move to next question
    user_data$current_question <- user_data$current_question + 1

    # Save scores when quiz ends
    if (user_data$current_question > nrow(user_data$quiz_data)) {
      saveRDS(user_data$word_scores, user_data$score_file)
      user_data$quiz_active <- FALSE
      user_data$feedback <- "Quiz complete! Your progress has been saved."
    }

    # Reset answer input for next question
    updateTextInput(session, "answer", value = "")
    updateRadioButtons(session, "post_action", selected = "continue")
  })

  # Restart quiz (goes back to setup)
  observeEvent(input$restart, {
    user_data$quiz_active <- FALSE
    user_data$current_question <- 0
    user_data$feedback <- ""
  })

  # Show feedback
  output$quiz_feedback <- renderText({
    user_data$feedback
  })
}

shinyApp(ui, server)
