library(shiny)
library(dplyr)
library(tibble)
library(here)

load(here::here('data', 'test_data_clean.Rdata'))

filterable_columns <- c("part_of_speech", "gender")
source(here::here('R', 'quiz_cli.R'))

ui <- fluidPage(
  titlePanel("Irish Vocabulary Quiz"),
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Enter Username:", value = ""),
      actionButton("save_user", "Enter Username"),
      numericInput("n_questions", "Number of Questions", value = 10, min = 1),
      textInput("dialect", "Dialect Filter (optional):"),

      radioButtons("rank_mode", "Choose Rank Input Mode:",
                   choices = c("Set Value" = "single", "Value Range" = "range"),
                   selected = "single", inline = TRUE),

      conditionalPanel(
        condition = "input.rank_mode == 'single'",
        sliderInput("rank_value", "Select Max Rank Value:",
                    min = 1, max = 7500, value = 100, step = 5)
      ),

      conditionalPanel(
        condition = "input.rank_mode == 'range'",
        sliderInput("rank_range", "Select Rank Range:",
                    min = 1, max = 1000, value = c(1, 100), step = 10)
      ),

      uiOutput("dynamic_filters"),
      actionButton("start_quiz", "Start Quiz"),
      hr(),
      verbatimTextOutput("quiz_status")
    ),
    mainPanel(
      uiOutput("question_ui"),
      uiOutput("feedback_ui")
    )
  )
)

server <- function(input, output, session) {

  state <- reactiveValues(
    word_scores = NULL,
    score_file = NULL,
    username = NULL
  )

  wordbank <- test_data.clean

  quiz <- reactiveValues(
    session = NULL,
    quiz_data = NULL,
    current_index = 1,
    feedback = "",
    complete = FALSE
  )

  observeEvent(input$save_user, {
    req(input$username)

    session_data <- load_user_scores(wordbank)
    state$word_scores <- session_data$word_scores
    state$score_file <- session_data$score_file
    state$username <- input$username
    showNotification(paste("Loaded user data for", input$username), type = "message")
  })

  output$dynamic_filters <- renderUI({
    req(wordbank)
    filter_uis <- lapply(filterable_columns, function(col) {
      choices <- sort(unique(na.omit(wordbank[[col]])))
      selectInput(
        inputId = paste0("filter_", col),
        label = paste("Filter by", col),
        choices = choices,
        selected = NULL,
        multiple = TRUE
      )
    })
    do.call(tagList, filter_uis)
  })

  output$quiz_status <- renderText({
    if (quiz$complete) return(quiz$feedback)
    if (is.null(quiz$quiz_data)) return("Waiting to start quiz...")
    paste("Question", quiz$current_index, "of", nrow(quiz$quiz_data))
  })

  observeEvent(input$start_quiz, {
    req(state$word_scores)

    # Handle rank input mode
    rank_input <- switch(input$rank_mode,
                         "single" = input$rank_value,
                         "range" = input$rank_range)

    selected_filters <- list()
    for (col in filterable_columns) {
      input_id <- paste0("filter_", col)
      selected_vals <- input[[input_id]]
      if (!is.null(selected_vals) && length(selected_vals) > 0) {
        selected_filters[[col]] <- selected_vals
      }
    }

    quiz_data <- filter_words(
      state$word_scores,
      column_filters = selected_filters,
      dialect_filter = input$dialect,
      rank_limit = rank_input
    )

    quiz_data <- quiz_data %>%
      filter(!excluded, !is.na(ga), !is.na(en)) %>%
      distinct(ga, .keep_all = TRUE)

    if (nrow(quiz_data) == 0) {
      quiz$feedback <- "No valid words available after filtering."
      quiz$complete <- TRUE
      return()
    }

    n_questions <- min(input$n_questions, nrow(quiz_data))
    quiz$quiz_data <- quiz_data %>% slice_sample(n = n_questions)
    quiz$current_index <- 1
    quiz$complete <- FALSE
    quiz$feedback <- ""
    quiz$session <- state
  })

  output$question_ui <- renderUI({
    req(quiz$quiz_data, !quiz$complete)
    if (quiz$current_index > nrow(quiz$quiz_data)) return(NULL)

    word <- quiz$quiz_data[quiz$current_index, "ga", drop = TRUE]
    tagList(
      strong("Translate this word from Irish:"),
      h3(word),
      textInput("user_answer", "Your Answer:"),
      actionButton("submit_answer", "Submit Answer")
    )
  })

  observeEvent(input$submit_answer, {
    req(quiz$quiz_data, input$user_answer)

    word_row <- quiz$quiz_data[quiz$current_index, ]
    answer <- trimws(tolower(input$user_answer))
    correct <- !is.na(word_row$en) && answer == tolower(word_row$en)

    idx <- which(quiz$session$word_scores$ga == word_row$ga)

    if (length(idx) == 1) {
      quiz$session$word_scores$seen_count[idx] <- quiz$session$word_scores$seen_count[idx] + 1
      if (correct) {
        quiz$session$word_scores$correct_count[idx] <- quiz$session$word_scores$correct_count[idx] + 1
      }
      quiz$session$word_scores$score[idx] <-
        quiz$session$word_scores$correct_count[idx] / quiz$session$word_scores$seen_count[idx]
      quiz$session$word_scores$last_seen[idx] <- Sys.time()

      correct_flag <- ifelse(correct, "1", "0")
      now <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      entry <- paste0(now, "|", correct_flag)

      prev_log <- quiz$session$word_scores$history_log[idx]
      quiz$session$word_scores$history_log[idx] <- ifelse(prev_log == "", entry, paste(prev_log, entry, sep = ","))

      prev_hist <- quiz$session$word_scores$override_history[idx]
      quiz$session$word_scores$override_history[idx] <- ifelse(prev_hist == "", correct_flag, paste0(prev_hist, ",", correct_flag))
    }

    quiz$feedback <- if (correct) {
      "✅ Correct!"
    } else {
      paste0("❌ Incorrect. Correct answer: ", word_row$en)
    }

    quiz$current_index <- quiz$current_index + 1

    if (quiz$current_index > nrow(quiz$quiz_data)) {
      quiz$complete <- TRUE
      saveRDS(state$word_scores, state$score_file)
      quiz$feedback <- paste(quiz$feedback, "\n\nQuiz complete! Progress saved.")
    }

    updateTextInput(session, "user_answer", value = "")
  })

  output$feedback_ui <- renderUI({
    if (quiz$feedback != "") {
      div(style = "margin-top:20px;", strong(quiz$feedback))
    }
  })
}

shinyApp(ui = ui, server = server)
