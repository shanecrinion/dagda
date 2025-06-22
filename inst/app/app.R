library(shiny)
library(dplyr)
library(dagda)
library(tibble)

test_data.clean <- readRDS(system.file("data/test_data.rds", package = "dagda"))
filterable_columns <- c("part_of_speech", "gender")

ui <- fluidPage(
  titlePanel("Irish Vocabulary Quiz"),
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Enter Username:", value = "comrad.casement"),
      helpText("Press 'Enter Username' to load example user (comrad.casement) stats."),
      actionButton("save_user", "Enter Username"),
      hr(),
      numericInput("n_questions", "Number of Questions", value = 5, min = 5, step = 5),
      hr(),
      uiOutput("next_range_button"),
      radioButtons("rank_mode", "Word rank filtering:",
                   choices = c("None" = "none", "Range" = "range", "Top Ranked" = "single"),
                   selected = "none", inline = TRUE),
      hr(),
      conditionalPanel(
        condition = "input.rank_mode == 'single'",
        numericInput("rank_value", "Enter Max Rank Value:",
                     value = 7355, min = 1, max = 7355, step = 1),

      ),
      conditionalPanel(
        condition = "input.rank_mode == 'range'",
        fluidRow(
          column(6,
                 numericInput("rank_range_min", "Start of Range:",
                              value = 0, min = 0, max = 7355)
          ),
          column(6,
                 numericInput("rank_range_max", "End of Range:",
                              value = 5, min = 5, max = 7355)
          ))),

      textInput("keyword_search", "Keyword Search:", placeholder = "Enter keyword to filter questions"),
      helpText("Include more words for limited results (e.g. Top Ranked = 7355)"),
      hr(),
      actionButton("start_quiz", "Start Quiz"),
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

  quiz <- reactiveValues(
    session = NULL,
    quiz_data = NULL,
    current_index = 1,
    feedback = "",
    complete = FALSE
  )

  wordbank <- test_data.clean

  observeEvent(input$save_user, {
    req(input$username)
    session_data <- load_user_scores(wordbank, username = input$username, interactive_mode = FALSE)
    state$word_scores <- session_data$word_scores
    state$score_file <- session_data$score_file
    state$username <- input$username
    showNotification(paste("Loaded user data for", input$username), type = "message")
  })

  observe({
    req(input$rank_mode == "range", input$rank_range_min, input$n_questions)
    updateNumericInput(session, "rank_range_max",
                       value = input$rank_range_min + input$n_questions)
  })

  filtered_quiz_data <- reactive({
    req(state$word_scores)

    selected_filters <- list()
    for (col in filterable_columns) {
      input_id <- paste0("filter_", col)
      selected_vals <- input[[input_id]]
      if (!is.null(selected_vals) && length(selected_vals) > 0) {
        selected_filters[[col]] <- selected_vals
      }
    }

    keyword <- tolower(trimws(input$keyword_search))

    rank_limit <- switch(input$rank_mode,
                         "single" = input$rank_value,
                         "range" = c(input$rank_range_min, input$rank_range_max),
                         "none" = 7355)

    if (is.null(rank_limit)) {
      quiz$feedback <- "Invalid rank settings."
      quiz$complete <- TRUE
      return(NULL)
    }

    if (is.null(rank_limit)) {
      quiz_data <- state$word_scores
    } else {
      quiz_data <- filter_words(
        state$word_scores,
        dialect_filter = input$dialect,
        rank_limit = rank_limit
      )
    }

    if (nzchar(keyword)) {
      quiz_data <- quiz_data %>%
        filter(grepl(keyword, tolower(en)) | grepl(keyword, tolower(genitiveVN)))
    }

    quiz_data %>%
      filter(!excluded, !is.na(ga), !is.na(en)) %>%
      distinct(ga, .keep_all = TRUE)
  })

  observeEvent(input$start_quiz, {
    req(state$word_scores)

    quiz_data <- filtered_quiz_data()
    if (is.null(quiz_data) || nrow(quiz_data) == 0) {
      quiz$feedback <- "No valid words available after filtering."
      quiz$complete <- TRUE
      return()
    }

    # Remove any sampling here, just take all filtered data
    quiz$quiz_data <- quiz_data

    quiz$current_index <- 1
    quiz$complete <- FALSE
    quiz$feedback <- ""
    quiz$session <- state
  })

  observeEvent(input$next_range, {
    req(input$rank_range_min, input$n_questions)
    new_min <- input$rank_range_min + input$n_questions
    new_max <- new_min + input$n_questions
    updateNumericInput(session, "rank_range_min", value = new_min)
    updateNumericInput(session, "rank_range_max", value = new_max)
    isolate({ click("start_quiz") })
  })

  output$next_range_button <- renderUI({
    if (quiz$complete && input$rank_mode == "range") {
      actionButton("next_range", paste0("Next ", input$n_questions, " in range"))
    }
  })

  output$quiz_status <- renderText({
    if (quiz$complete) return(quiz$feedback)
    if (is.null(quiz$quiz_data)) return("Waiting to start quiz...")
    paste("Question", quiz$current_index, "of", nrow(quiz$quiz_data))
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
