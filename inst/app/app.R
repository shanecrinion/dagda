library(shiny)
library(dplyr)
library(dagda)
library(tibble)


test_data.clean <- readRDS(system.file("data/test_data_clean.rds", package = "dagda"))

#load(system.file("data", "test_data_clean.Rdata", package = "dagda"))

filterable_columns <- c("part_of_speech", "gender")
#source(system.file("R", "quiz_cli.R", package = "dagda"))


ui <- fluidPage(
  titlePanel("Irish Vocabulary Quiz"),
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Enter Username:", value = ""),
      actionButton("save_user", "Enter Username"),
      numericInput("n_questions", "Number of Questions", value = 5, min = 5),
      textInput("dialect", "Dialect Filter (optional):"),

      radioButtons("rank_mode", "Choose Rank Input Mode:",
                   choices = c("Set Value" = "single", "Value Range" = "range"),
                   selected = "single", inline = TRUE),

      conditionalPanel(
        condition = "input.rank_mode == 'single'",
        numericInput("rank_value", "Enter Max Rank Value:",
                     value = 100, min = 1, max = 7357, step = 1)
      ),

      conditionalPanel(
        condition = "input.rank_mode == 'range'",
        fluidRow(
          column(6,
                 numericInput("rank_range_min", "Start of Range:",
                              value = 0, min = 0, max = 50, step = 1)
          ),
          column(6,
                 numericInput("rank_range_max", "End of Range:",
                              value = 10, min = 0, max = 50, step = 1)
          )
        ),
        actionButton("next_range_words", "Next 10 Words in Range")
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

  quiz$range_offset <- 0

  # observe count of words
  observeEvent(input$next_range_words, {
  req(filtered_quiz_data())

  data <- filtered_quiz_data()

  start_idx <- quiz$range_offset + 1
  end_idx <- start_idx + 9

  if (start_idx > nrow(data)) {
    showNotification("No more words in this range.", type = "warning")
    return()
  }

  quiz$quiz_data <- data[start_idx:min(end_idx, nrow(data)), ]
  quiz$current_index <- 1
  quiz$complete <- FALSE
  quiz$feedback <- ""

  quiz$range_offset <- quiz$range_offset + 10
})

  observe({
    input$rank_range_min
    input$rank_range_max
    quiz$range_offset <- 0
  })

  observeEvent(input$save_user, {
    req(input$username)
    paste('Entering info for', input$username)

    session_data <- load_user_scores(wordbank, username = input$username, interactive_mode = FALSE)
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

    rank_limit <- switch(input$rank_mode,
                         "single" = input$rank_value,
                         "range" = c(input$rank_range_min, input$rank_range_max),
                         NULL)

    if (is.null(rank_limit)) {
      quiz$feedback <- "Invalid rank settings."
      quiz$complete <- TRUE
      return(NULL)
    }

    quiz_data <- filter_words(
      state$word_scores,
      column_filters = selected_filters,
      dialect_filter = input$dialect,
      rank_limit = rank_limit
    )

    quiz_data %>%
      filter(!excluded, !is.na(ga), !is.na(en)) %>%
      distinct(ga, .keep_all = TRUE)
  })


  observeEvent(input$start_quiz, {
    req(state$word_scores)

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
      rank_limit = input$rank_value
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
