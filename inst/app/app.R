library(shiny,quietly = T)
library(dplyr,quietly = T)
library(dagda,quietly = T)
library(tibble,quietly = T)

test_data.clean <- readRDS(system.file("data/test_data.rds", package = "dagda"))
filterable_columns <- c("part_of_speech", "gender")


ui <- fluidPage(
  titlePanel("Vocab Quiz "),
  sidebarLayout(
    sidebarPanel(
      textInput("username", "Enter Username:", value = "comrad.casement"),
      helpText("Press 'Enter Username' to load example user data or type your desired username."),
      actionButton("save_user", "Enter Username"),
      hr(),
      numericInput("n_questions", "Number of Questions", value = 5, min = 5, step = 5),
      hr(),
      radioButtons("quiz_order_mode", "Order:",
                   choices = c("Word frequency" = "ordered",
                               "Random" = "random"),
                   selected = "ordered", inline = TRUE),
      hr(),
      conditionalPanel(
        condition = "input.quiz_order_mode == 'ordered'",
        tagList(
          radioButtons("rank_mode", "Order by frequency:",
                       choices = c("Range" = "range", "Top Ranked" = "single", "None" = "none"),
                       selected = "range", inline = TRUE),

          conditionalPanel(
            condition = "input.rank_mode == 'range'",
            fluidRow(
              column(2.5,
                     numericInput("rank_range_min", "Start of Range:",
                                  value = 0, min = 0, max = 7355, step=5)
              ),
              column(2.5,
                     numericInput("rank_range_max", "End of Range:",
                                  value = 5, min = 5, max = 7355, step = 5)
              )
            )
          ),

          conditionalPanel(
            condition = "input.rank_mode == 'single'",
            numericInput("rank_value", "Enter Max Rank Value:",
                         value = 7355, min = 1, max = 7355, step = 1)
          ))),

      textInput("keyword_search", "Keyword Search:", placeholder = "Enter keyword to filter questions"),
      helpText("Include more words for limited results (e.g. Top Ranked = 7355)"),
      hr(),
      actionButton("start_quiz", "Start"),
      verbatimTextOutput("quiz_status")
    ),
    mainPanel(
     uiOutput("question_ui"),
      uiOutput("feedback_ui")
    )
  )
)

server <- function(input, output, session) {

  start_quiz_logic <- function() {
    req(state$word_scores)

    quiz_data <- filtered_quiz_data()
    if (is.null(quiz_data) || nrow(quiz_data) == 0) {
      quiz$feedback <- "No valid words available after filtering."
      quiz$complete <- TRUE
      return()
    }

    if (input$quiz_order_mode == "random") {
      quiz_data <- state$word_scores %>%
        filter(!excluded, !is.na(ga), !is.na(en)) %>%
        distinct(ga, .keep_all = TRUE) %>%
        slice_sample(n = input$n_questions)
    } else {
      if (nrow(quiz_data) > input$n_questions) {
        quiz_data <- quiz_data %>% slice_head(n = input$n_questions)
      }
    }

    quiz$quiz_data <- quiz_data
    quiz$last_quiz_words <- quiz_data
    quiz$current_index <- 1
    quiz$complete <- FALSE
    quiz$feedback <- ""
    quiz$session <- state
  }

  state <- reactiveValues(
    word_scores = NULL,
    score_file = NULL,
    username = NULL)

  quiz <- reactiveValues(
    session = NULL,
    quiz_data = NULL,
    current_index = 1,
    feedback = "",
    complete = FALSE,
    last_quiz_words = NULL)

  excluded_words <- reactiveVal(character())

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
    rank_mode <- input$rank_mode

    # Start with all words if rank_mode is "none", otherwise filter
    if (rank_mode == "none") {
      quiz_data <- state$word_scores
    } else {
      rank_limit <- switch(rank_mode,
                           "single" = input$rank_value,
                           "range" = c(input$rank_range_min, input$rank_range_max))

      # If rank_limit is still NULL here, error
      if (is.null(rank_limit)) {
        quiz$feedback <- "Invalid rank settings."
        quiz$complete <- TRUE
        return(NULL)
      }


      quiz_data <- filter_words(
        state$word_scores,
        rank_limit = rank_limit
      )
    }

    # Apply keyword filter
    if (nzchar(keyword)) {
      quiz_data <- quiz_data %>%
        filter(grepl(keyword, tolower(en)) | grepl(keyword, tolower(genitiveVN)))
    }

    # Final cleanup and return
    quiz_data %>%
      filter(!excluded, !is.na(ga), !is.na(en)) %>%
      distinct(ga, .keep_all = TRUE)
  })

  observeEvent(input$start_quiz, {
    if (is.null(state$username)) {
      showNotification("⚠️ Please press 'Enter Username' before starting the quiz.", type = "warning")
      return()
    }
    start_quiz_logic()
  })


  observeEvent(input$repeat_same_words, {
    if (is.null(quiz$last_quiz_words) || is.null(quiz$session)) {
      showNotification("⚠️ No previous quiz data available. Please start a quiz first.", type = "warning")
      return()
    }

    quiz$quiz_data <- quiz$last_quiz_words %>% slice_sample(n = nrow(.))
    quiz$current_index <- 1
    quiz$feedback <- ""
    quiz$complete <- FALSE

    showNotification("Restarted quiz with same words.", type = "message")
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
    paste("Question", quiz$current_index, "of", nrow(quiz$quiz_data))
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
      paste0("❌ Incorrect. \nCorrect answer: \n",word_row$en)
    }

    quiz$current_index <- quiz$current_index + 1

    if (quiz$current_index > nrow(quiz$quiz_data)) {
      quiz$complete <- TRUE
      saveRDS(state$word_scores, state$score_file)
      quiz$feedback <- paste(quiz$feedback, "\n\nQuiz complete! Progress saved.")
    }

    updateTextInput(session, "user_answer", value = "")
  })

  observeEvent(input$mark_correct, {
    req(quiz$quiz_data, quiz$current_index > 1)
    idx <- quiz$current_index - 1
    word_row <- quiz$quiz_data[idx, ]
    match_idx <- which(quiz$session$word_scores$ga == word_row$ga)

    if (length(match_idx) == 1) {
      quiz$session$word_scores$correct_count[match_idx] <-
        quiz$session$word_scores$correct_count[match_idx] + 1
      quiz$session$word_scores$score[match_idx] <-
        quiz$session$word_scores$correct_count[match_idx] /
        quiz$session$word_scores$seen_count[match_idx]

      # Update override history
      prev_hist <- quiz$session$word_scores$override_history[match_idx]
      new_hist <- gsub("0$", "1", prev_hist)
      quiz$session$word_scores$override_history[match_idx] <- new_hist

      saveRDS(state$word_scores, state$score_file)
      showNotification("Answer marked as correct and saved.", type = "message")
    }

  })

  observeEvent(input$exclude_word, {
    req(quiz$quiz_data, quiz$current_index > 1)
    idx <- quiz$current_index - 1
    word_row <- quiz$quiz_data[idx, ]
    match_idx <- which(quiz$session$word_scores$ga == word_row$ga)

    if (length(match_idx) == 1) {
      quiz$session$word_scores$excluded[match_idx] <- TRUE
      saveRDS(state$word_scores, state$score_file)
      showNotification("Word excluded from future quizzes.", type = "warning")
    }
  })

  observeEvent(input$next_range, {
    req(state$word_scores)
    req(input$rank_range_min, input$n_questions)

    new_min <- input$rank_range_min + input$n_questions + 1
    new_max <- new_min + input$n_questions

    updateNumericInput(session, "rank_range_min", value = new_min)
    updateNumericInput(session, "rank_range_max", value = new_max)
  })

  observeEvent(input$rank_range_min, {
    # Only start quiz if we have a valid user and word_scores loaded
    req(state$username, state$word_scores)
    start_quiz_logic()
  })

  output$feedback_ui <- renderUI({
    req(quiz$feedback != "")

    current_word <- quiz$quiz_data[quiz$current_index - 1, "ga", drop = TRUE]
    is_excluded <- current_word %in% excluded_words()

    feedback_elements <- list(
      div(style = "margin-top:20px;", quiz$feedback)
    )

    if (!quiz$complete) {
      # During quiz: show interactive buttons
      feedback_elements <- append(feedback_elements, list(
        actionButton("mark_correct", "✅ Overwrite as Correct"),
        actionButton(
          "toggle_exclude_word",
          if (is_excluded) "♻️ Un-Exclude Word" else "🚫 Exclude Word"
        )
      ))
    } else {
      # After quiz: show repeat and next options
      if (!is.null(quiz$last_quiz_words)) {
        feedback_elements <- append(feedback_elements, list(
          actionButton(
            "repeat_same_words",
            "🔁 Same Words Again?",
            disabled = is.null(quiz$last_quiz_words)
          )
        ))
      }

      if (input$rank_mode == "range" && input$quiz_order_mode != "random") {
        feedback_elements <- append(feedback_elements, list(
          actionButton(
            "next_range",
            paste0("➡️ Next ", input$n_questions, " Most Common Words"),
            disabled = is.null(quiz$quiz_data)
          )
        ))
      }
    }

    tagList(feedback_elements)
  })

  observeEvent(input$toggle_exclude_word, {
    current <- quiz$quiz_data[quiz$current_index - 1, "ga", drop = TRUE]
    if (is.null(current)) return()

    current_excluded <- excluded_words()

    if (current %in% current_excluded) {
      excluded_words(setdiff(current_excluded, current))
      quiz$session$word_scores$excluded[quiz$session$word_scores$ga == current] <- FALSE
      showNotification("✅ Word included again!", type = "message")
    } else {
      excluded_words(c(current_excluded, current))
      quiz$session$word_scores$excluded[quiz$session$word_scores$ga == current] <- TRUE
      showNotification("🚫 Word excluded!", type = "warning")
    }

    saveRDS(state$word_scores, state$score_file)
  })}

shinyApp(ui = ui, server = server)
