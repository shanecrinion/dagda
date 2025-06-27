library(shiny)
library(dplyr)
library(dagda)
library(tibble)

test_data.clean <- readRDS(system.file("data/test_data.rds", package = "dagda"))
generate_feedback_html <- function(word_row, correct = FALSE) {
  icon <- if (correct) "✔" else "✗"
  header <- if (correct) "Maith thú!" else "Mícheart!"
  css_class <- if (correct) "correct" else "incorrect"

  HTML(paste0(
    "<div class='feedback-container ", css_class, "'>",
    "<div class='feedback-header'>", icon, " <strong>", header, "</strong></div>",
    "<div class='feedback-entry'><span class='ga-label'>Gaeilge:</span> <span class='ga-text'>", word_row$ga, "</span><br></div>",
    "<div class='feedback-entry'><span class='en-label'>English:</span> <span class='en-text'>", word_row$en, "</span><br></div>",
    "<div class='feedback-entry'><span class='genitive-label'>Notes:</span><br> <span class='genitive-text'>", word_row$genitiveVN, "</span><br></div>",
    "</div>"
  ))
}

  ui <- fluidPage(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
      tags$script(HTML("
      // Pressing Enter in #username triggers #save_user
      $(document).on('keypress', '#username', function(e) {
        if (e.which == 13) {
          $('#save_user').click();
        }
      });

      // Pressing Enter in #user_answer triggers #submit_answer
      $(document).on('keypress', '#user_answer', function(e) {
        if (e.which == 13) {
          $('#submit_answer').click();
        }
      });
    "))
    ),

  titlePanel("dagda", windowTitle = "Dagda: Irish Knowledge Platform."),

  sidebarLayout(
    sidebarPanel(
      class = "custom-sidebar",
      width = 4,
      fluidRow(class = "user-row",
        column(8, textInput("username", "", value = "comrad.casement")),
        column(4,
               tags$div(style = "color: var(--accent-blue); margin-top: 22px;font-family: urgc; font-size: 16px; padding 0px;",
                        actionButton("save_user", "Enter")))
      ),
      tags$small(
        style = "margin-left: 8px; margin-right: 8px; margin-top: 8px; font-family: 'Fira Code', monospace; color: var(--accent-cream); font-size: 12px;",
        "Type username or press Enter to load example."
      ),
      # fluidRow(
      #   column(3, textInput("username", "", value = "comrad.casement")),
      #   column(1, actionButton("save_user", "Enter", style = "font-family: urgc; font-size: 16px; padding: 1px;"))),

      tags$hr(),
      numericInput("n_questions", "Questions:", value = 10, min = 5, step = 5),
      tags$hr(),
      radioButtons("quiz_order_mode", "Order:",
                   choices = c("Random" = "random", "Frequency" = "ordered"),
                   selected = "random", inline = F),

      conditionalPanel(
        condition = "input.quiz_order_mode == 'ordered'",
        radioButtons("rank_mode", "Frequency Filter:",
                     choices = c("Rank range" = "range", "Most frequent" = "single", "No filter" = "none"),
                     selected = "range", inline = F),

        conditionalPanel(
          condition = "input.rank_mode == 'range'",
          fluidRow(class = "ranker",
            column(6, numericInput("rank_range_min", "Start:", value = 0, min = 0, max = 7355, step = 5)),
            column(6, numericInput("rank_range_max", "End:", value = 5, min = 5, max = 7355, step = 5))
          )),

        conditionalPanel(
          condition = "input.rank_mode == 'single'",
          numericInput("rank_value", "Enter value (i.e. 100 = top 100 words, 10% = top 10%):", value = 100, min = 100, max = 7355, step = 5)
        )
      ),
      tags$hr(),
      checkboxInput("enable_attrib_filter", label = tags$span("Focused Quiz?", class = "form-label"), value = FALSE),
      conditionalPanel(
        condition = "input.enable_attrib_filter",
        selectInput(
          inputId = "attrib",
          label = "Select for a focused quiz:",
          choices = NULL,
          multiple = TRUE)),

      tags$hr(),
      actionButton("start_quiz", "Start Quiz", class = "btn-primary"),
      br(),
     # verbatimTextOutput("quiz_status")
    ),

    mainPanel(
      fluidRow(column(6,uiOutput("question_ui")),
              column(6, uiOutput("feedback_ui")))
    )
  )
)

server <- function(input, output, session) {

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

  wordbank <- reactiveVal(test_data.clean)

  observeEvent(input$save_user, {
    req(input$username)
    session_data <- load_user_scores(wordbank(), username = input$username, interactive_mode = FALSE)
    state$word_scores <- session_data$word_scores
    state$score_file <- session_data$score_file
    state$username <- input$username
    showNotification(paste("Loaded user data for", input$username), type = "message")
  })

  observe({
    wb <- wordbank()
    req(wb)

    gender_list <- sort(unique(unlist(strsplit(na.omit(wb$gender), '\n'))))
    gender_list <- gender_list[gender_list != ""]
    gender_list <- as.character(gender_list)

    pos_list <- sort(unique(na.omit(wb$part_of_speech)))
    pos_list <- as.character(pos_list)

    choices <- c(
      paste0("gender=", gender_list),
      paste0("part_of_speech=", pos_list)
    )

    updateSelectInput(
      session,
      inputId = "attrib",
      label = "",
      choices = choices
    )
  })

  observe({
    req(input$rank_mode == "range", input$rank_range_min, input$n_questions)
    updateNumericInput(session, "rank_range_max",
                       value = input$rank_range_min + input$n_questions)
  })

  quiz_data <- reactive({
    req(state$word_scores)
    data <- state$word_scores

    if (!is.null(input$attrib) && length(input$attrib) > 0) {
      for (att in input$attrib) {
        split_att <- strsplit(att, "=")[[1]]
        col <- split_att[1]
        val <- split_att[2]
        data <- data[data[[col]] == val, ]
      }
    }

    rank_mode <- input$rank_mode
    if (input$quiz_order_mode == "ordered" && rank_mode != "none") {
      rank_limit <- switch(rank_mode,
                           "single" = input$rank_value,
                           "range" = c(input$rank_range_min, input$rank_range_max))
      data <- filter_words(data, rank_limit = rank_limit)
    }

    data %>%
      filter(!excluded, !is.na(ga), !is.na(en)) %>%
      distinct(ga, .keep_all = TRUE)
  })

  filtered_quiz_data <- quiz_data

  start_quiz_logic <- function() {
    req(filtered_quiz_data())
    quiz_data_df <- filtered_quiz_data()

    if (is.null(quiz_data_df) || nrow(quiz_data_df) == 0) {
      quiz$feedback <- "No valid words available after filtering."
      quiz$complete <- TRUE
      return()
    }

    if (input$quiz_order_mode == "random") {
      quiz_data_df <- quiz_data_df %>% slice_sample(n = input$n_questions)
    } else {
      if (nrow(quiz_data_df) > input$n_questions) {
        quiz_data_df <- quiz_data_df %>% slice_head(n = input$n_questions)
      }
    }

    quiz$quiz_data <- quiz_data_df
    quiz$last_quiz_words <- quiz_data_df
    quiz$current_index <- 1
    quiz$complete <- FALSE
    quiz$feedback <- ""
    quiz$session <- state
  }

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

  # output$quiz_status <- renderText({
  #   if (quiz$complete) return(quiz$feedback)
  #   if (is.null(quiz$quiz_data)) return("Waiting to start quiz...")
  #   paste("Question", quiz$current_index, "of", nrow(quiz$quiz_data))
  # })

  output$question_ui <- renderUI({
    req(quiz$quiz_data, !quiz$complete)
    if (quiz$current_index > nrow(quiz$quiz_data)) return(NULL)

    word <- quiz$quiz_data[quiz$current_index, "ga", drop = TRUE]

    tagList(#40637c
      div(class = "card bg-light mb-3", style = "padding: 0px; box-shadow: 0 12px 16px 0 rgba(0,0,0,0.24), 0 17px 50px 0 rgba(0,0,0,0.19);",
          h3("Irish: " , class = "prompt-label", style = "margin-left: 5px; font-size: 2rem; color: var(--accent-cream);"),
          h3(word, style = "margin-bottom: 5px; font-family: 'Fira Code', monospace; color: var(--accent-cream); font-size: 2rem; "),
          h4(textInput("user_answer", "",placeholder = 'Enter English translation..'),style = "margin-bottom: 10px; color: var(--accent-cream); font-family: 'Fira Code', monospace;"),
          actionButton("submit_answer", "Submit")
      )
    )
  })

  observeEvent(input$submit_answer, {
    req(quiz$quiz_data, input$user_answer)
    user_input <- isolate(input$user_answer)
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
      quiz$feedback <- generate_feedback_html(word_row, correct = TRUE)

    } else {
      quiz$feedback <- generate_feedback_html(word_row, correct = FALSE)
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
    is_excluded <- current_word %in% state$word_scores$ga[state$word_scores$excluded]


    # Feedback text area with padding and background
    feedback_text <- div(
  #    style = "padding: 5px; background-color: #f8f9fa; border-radius: 8px; margin-bottom: 15px; font-size: 16px;",
      HTML(quiz$feedback)
    )

    # Buttons styled and spaced well
    if (!quiz$complete) {
      # During quiz: interactive buttons in one row
      buttons <- fluidRow(class='silly',
        column(6, actionButton("mark_correct", "✅Override", class = "btn btn-success btn-block")),
        column(6, actionButton(
          "toggle_exclude_word",
          if (is_excluded) "♻️ Un-Exclude Word" else "🚫 Exclude",
          class = if (is_excluded) "btn btn-warning btn-block" else "btn btn-danger btn-block"
        ))
      )
    } else {
      # After quiz: repeat and next range options
      buttons_list <- list()

      if (!is.null(quiz$last_quiz_words)) {
        buttons_list <- c(buttons_list,
                          fluidRow(
                            column(12,
                                   actionButton("repeat_same_words", "🔁 Same Words Again?", class = "btn btn-primary btn-block",
                                                disabled = is.null(quiz$last_quiz_words))
                            )
                          )
        )
      }

      if (input$rank_mode == "range" && input$quiz_order_mode != "random") {
        buttons_list <- c(buttons_list,
                          fluidRow(
                            column(12,
                                   actionButton("next_range",
                                                paste0("➡️ Next ", input$n_questions, " Most Common Words"),
                                                class = "btn btn-secondary btn-block",
                                                disabled = is.null(quiz$quiz_data))
                            )
                          )
        )
      }

      buttons <- tagList(buttons_list)
    }

    # Return combined UI elements inside a well panel for subtle box effect
    wellPanel(
      feedback_text,
      buttons
    )
  })

  observeEvent(input$toggle_exclude_word, {
    req(quiz$current_index > 1, quiz$quiz_data)
    current <- quiz$quiz_data[quiz$current_index - 1, "ga", drop = TRUE]

    if (is.null(current)) return()

    # Find the current exclusion status from word_scores
    is_current_excluded <- state$word_scores$excluded[state$word_scores$ga == current]

    if (isTRUE(is_current_excluded)) {
      # Un-exclude the word
      state$word_scores$excluded[state$word_scores$ga == current] <- FALSE
      showNotification("✅ Word included again!", type = "message")
    } else {
      # Exclude the word
      state$word_scores$excluded[state$word_scores$ga == current] <- TRUE
      showNotification("🚫 Word excluded!", type = "warning")
    }
  })
}

shinyApp(ui = ui, server = server)
