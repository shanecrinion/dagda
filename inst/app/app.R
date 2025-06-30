library(shiny)
library(dplyr)
library(dagda)
library(tibble)
library(stringr)

# Load test data (adjust path if needed)
test_data.clean <- readRDS(system.file("data", "test_data.rds", package = "dagda"))
# test_data.clean <- readRDS('user_data/word_scores_comrad.casement.rds')

# Returns a string of HTML for feedback
generate_feedback_html <- function(word_row, correct = FALSE) {
  icon <- if (correct) "🎈" else "💀"
  header <- if (correct) "<span style='color: green;'>Maith thú!</span>" else "<span style='color: red;'>Mícheart!</span>"
  css_class <- if (correct) "correct" else "incorrect"

  feedback_text <- paste0(
    "<div class='feedback-container ", css_class, "'>",
    "<div class='feedback-header' style='color: #4a2e4a;'>", icon, " <strong>", header, "</strong></div>",
    "<div class='feedback-entry'><span class='ga-label'><br>Gaeilge:</span> <span class='ga-text'>", word_row$ga, "</span><br></div>",
    "<div class='feedback-entry'><span class='en-label'><br>English:</span> <span class='en-text'>", word_row$main_term, "</span><br></div>",
    "<div class='feedback-entry'><span class='genitive-label'><br>Notes:</span><br> <span class='genitive-text'>", word_row$genitiveVN, "</span><br></div>",
    "<div class='feedback-entry'><span class='example-label'><br>Tooltips:</span><br> <span class='example-text'>", word_row$tooltips, "</span><br></div>",
    "</div>"
  )

    # Construct UI output
  tagList(
    HTML(feedback_text),
    if (!correct) {
    actionButton("mark_correct", "✅ Override", class = "btn btn-success btn-block")
    },
    if (correct) {
    actionButton("exclude", "Exclude!")
    })}


ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(HTML("
      // When pressing Enter in #username or #user_answer, trigger appropriate button clicks.
      $(document).on('keypress', '#username', function(e) {
        if (e.which == 13) { $('#save_user').click(); }
      });
      $(document).on('keypress', '#user_answer', function(e) {
        if (e.which == 13) { $('#submit_answer').click(); }
      });
    "))
  ),

  titlePanel("dagda", windowTitle = "Dagda: Irish Knowledge Platform."),

  sidebarLayout(
    sidebarPanel(
      class = "custom-sidebar",
      width = 3,
      fluidRow(class = "user-row",
               column(8, textInput("username", "Username:", value = "comrad.casement")),
               column(4, tags$div(style = "color: var(--sand); margin-top: 45px; background-color: var(--sand); font-family: urgc; font-size: 16px;",
                                  actionButton("save_user", "Enter")))
      ),
      tags$small(
        style = "text-align: center; padding: 0px; margin: 10px; font-family: 'Fira Code', monospace; color: var(--lichen); font-size: 12px;",
        "Type username or load example"
      ),
      numericInput("n_questions", "Questions:", value = 10, min = 5, step = 5),
      selectInput(
        inputId = "attrib",
        label = "Select for a focused quiz:",
        choices = NULL,
        multiple = TRUE),
      radioButtons("quiz_order_mode", "Order:",
                   choices = c("Random" = "random", "Frequency" = "ordered"),
                   selected = "random", inline = FALSE),
      conditionalPanel(
        condition = "input.quiz_order_mode == 'ordered'",
        numericInput("rank_value", "Max:", value = 1000, min = 100, max = 7010, step = 5),
        helpText('e.g. Top 1000 words by freq (max=7010)'),
        checkboxInput('randomise', 'Randomise after filtering?', value = TRUE)
      ),
      actionButton("start_quiz", "Start Quiz", class = "btn-primary")
    ),

    mainPanel(
      fluidRow(
        column(6, uiOutput("question_ui")),
        column(6, uiOutput("feedback_ui"))
      )
    )
  )
)

server <- function(input, output, session) {

  # Reactive state for user data and quiz
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
    complete = FALSE,
    last_quiz_words = NULL,
    last_correct = NULL
  )

  message('Loading data...')
  wordbank <- reactiveVal(test_data.clean)
  message('Data loaded successfully.')

  # 1. Load user
  observeEvent(input$save_user, {
    req(input$username)
    session_data <- load_user_scores(wordbank(), username = input$username, interactive_mode = FALSE)
    state$word_scores <- session_data$word_scores
    state$score_file <- session_data$score_file
    state$username <- input$username
    showNotification(paste("Loaded user data for", input$username), type = "message")
  })

  # 2. Filter on category and rank
  observe({
    wb <- wordbank()
    req(wb)

    gender_list <- str_trim(sort(unique(unlist(strsplit(na.omit(wb$gender_clean), ',')))))
    gender_list <- gender_list[gender_list != ""]
    # Remove troublesome values
    mistakes <- c('f2', 'nf3', 'ag seinm', 'mn1', 'nm2', 'm')
    gender_list <- gender_list[!gender_list %in% mistakes]
    gender_list <- as.character(sort(gender_list))

    pos_list <- sort(unique(na.omit(wb$part_of_speech)))
    pos_list <- as.character(pos_list)
    special_pos <- c("Verbal Noun", "Irregular Verb")
    pos_list <- sort(unique(c(pos_list, special_pos)))

    subjects <- sort(names(table(wb$subjectField)))
    subjects <- subjects[!subjects %in% c("4627704", "4637300")]

    choices <- c(
      paste0("gender=", gender_list),
      paste0("part_of_speech=", pos_list),
      paste0("subjectField=", subjects)
    )

    updateSelectInput(session, inputId = "attrib", label = "Categories:", choices = choices)
  })

  quiz_data <- reactive({
    req(state$word_scores)
    data <- state$word_scores

    if (!is.null(input$attrib) && length(input$attrib) > 0) {
      for (att in input$attrib) {
        split_att <- strsplit(att, "=")[[1]]
        col <- split_att[1]
        val <- split_att[2]

        if (col == "part_of_speech" && val == "Verbal Noun") {
          data <- data[stringr::str_detect(data$pos, regex("verbal noun", ignore_case = TRUE)), ]
        } else if (col == "part_of_speech" && val == "Irregular Verb") {
          data <- data[stringr::str_detect(data$pos, regex("irregular", ignore_case = TRUE)), ]
        } else {
          data <- data[data[[col]] == val, ]
        }
      }
    }

    if (input$quiz_order_mode == "ordered") {
      rank_limit <- input$rank_value
      data <- filter_words(data, rank_limit = rank_limit)
      if (isTRUE(input$randomise) && nrow(data) > 0) {
        data <- data[sample(nrow(data)), ]
      }
    }

    print(paste("Words selected:", length(unique(data$main_term)),
                "| Randomised:", isTRUE(input$randomise)))

    data %>%
      filter(!excluded, !is.na(ga), !is.na(en)) %>%
      distinct(ga, .keep_all = TRUE)
  })

  filtered_quiz_data <- quiz_data

  # Start or restart the quiz
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

  output$question_ui <- renderUI({
    req(quiz$quiz_data, !quiz$complete)
    if (quiz$current_index > nrow(quiz$quiz_data)) return(NULL)

    word <- quiz$quiz_data[quiz$current_index, "ga", drop = TRUE]

    tagList(
      div(
        style = "display: flex; align-items: baseline; gap: 8px; margin-left: 5px; margin-bottom: 5px;",
        h3("Irish:", class = "prompt-label", style = "font-size: 2rem; color: #7a6248;"),
        h3(word, style = "font-size: 2rem; font-family: 'Fira Code', monospace; color: #7a6248;")
      ),
      div(
        style = "display: flex; align-items: center; gap: 10px; margin-left: 5px; margin-bottom: 10px;",
        textInput("user_answer", "", placeholder = "Enter English translation..", width = "200px"),
        actionButton("submit_answer", "Submit")
      )
    )
  })

  # When the user submits an answer, update feedback and move to the next word
  observeEvent(input$submit_answer, {
    req(quiz$quiz_data, trimws(input$user_answer) != "")
    user_input <- isolate(input$user_answer)
    word_row <- quiz$quiz_data[quiz$current_index, ]
    answer <- trimws(tolower(user_input))
    correct <- !is.na(word_row$en) && answer == tolower(word_row$main_term)

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

    # Update feedback and record if the answer was correct
    quiz$feedback <- generate_feedback_html(word_row, correct = correct)
    quiz$last_correct <- correct

    # Advance quiz index after feedback is generated
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

      prev_hist <- quiz$session$word_scores$override_history[match_idx]
      new_hist <- gsub("0$", "1", prev_hist)
      quiz$session$word_scores$override_history[match_idx] <- new_hist

      saveRDS(state$word_scores, state$score_file)
      showNotification("Answer marked as correct and saved.", type = "message")
    }
  })

  output$feedback_buttons <- renderUI({
    actionButton("mark_correct", "✅ Override", class = "btn btn-success btn-block")
  })


  observeEvent(input$toggle_exclude_word, {
    req(quiz$current_index > 1, quiz$quiz_data)
    current <- quiz$quiz_data[quiz$current_index - 1, "ga", drop = TRUE]
    if (is.null(current)) return()

    # Toggle current word's exclusion status
    is_current_excluded <- state$word_scores$excluded[state$word_scores$ga == current]
    if (isTRUE(is_current_excluded)) {
      state$word_scores$excluded[state$word_scores$ga == current] <- FALSE
      showNotification("✅ Word included again!", type = "message")
    } else {
      state$word_scores$excluded[state$word_scores$ga == current] <- TRUE
      showNotification("🚫 Word excluded!", type = "warning")
    }
  })


  # Feedback UI: Displays the feedback HTML and the corresponding button
  output$feedback_ui <- renderUI({
    req(quiz$feedback != "")

    # Use the word from the just-answered question (index - 1)
    current_word <- quiz$quiz_data[quiz$current_index - 1, "ga", drop = TRUE]
    is_excluded <- current_word %in% state$word_scores$ga[state$word_scores$excluded]

    feedback_text <- quiz$feedback

    wellPanel(
      feedback_text)
  })
}

shinyApp(ui = ui, server = server)
