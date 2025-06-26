library(xml2)
library(rvest)

### generate some dates for last seen,
set.seed(123)  # for reproducibility

# read in user history as a slight template
user_data <- readRDS("inst/app/user_data/word_scores_comrad.casement.rds") # read in user history

simulated.data =
  data.frame(
    ga = user_data$ga,
    en = user_data$en, # needs parsing, maybe by html code
    lemma = user_data$lemma,
    rank = user_data$rank,
    part_of_speech = user_data$part_of_speech,
    pos = user_data$pos,
    gender = user_data$gender,
    genitiveVN = user_data$genitiveVN, #html parsing needed
    seen_count = round(rnorm(n = nrow(user_data), mean = 25, sd = 5)),
    correct_count = round(rnorm(n = nrow(user_data) , mean = 10, sd=3)),
    excluded = as.logical(rbinom(nrow(user_data), size = 1, prob = 0.1)))

# follow up calculations
simulated.data$accuracy = simulated.data$correct_count / simulated.data$seen_count

# Define the range
start_date <- as.POSIXct("2025-05-01 00:00:00", tz = "IST")
end_date <- as.POSIXct("2025-06-22 23:59:59", tz = "IST")

# Generate nrow(user_data) random datetime values in the range
random_datetimes <- as.POSIXct(runif(nrow(user_data), as.numeric(start_date), as.numeric(end_date)), origin = "1970-01-01", tz = "IST")

# Add this to the simulated data df
simulated.data$last_seen <- random_datetimes

# Assume user_data and simulated.data already exist
n_strings <- nrow(user_data)
string_length <- simulated.data$seen_count  # Vector of lengths per row

# 1. Generate sigmoid-like increasing binary strings
binary_strings <- lapply(seq_len(n_strings), function(i) {
  len <- string_length[i]
  probs <- seq(0, 1, length.out = len)^2  # Quadratic increase
  bits <- rbinom(len, size = 1, prob = probs)
  paste(bits, collapse = ",")
})

# 2. Randomly modify some strings to ensure trailing 5–7 ones
n_modify <- min(500, n_strings)  # prevent sampling more than available
indices_to_modify <- sample(seq_len(n_strings), n_modify)

binary_strings[indices_to_modify] <- lapply(indices_to_modify, function(i) {
  bits <- as.integer(unlist(strsplit(binary_strings[[i]], ",")))
  len <- length(bits)
  overwrite_length <- min(sample(5:7, 1), len)  # avoid exceeding string length
  bits[(len - overwrite_length + 1):len] <- 1
  paste(bits, collapse = ",")
})

# 3. Write to simulated.data
simulated.data$override_history <- unlist(binary_strings)


#  extract the start time
date_list <- simulated.data$last_seen

# seen count so we know how many dates to generate
seen_count <- simulated.data$seen_count

# Generate gradual 0->1 answers per history entry (length = seen_count[i])
#    Using increasing probability from 0 to 1 along the history length per string
generate_answers <- function(n) {
  probs <- seq(0, 1, length.out = n)^2  # quadratic increase
  rbinom(n, 1, prob = probs)
}

# Construct the history_log strings
history_log <- mapply(function(date, count) {
  answers <- generate_answers(count)
  # Format each entry as "date|answer"
  entries <- paste0(format(date, "%Y-%m-%d %H:%M:%S"), "|", answers)
  paste(entries, collapse = ",")
}, date_list, seen_count, SIMPLIFY = TRUE)

# history_log is a character vector length 50 with desired format
simulated.data$history_log <- history_log

n_nonzero <- 1000  # ~20% non-zero

# Start with all zeros
values <- rep(0, nrow(user_data))

# Randomly choose positions to insert non-zero values
nonzero_indices <- sample(1:nrow(user_data), n_nonzero)

# Insert values from 1 to 5 (random)
values[nonzero_indices] <- sample(1:5, n_nonzero, replace = TRUE)

# Wrap in a list if needed
value_list <- as.list(values)

# add to df
simulated.data$skipped_count <- unlist(value_list)

# Plain-text extractor (robust to nested and malformed HTML)
strip_html <- function(x) {
  vapply(x, function(text) {
    xml_text(read_html(paste0("<body>", text, "</body>")))
  }, character(1))
}

# generate plain versions
simulated.data$en_plain <- strip_html(simulated.data$en)
simulated.data$genitiveVN_plain <- strip_html(simulated.data$genitiveVN)

# html parsing
extract_small_notes <- function(x) {
  sapply(x, function(text) {
    html <- read_html(paste0("<body>", text, "</body>"))
    smalls <- html_elements(html, "small")
    paste(html_text(smalls), collapse = "; ")
  })
}

# extract rough notes
simulated.data$tooltips <- extract_small_notes(simulated.data$en)

extract_main_term <- function(x) {
  # Step 1: Remove [AUTO] entirely
  x <- gsub("\\[AUTO\\]", "", x, ignore.case = TRUE)

  # Step 2: Extract bracket contents (excluding [AUTO], already removed)
  brackets <- regmatches(x, gregexpr("\\[[^\\]]+\\]", x))
  brackets <- unlist(brackets)

  # Keep only content from square brackets (remove brackets)
  if (length(brackets) > 0) {
    brackets_clean <- gsub("^\\[|\\]$", "", brackets)
  } else {
    brackets_clean <- character(0)
  }

  # Step 3: Extract everything before the first < or \n
  prefix <- sub("[\n<].*", "", x)
  prefix <- gsub("\\[.*?\\]", "", prefix)  # Remove any remaining bracketed expressions
  prefix <- trimws(prefix)

  # Step 4: Collapse result: use prefix if not empty, else fallback to cleaned bracketed values
  if (nzchar(prefix)) {
    return(prefix)
  } else if (length(brackets_clean) > 0) {
    return(trimws(paste(brackets_clean, collapse = " ")))
  } else {
    return("")
  }
}

simulated.data$main_term <- vapply(simulated.data$en, extract_main_term, character(1))

# words that need work - mostly alternative spelling and none of the top 760 ranked terms
irish_terms_need_manual <- simulated.data[simulated.data$main_term=='',]

length(unique(irish_terms_need_manual$lemma)) # 562

# remove temporarity
simulated.data <- simulated.data[!simulated.data$main_term=='',]

length(unique(simulated.data$ga)) # 6359
length(unique(simulated.data$lemma)) # 6416
length(unique(simulated.data$en)) # 6287

dim(simulated.data) # 8145
dim(simulated.data[na.omit(simulated.data$main_term),]) # 8145

simulated.data$genitiveVN <- simulated.data$genitiveVN_plain
simulated.data$en <- simulated.data$main_term
simulated.data <- subset(simulated.data, select = -c(genitiveVN_plain, main_term))

saveRDS(object = simulated.data, file = 'user_data/simdata.rds')

