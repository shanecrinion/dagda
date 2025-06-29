library(xml2)
library(rvest)

### generate some dates for last seen,
set.seed(123)  # for reproducibility

# read in user history as a slight template
user_data <- readRDS(system.file("user_data/word_scores_cc.rds", 'dagda')) # read in user history

# simulate user improving with time
# simulate user improving with time
simulated.data <- user_data
simulated.data$seen_count <- pmax(round(rnorm(n = nrow(simulated.data), mean = 25, sd = 5)), 1)  # at least 1
simulated.data$correct_count <- pmax(round(rnorm(n = nrow(simulated.data), mean = 10, sd = 3)), 0)  # at least 0

simulated.data$excluded <- as.logical(rbinom(nrow(simulated.data), size = 1, prob = 0.1))

# follow up calculations
simulated.data$accuracy <- simulated.data$correct_count / simulated.data$seen_count
# Define the range
start_date <- as.POSIXct("2025-05-01 00:00:00", tz = "IST")
end_date <- as.POSIXct("2025-06-28 23:59:59", tz = "IST")

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

irish_terms_need_manual <- simulated.data[simulated.data$main_term=='',] # 0

simulated.data <- simulated.data[!simulated.data$main_term=='',]

length(unique(simulated.data$ga)) # 7325
length(unique(simulated.data$lemma)) # 6848
length(unique(simulated.data$main_term)) # 7010

saveRDS(object = simulated.data, file = 'user_data/simdata.rds')

