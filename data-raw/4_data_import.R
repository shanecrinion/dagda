library(tidyr)
library(reticulate)

### ---- Corpus
# Frequency lists of Irish words based on the National Corpus of Irish.
# link: https://www.corpas.ie/en/extras/word-lists/
corpus <- read.delim(file = 'inst/extdata/cng-toklempos.tsv', head=F,
                     col.names=c('word_form', 'lemma', 'part_of_speech', 'frequency'),
                     stringsAsFactors = F)

# load and save part-of-speech coding
corpus.pos <- read.delim(file = 'inst/extdata/pos.tsv',
                         head=F, col.names=c('pos', 'pos_ga', 'pos_en'),
                         stringsAsFactors = F)

#saveRDS(corpus.pos, file='inst/extdata/corpus.pos.rds')
pos_map <- setNames(corpus.pos$pos_en, corpus.pos$pos)

# set the corpus code
corpus$part_of_speech <- pos_map[as.character(corpus$part_of_speech)]
#saveRDS(corpus, file="inst/extdata/corpus.rds")

## --- Téarma
# téarma.ie terminology database
# link: https://www.tearma.ie/ioslodail/
# Parsed from xml to csv using python script python/parse_xml.py
tearma <- read.csv('inst/extdata/full_terminology_table.csv',stringsAsFactors = F)
# some addition parsing
tearma <- tearma %>% separate(subjectField, into = c('subjectField', 'subsetField'), sep=' » ')
#saveRDS(tearma, file="inst/extdata/tearma.rds") # more info for words that need work

## --- Anki - Irish words and short periphrases with ordering by frequency
# Link: https://ankiweb.net/shared/info/1128787897
# Parsed using python/parse_apkg.py
anki <- read.csv('inst/extdata/anki.csv') # contains html codes
anki$rank <-as.numeric(row.names(anki))
#saveRDS(anki, file="inst/extdata/anki.rds")

# clean the html codes from anki - keep originals too for shiny
# Plain-text extractor (robust to nested and malformed HTML)
strip_html <- function(x) {
  vapply(x, function(text) {
    xml_text(read_html(paste0("<body>", text, "</body>")))
  }, character(1))
}

# generate plain versions
anki$en_plain <- strip_html(anki$en)
anki$genitiveVN_plain <- strip_html(anki$genitiveVN)

# html parsing
extract_small_notes <- function(x) {
  sapply(x, function(text) {
    html <- read_html(paste0("<body>", text, "</body>"))
    smalls <- html_elements(html, "small")
    paste(html_text(smalls), collapse = "; ")
  })
}

# extract rough notes
anki$tooltips <- extract_small_notes(anki$en)

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

anki$main_term <- vapply(anki$en, extract_main_term, character(1))

word_data <- list(
  terminology = tearma,
  frequency = corpus,
  anki = anki,
  frequency.guide = corpus.pos)
#save(word_data, file='data/word_data.Rdata')
saveRDS(word_data, file='data/word_data.rds')


# making a html-clean version too
# ankicleanHTML <- read.csv('inst/extdata/anki_cleanHTML.csv', stringsAsFactors = F)
# ankicleanHTML$rank <- as.numeric(row.names(ankicleanHTML))

# Get test data ready for processing
test_data = merge(word_data$anki, word_data$frequency, by.x='ga', by.y='word_form')

#

# Extract the useful columns and order by rank
test_data <- test_data[order(as.integer(test_data$rank)),
                       c('ga','en', 'lemma', 'rank',
                         'pos','part_of_speech', 'gender','genitiveVN')]
save(test_data, file='data/test_data.Rdata')
load('data/test_data.Rdata')
saveRDS(test_data, file='data/test_data.rds')

# Get test data ready for processing
test_data.clean = merge(ankicleanHTML, word_data$frequency, by.x='ga', by.y='word_form')

# Extract the useful columns and order by rank
test_data.clean <- test_data.clean[order(as.integer(test_data.clean$rank)),
                                   c('ga','en', 'lemma', 'rank',
                                     'pos','part_of_speech', 'gender','genitiveVN')]
#save(test_data.clean, file='data/test_data_clean.Rdata')
#saveRDS(test_data.clean, 'data/test_data_clean.rds') # save RDS for shiny





# words that need work - mostly alternative spelling and none of the top 760 ranked terms
irish_terms_need_manual <- simulated.data[simulated.data$main_term=='',]

length(unique(irish_terms_need_manual$lemma)) # 562

# remove temporarity
simulated.data <- simulated.data[!simulated.data$main_term=='',]

length(unique(simulated.data$ga)) # 6359
length(unique(simulated.data$lemma)) # 6416
length(unique(simulated.data$en)) # 6287

dim(simulated.data) # 8751
dim(simulated.data[na.omit(simulated.data$main_term),]) # 8751

simulated.data$genitiveVN <- simulated.data$genitiveVN_plain
simulated.data$en <- simulated.data$main_term
simulated.data <- subset(simulated.data, select = -c(genitiveVN_plain, main_term))



## --- Corpas (word embeddings)
# link: https://www.corpas.ie/en/extras/word-embeddings/
# load python functionality for finding similar words based on meaning and/or morphology
similar = source('R/load_vectors.R')
