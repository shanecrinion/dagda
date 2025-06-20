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
saveRDS(corpus, file="inst/extdata/corpus.rds")

## --- Téarma
# téarma.ie terminology database
# link: https://www.tearma.ie/ioslodail/
# Parsed from xml to csv using python script python/parse_xml.py
tearma <- read.csv('inst/extdata/full_terminology_table.csv',stringsAsFactors = F)
# some addition parsing
tearma <- tearma %>% separate(subjectField, into = c('subjectField', 'subsetField'), sep=' » ')
#saveRDS(tearma, file="inst/extdata/tearma.rds")

## --- Anki - Irish words and short periphrases with ordering by frequency
# Link: https://ankiweb.net/shared/info/1128787897
# Parsed using python/parse_apkg.py
anki <- read.csv('inst/extdata/anki.csv') # contains html codes
anki$rank <-as.numeric(row.names(anki))
#saveRDS(anki, file="inst/extdata/anki.rds")

word_data <- list(
  terminology = tearma,
  frequency = corpus,
  anki = anki,
  frequency.guide = corpus.pos)
#save(word_data, file='data/word_data.Rdata')
saveRDS(word_data, file='data/word_data.rds')

# making a html-clean version too
ankicleanHTML <- read.csv('inst/extdata/anki_cleanHTML.csv', stringsAsFactors = F)
ankicleanHTML$rank <- as.numeric(row.names(ankicleanHTML))

# Get test data ready for processing
test_data = merge(word_data$anki, word_data$frequency, by.x='ga', by.y='word_form')

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

## --- Corpas (word embeddings)
# link: https://www.corpas.ie/en/extras/word-embeddings/
# load python functionality for finding similar words based on meaning and/or morphology
similar = source('R/load_vectors.R')
