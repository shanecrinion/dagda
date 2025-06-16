# library(devtools)
# library(purrr)
# library(stringr)
# library(tidyverse)
library(tidyr)
library(reticulate)
### ---- Corpus
# Frequency lists of Irish words based on the National Corpus of Irish.
# link: https://www.corpas.ie/en/extras/word-lists/
corpus <- read.delim(file = 'inst/extdata/cng-toklempos.tsv', head=F,
                    col.names=c('word_form', 'lemma', 'part_of_speech', 'frequency'))
corpus$rank <- row.names(corpus)
saveRDS(corpus, "inst/extdata/corpus.rda")

# load and save part-of-speech coding
corpus.pos <- read.delim(file = 'inst/extdata/pos.tsv', head=F, col.names=c('pos', 'pos_ga', 'pos_en'))
saveRDS(corpus.pos, 'inst/extdata/corpus.pos.rda')

## --- Téarma
# téarma.ie terminology database
# link: https://www.tearma.ie/ioslodail/
# Parsed from xml to csv using python script R/parse_xml.py
tearma <- read.csv('full_terminology_table.csv')
# some addition parsing
tearma <- tearma %>% separate(subjectField, into = c('subjectField', 'subsetField'), sep=' » ')
saveRDS(tearma, "inst/extdata/tearma.rda")

## --- Anki - Irish words and short periphrases with ordering by frequency
# Link: https://ankiweb.net/shared/info/1128787897
# Parsed using R/parse_apkg.py
anki <- read.csv('anki_cleanHTML.csv')
saveRDS(anki, "inst/extdata/anki_cleanHTML.rda")
anki <- read.csv('anki.csv') # contains html codes
saveRDS(anki, "inst/extdata/anki.rda")

## --- Corpas (word embeddings)
# link: https://www.corpas.ie/en/extras/word-embeddings/
# load python functionality for finding similar words based on meaning and/or morphology
gensim <- import("gensim")
# Load the model in native format
wv.meaning <- gensim$models$KeyedVectors$load("inst/extdata/cng-meaning.kv")
saveRDS(wv.meaning, 'inst/extdata/wv.meaning.rda')
wv.morphology <- gensim$models$KeyedVectors$load("inst/extdata/cng-morphology.kv")
saveRDS(wv.morphology, 'inst/extdata/wv.morphology.rda')

# --- Wish list
# Report card - you got x percent, you need to work on y
# Resource for fadas
# Events
# Grammar Terminology
# chat bot, tell me everything you know about x word
# pick your activities for the day - give vocab for those
# irish genetics facts
# irish history facts
# resources
# license and acknowledgments
# #dbWriteTable(con, "words", combined) - for faster running make an SQL database

