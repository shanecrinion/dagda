library(tidyr)
library(reticulate)

### ---- Corpus
# Frequency lists of Irish words based on the National Corpus of Irish.
# link: https://www.corpas.ie/en/extras/word-lists/
corpus <- read.delim(file = 'inst/extdata/cng-toklempos.tsv', head=F,
                    col.names=c('word_form', 'lemma', 'part_of_speech', 'frequency'),
                    stringsAsFactors = F)
corpus$rank <- row.names(corpus)
#saveRDS(corpus, file="inst/extdata/corpus.rds")

# load and save part-of-speech coding
corpus.pos <- read.delim(file = 'inst/extdata/pos.tsv',
                         head=F, col.names=c('pos', 'pos_ga', 'pos_en'),
                         stringsAsFactors = F)
#saveRDS(corpus.pos, file='inst/extdata/corpus.pos.rds')

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
#saveRDS(anki, file="inst/extdata/anki.rds")

#ankicleanHTML <- read.csv('inst/extdata/anki_cleanHTML.csv', stringsAsFactors = F)
#saveRDS(ankicleanHTML, file="inst/extdata/anki_cleanHTML.rds")

word_data <- list(
  terminology = tearma,
  frequency = corpus,
  anki = anki,
  frequency.guide = corpus.pos)

save(word_data, file='data/word_data.Rdata')

