
load_vectors <- function(limit = 100000) {
  #gensim <- reticulate::import("gensim", delay_load = FALSE)

  vec_path_meaning <- here::here('inst/extdata', "cng-meaning.kv")
  vec_path_morph <- here::here('inst/extdata', "cng-morphology.kv")

  reticulate::py_run_string(sprintf("
import gensim
wv_meaning = gensim.models.KeyedVectors.load('%s')
wv_morphology = gensim.models.KeyedVectors.load('%s')
", vec_path_meaning, vec_path_morph))

  list(
    meaning = reticulate::py$wv_meaning,
    morphology = reticulate::py$wv_morphology
  )
}

# Get similar words dynamically
get_similar_words <- function(word, topn = 10, model = NULL) {
  stopifnot(!is.null(model))
  model$most_similar(word, topn = as.integer(topn))
}
