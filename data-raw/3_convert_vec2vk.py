
import gensim

# load the vectors for word2vec (similar in meaning) :
wv = gensim.models.KeyedVectors.load_word2vec_format('inst/extdata/cng-word2vec.vec', binary=False, limit=100000)

# Save to fast native format for efficient R Shiny usage
wv.save("inst/extdata/cng-meaning.kv")

# load the vectors for word2vec (similar in morphology):
wv = gensim.models.KeyedVectors.load_word2vec_format('inst/extdata/cng-fasttext.vec', binary=False, limit=100000)

wv.save("inst/extdata/cng-morphology.kv")
