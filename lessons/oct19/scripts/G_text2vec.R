#' Title: text2vec example
#' Purpose: Perform text2vec to find abstractive relationships
#' Author: Ted Kwartler
#' email: edward.kwartler@hult.edu
#' License: GPL>=3
#' Date: Oct 18, 2021
#'
#'

# WD
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct19/data")

# Libs
library(data.table)
library(tm)
library(pbapply)
library(text2vec)

# Code Testing
testing <- F

# Bring in our supporting functions
source('~/Documents/GitHub/LUX_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')
airbnb <- readr::read_csv('Airbnb-boston_only.zip') #GG: loading smaller dataset (only Boston), unzips the file as it opens it
names(airbnb) #GG: data from Airbnb reviews

# Make appropriate for tm
airbnbTxt <- data.table(doc_id = airbnb$listing_id, #GG: keeping only vars that we need, and renaming them. can retrieve further variables later if needed
                        text = airbnb$comments,
                        reviewScores = airbnb$review_scores_rating)
rm(airbnb) #GG: detaching full file
gc()

# Apply standard cleaning #GG: These steps take a while so I've saved the rds and commented out some linew
#txt <- VCorpus(DataframeSource(airbnbTxt))
#txt <- cleanCorpus(txt, c(stopwords('en'), 'boston'))
#saveRDS(txt,'txt.rds')
txt <- readRDS('txt.rds')

# Get the cleaned text out
txtTokens <- unlist(pblapply(txt, content))

# Create the vocabulary iterator (string splitting)
if(testing == T){
  n <- 10
} else {
  n <- length(txtTokens)
}
iter <- itoken(txtTokens[1:n],
            tokenizer = word_tokenizer, n_chunks = 10) #GG: perform iterations of tokenizer in chunks
nGramVocab <- create_vocabulary(iter, c(1, 3) ) #number of n-grams that we want: unigram, bigram, trigram: will be huge sparse matrix

# Examine
head(nGramVocab, 8)

# Heuristic prune
nGramVocab <- prune_vocabulary(nGramVocab, term_count_min = 5) # GG: wanna chop off the tail if term count less then 5

# Create the custom function 
tokenVecs <- vocab_vectorizer(nGramVocab)

# Create a DTM if needed
tmDTM <-  create_dtm(iter, tokenVecs, type = 'dgTMatrix') #GG: declared as a document-term matrix.
class(tmDTM)
tmDTM <- as.DocumentTermMatrix(tmDTM, weighting = 'weightTf') #GG: converting object if we want to use -tm-
tmDTM
rm(tmDTM) #GG: removed because we're not gonna use it. instead we create tcm

# PPTX review skip gram method
# Create the term-cooccurence matrix; "TCM matrix usually used with GloVe word embedding model."
tcm <- create_tcm(iter, # tokenized & normalized stings
                  tokenVecs, # function: n-gram & specific word vec construction
                  skip_grams_window = 5, # token's context is defined by 5 words before and after
                  skip_grams_window_context = "symmetric") #GG: we want to keep terms that are shared across documents (overlap, common support)
?create_tcm
# Square since all retained tokens are shared between 1+ docs
# Each element the matrix represents how often word i appears in context of word j.
dim(tcm)

# Build the GloVe model
# instantiate the object #GG: dimension reduction: from 68898 to 50
fitGlove <- GlobalVectors$new(rank = 50,  # how many vectors to represent
                              x_max = 10) # max number of cooccurences; focus on 10 most frequent co-occurences

# Fit the model; pg 176 in the book
st <- Sys.time()
set.seed(123)
contextGlove <- fitGlove$fit_transform(tcm, 
                                   n_iter = 10, 
                                   convergence_tol = 0.01, #GG: learning rate
                                   n_threads = 8) # Parallelize, if you have 8-core chip

# According to the package author there are two vectors
gloveContext <-  fitGlove$components
dim(gloveContext) #GG: 50 vectors with all the 68898 tokens on top

# From package
# While both of word-vectors matrices can be used as result it usually better (idea from GloVe paper) to average or take a sum of main and context vector
wordVectors <- contextGlove + t(gloveContext) #GG: package writers recommend doing this in the Glova paper
head(wordVectors)

# Find the closest word vectors for good walks in boston airbnb stays #GG: what represents good walk? took all token "walk"
goodWalks <- wordVectors["walk", , drop = FALSE] - 
  wordVectors["disappointed", , drop = FALSE] + #GG: wanna focus on a contextualization withouth disappoinment and with good
  wordVectors["good", , drop = FALSE]
goodWalks

cosSimilarity <- sim2(x = wordVectors, #GG: now measuring the distance (through cosine similarity) between the "goodwalks" vector and the "wordVectors" huge sparse matrix
                      y = goodWalks, 
                      method = "cosine")
cosSimilarity <- data.frame(term = rownames(cosSimilarity),
                            coSineSim = cosSimilarity[,1], 
                            row.names = NULL)

# get only the top 20 contextualized terms 
cosSimilarity <- cosSimilarity[order(cosSimilarity$coSineSim, decreasing = T),]
head(cosSimilarity, 20) #GG: this is the context by which people in their reviews are saying they had a good walk

# Find the closest word vectors for dirty sinks in boston airbnb stays #GG: let's do the same with dirty stays
dirtyStay <- wordVectors["dirty", , drop = FALSE] - 
  wordVectors["condition", , drop = FALSE] -
  wordVectors["clean", , drop = FALSE] 
dirtyStay

cosSimilarity <- sim2(x = wordVectors, 
                      y = dirtyStay, 
                      method = "cosine")
cosSimilarity <- data.frame(term = rownames(cosSimilarity),
                            coSineSim = cosSimilarity[,1], 
                            row.names = NULL)

# get only the top 20 contextualized terms 
cosSimilarity <- cosSimilarity[order(cosSimilarity$coSineSim, decreasing = T),]
head(cosSimilarity, 20)

# End