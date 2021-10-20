#' Title: Topic Modeling 
#' Purpose: Unsupervised LDA model building
#' Author: Ted Kwartler
#' email: edwardkwartler@fas.harvard.edu
#' License: GPL>=3
#' Date: Oct 17, 2021
#'
#'FOR REALLY DECENT EXPLANATION w/more math http://i.amcat.nl/lda/understanding_alpha.html

# Wd
setwd("~/Documents/GitHub/LUX_NLP_student/lessons/oct18/data")

# Install Issue on cloud...sometimes
# install.packages("httpuv", dependencies = TRUE, INSTALL_opts = '--no-lock')

# Libs
library(tm)
library(qdap)
library(pbapply)
library(lda)
library(LDAvis)
library(dplyr)
library(treemap)

# Bring in our supporting functions
source('~/Documents/GitHub/LUX_NLP_student/lessons/Z_otherScripts/ZZZ_supportingFunctions.R')

# In some cases, blank documents and words are created bc of preprocessing.  This will remove them.
blankRemoval<-function(x){
  x <- unlist(strsplit(x,' '))
  x <- subset(x,nchar(x)>0) #GG: removing documents with length=0 coz otherwise it throws an error as there are no words to look at the distribution of
  x <- paste(x,collapse=' ')
} #GG: don't apply too many stopwords (aggressive pre-processing) when documents are short. You may end up with empty documents

# Each term is assigned to a topic, so this will tally for a document & assign the most frequent as membership
docAssignment<-function(x){
  x <- table(x)
  x <- as.matrix(x)
  x <- t(x)
  maxCol <-max.col(x) #GG: we are going to have K probabilities. I want to detect the topic with the highest probability so that I can assign a document to the cluster with the highest probability
  response <- colnames(x)[maxCol]
  return(response)
}

# Options & Functions
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL','C')

# Stopwords
stops <- c(stopwords('SMART'), 'pakistan', 'gmt', 'pm') #GG: pakistan was the search term

# Data articles from ~2016-04-04
text <- readRDS("Guardian_text.rds")
text$body[1]

# String clean up #GG: some cleaning up here is needed, removing URLs etc.
text$body <- iconv(text$body, "latin1", "ASCII", sub="")
text$body <- gsub('http\\S+\\s*', '', text$body ) #rm URLs; qdap has rm_url same outcome. 
text$body <- bracketX(text$body , bracket="all") #rm strings in between parenteses, and other brackets
text$body <- replace_abbreviation(text$body ) # replaces a.m. to AM etc
text$body[1] #GG: this is clean now

# Instead of DTM/TDM, just clean the vector w/old functions #GG: the reason for that is that the LDA package does not work with that data structure...
txt <- VCorpus(VectorSource(text$body))
txt <- cleanCorpus(txt, stops)

# Extract the clean text
txt <- unlist(pblapply(txt, content)) #GG: ...instead, after cleaning we extract out the content using unlist and pblapply
#GG: so we get a vector of 29 articles that are now cleaned text
# Remove any blanks, happens sometimes w/tweets bc small length & stopwords
txt <- pblapply(txt, blankRemoval) #GG: also removing blank documents (see above)

# Lexicalize
txtLex <- lexicalize(txt)
#GG: here lies the differnce between LDA and tm. Instead of a TDM or DTM you have this function lexicalize().
#GG: essentially, a vocabulary building exercise that you apply it to your vector of text.
# Examine the vocab or key and value pairing between key ()
head(txtLex$vocab) # remember #6 #GG: vocab is a lookup table. See how it's cchopping off stopwords and considering the ones left as part of a vocabulary
length(txtLex$vocab) #8k+ unique words among all articles, each 
txtLex$documents[[1]][,1:25] #look at 21, 22 etc #GG: 6th word has index 5 (starts from 0) and is "test". the same term occurs also as 10th and 21st word
txtLex$documents[[20]][,1:25] #GG: now first 25 words in document n.20

# Corpus stats
txtWordCount  <- word.counts(txtLex$documents, txtLex$vocab)
txtDocLength  <- document.lengths(txtLex$documents)

# LDA Topic Modeling
# suppose you have a bag of dice (documents)
# alpha - there is a distribution of the probabilities of how similar they are to each other, are dice similar in size/shape/weight?
# eta   - there is also a distribution of probabilities for the number of topics inside a single document, are dice 6 sided or other?
# 
k       <- 5 # number of topics
numIter <- 25 # number of reviews, it performs random word sampling each time #GG: number of times we resample and look for these distinct word distributions
alpha   <- 0.02 #see above #GG: shape of distribution
eta     <- 0.02 #see above
set.seed(1234) 
fit <- lda.collapsed.gibbs.sampler(documents      = txtLex$documents, #GG: pass in your documents, and the rest
                                   K              = k, 
                                   vocab          = txtLex$vocab, 
                                   num.iterations = numIter, 
                                   alpha          = alpha, 
                                   eta            = eta, 
                                   initial        = NULL, 
                                   burnin         = 0,
                                   compute.log.likelihood = TRUE)

# Prototypical Document
top.topic.documents(fit$document_sums,2) #top 2 docs (rows) * topics(cols)
#GG: docs n. 28 and 29 are the most indicative of cluster topic 1; n. 3 and 2 of 2nd cluster and so on. Important to inspect prototypical documents
# explore some of the results
fit$document_sums #topics by articles #GG: In document n.1, 40 words were attributed to cluster 1, 156 to cluster 2, etc. Similarly, for the distribution of topic 1, 40 words were found in document 1. While for the distribution of topic 2, 156 words were found in article 1
head(t(fit$topics)) #words by topics #GG: frequency (alignment) of words in (to) each topic. A word can belong to more than one topic

# LDAvis params
# normalize the article probabilities to each topic #GG: now normalizing frequencies wrt document lengths
theta <- t(pbapply(fit$document_sums + alpha, 2, function(x) x/sum(x))) # topic probabilities within a doc will sum to 1

# normalize each topic word's impact to the topic #GG: normalizing wrt to topic
phi  <- t(pbapply(fit$topics + eta, 1, function(x) x/sum(x)))

ldaJSON <- createJSON(phi = phi,
                      theta = theta, 
                      doc.length = txtDocLength, 
                      vocab = txtLex$vocab, 
                      term.frequency = as.vector(txtWordCount))

serVis(ldaJSON) #GG: very cool visualization. allows to visualize clusters, words in them, and overlaps between them

# Topic Extraction
top.topic.words(fit$topics, 10, by.score=TRUE) #GG: browse 10 most distinct words by topic

# Name Topics
topFive <- top.topic.words(fit$topics, 5, by.score=TRUE) #GG: using top5 words to rename clusters
topFive <- apply(topFive,2,paste, collapse='_') #collapse each of the single topics word into a single "name"

# Topic fit for first 10 words of 2nd doc
fit$assignments[[2]][1:10] #GG: In 2nd article, word 1 was assigned to topic 3 (clusters indexed at 0)

# Tally the topic assignment for the first doc, which topic should we assign it to?
table(fit$assignments[[1]])

# What topic is article 1 assigned to? #GG: cluster 2 (indexed with 1)
singleArticle <- docAssignment(fit$assignments[[1]]) #GG: document-level assignment

# Get numeric assignments for all docs
topicAssignments <- unlist(pblapply(fit$assignments, #GG: docAssignment + pblapply to do it on all docs
                                    docAssignment)) #GG: then unlist it
topicAssignments #GG: has 29 elements corresponding to the assignments via maxcol (that's what docAssignment basically does)

# Recode to the top words for the topics; instead of topic "1", "2" use the top words identified earlier
length(topicAssignments)
assignments <- recode(topicAssignments, "0" = topFive[1], "1" = topFive[2], #GG: going from pythonic to R indexing
                      "2" = topFive[3],"3" = topFive[4],"4" = topFive[5])

# Polarity calc to add to visual #GG: now we do polarity analysis on these clusters
txtPolarity <- polarity(txt)[[1]][3]
#saveRDS(txtPolarity, 'txtPolarity.rds')
txtPolarity <- readRDS('txtPolarity.rds')

# Final Organization
allTree <- data.frame(topic    = assignments, 
                      polarity = txtPolarity,
                      length   = txtDocLength)
head(allTree) #GG: tabulate cluster assignment, polarity and length of each document
#GG: let's visualize
set.seed(1237)
tmap <- treemap(allTree,
                index   = c("topic","length"),
                vSize   = "length",
                vColor  = "polarity",
                type    ="value", 
                title   = "Guardan Articles mentioning Pakistan",
                palette = c("red","white","green"))
tmap
#GG: tree map is multidimensional and hierarchical. Unboldedly contoured are articles with their length. They are encased within clusters. And the polarity is rendered with a color
# End