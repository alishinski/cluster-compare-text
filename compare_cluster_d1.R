#-------------------------------------------------------
# 1. Setting working directory, loading packages, and loading files
#-------------------------------------------------------

# Setting working directory

setwd("~/Dropbox/Current Research/scip/Scientific Practices Josh/longitudinal")

# Loading packages

library(dplyr)
library(tidyr)
library(SnowballC)
library(tm)
library(lsa)
library(cluster)
library(ggplot2)
library(wordcloud)

# Loading vector of documents (can also take a list of vectors so that list items can be used as groups)

doc_vec <- audience

#-------------------------------------------------------
# 2. Variables to modify
#-------------------------------------------------------

# Number of clusters

n_clusters <- 4

# Stopwords

#stopwords <- TRUE

# Custom stopwords

custom_stopwords <- c("lorum ipsum")

# Sparseness of term document matrices (from 0.00 to 1.00)

sparseness <- .999 ## is there a way to improve this to make it more intuitive? currently, lower sparseness leads to more terms

# Weighting of terms in term document matrices (weightTF, weightTfIdf, or weightBin)

# weighting <- 
  
# Normalization of terms in term document matrices
  
# normalization <- TRUE

# Stem document

# stem <- TRUE

# Distance metric

# distance <-

# Type of plot (usin scaled or un-scaled cosines)

# plot_type <- 

#-------------------------------------------------------
# 3. Creating corpora and term document matrices
#-------------------------------------------------------

# Creating a list of groups of corpora for each group of vectors

# Modified stemCompletion function, as stemCompletion was not working

stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

corpora_list <- list()

for (i in seq(doc_vec)){
  
  myCorpus <- Corpus(DataframeSource(doc_vec[[i]]))
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  myCorpus <- tm_map(myCorpus, removePunctuation, mc.cores = 1)
  myCorpus <- tm_map(myCorpus, removeNumbers, mc.cores = 1)
  myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"), mc.cores = 1)
  myCorpus <- tm_map(myCorpus, removeWords, custom_stopwords, mc.cores=1)
  myCorpus_copy <- myCorpus
  myCorpus <- tm_map(myCorpus, stemDocument, mc.cores = 1)
  myCorpus <- tm_map(myCorpus, stemCompletion2, myCorpus_copy)
  myCorpus <- tm_map(myCorpus, stripWhitespace, mc.cores = 1)
  myCorpus <- tm_map(myCorpus, PlainTextDocument)
  corpora_list[[i]] <- myCorpus
  print(paste("Document group ", "#", i, "processed"))
}

# Creating a list of term document document matrices for each group of corpora 

TDM <- list()

for (i in seq(length(doc_vec))){
  TDM[[i]] <- TermDocumentMatrix(corpora_list[[i]], control = list(weighting = weightTfIdf, normalize = TRUE))
}

# Concatenating the term document matrices

TDM_all <- myTDM[[1]]

for (i in 2:length(doc_vec)){
  TDM_all <- c(TDM_all, TDM[[i]])
}

# Removing sparse terms

TDM_common <- removeSparseTerms(TDM_all, sparseness)

#-------------------------------------------------------
# 4. Analyzing the term document matrices
#-------------------------------------------------------

# Finds the frequency of terms for each term document matrix

freq_terms <- list()

for (i in seq(length(TDM))){
  freq_terms[[i]] <- rowSums(as.matrix(TDM[[i]])) # Need to output this
}

# Playing

# x <- t(as.matrix(TDM_common))
# y <- kmeans(x, 3)

# Creating a distance matrix 

d <- dist(TDM_common, method = "euclidean")

# Calculating clusters from distance matrix

kfit <- kmeans(d, n_clusters)

# Creating a list of weighted term document matrices for each cluster 

clusters <- list()

for (i in seq(n_clusters)){
  clusters[[i]] <- TDM_common[, kfit$cluster == i]
  clusters[[i]] <- rowSums(as.matrix(clusters[[i]])) / table(kfit$cluster)[i]
}

# Creating an ordered list of clusters

ordered_clusters <- list()

df1 <- sort(clusters[[1]], decreasing = TRUE)
df1 <- names(df1[1:10])
clusters_df <- data.frame(df1)

ordered_clusters[[1]] <- sort(clusters[[1]], decreasing = TRUE)
for (i in 2:length(clusters)){
  ordered_clusters[[i]] <- sort(clusters[[i]], decreasing = TRUE)
  clusters_df <- cbind(clusters_df, names(ordered_clusters[[i]][1:10]))
}

colnames(clusters_df) <- c(seq(n_clusters))

# Creating vectors "terms" for each item in entered list 

group_terms <- list()

for (i in seq(length(TDM))){
  
  group_terms[[i]] <- vector(length = dim(TDM_common)[1])
  names(group_terms[[i]]) <- TDM_common[["dimnames"]][["Terms"]]
  
  for (j in seq(dim(TDM_common)[1])){
    
    if (names(group_terms[[i]][j]) %in% names(freq_terms[[i]])){
      group_terms[[i]][j] <- freq_terms[[i]][names(freq_terms[[i]]) == names(group_terms[[i]][j])]
      
    }
    else {
      group_terms[[i]][j] <- 0
      
    }
  }
}

#-------------------------------------------------------
# 5. Calculating similarities
#-------------------------------------------------------

# Computing similarities  

term_cosines_list <- list()
term_cosines <- list()

for (i in seq(length(TDM))){ ## change this
  
  term_cosines[[i]] <- vector()
  
  for (j in seq(length(clusters))){
    
    term_cosines[[i]] <- append(term_cosines[[i]], cosine(group_terms[[i]], clusters[[j]]))
    
  }
  
  term_cosines_list[[i]] <- term_cosines[[i]]
}

# Creating data frame and scaled data frame

cosines <- as.data.frame(do.call(rbind, term_cosines_list))
cosines_scaled <- sapply(cosines, scale)

# Creating a list of the most frequent terms in each cluster for word clouds

ordered_clusters <- list()
for (i in seq(length(clusters))){
  ordered_clusters[[i]] <- sort(clusters[[i]], decreasing = TRUE)
}

wc <- ordered_clusters

#-------------------------------------------------------
# 6. Output
#-------------------------------------------------------

# Terms in term document matrix 

print(TDM_all)

# Terms in term document matrix with sparse terms removed

print(TDM_common)

# Number of documents in each cluster

print(table(kfit$cluster))

# 10 most frequently included terms in each cluster

print(clusters_df)

# Wordclouds ## need to fix

# wordclouds <- list()

# for (i in 1:length(clusters)){
#   wordclouds[[i]] <- wordcloud(names(wc[[i]][1:40]), wc[[i]][1:40]) # Still need to print wordclouds
# }

# Scaled plot

cosines_scaled <- as.data.frame(cosines_scaled)
y <- cosines_scaled[1:length(doc_vec), ]
x <- gather(y, cluster, cosines)
x$observation <- rep(1:length(doc_vec), length(cosines))
# ggplot(data = x, aes(x = observation, y = cosines, fill = cluster)) +
# geom_bar(position = "dodge", stat = "identity", width = .75)

# Plot

cosines <- as.data.frame(cosines)
y <- cosines[1:length(doc_vec), ]
x <- gather(y, cluster, cosines)
x$observation <- rep(1:length(doc_vec), length(cosines))
ggplot(data = x, aes(x = observation, y = cosines, fill = cluster)) +
geom_bar(position = "dodge", stat = "identity", width = .75)
