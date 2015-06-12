#-------------------------------------------------------
# 1. Setting working directory, loading packages, and loading files
#-------------------------------------------------------

# Setting working directory

setwd()

# Loading packages

library(tidyr)
library(ggplot2)
library(wordcloud)
library(dplyr)
library(SnowballC)
library(tm)
library(lsa)
library(cluster)

# Loading vector of documents (can also take a list of vectors so that list items can be used as groups)

my_vec <- ""

#-------------------------------------------------------
# 2. Variables to modify
#-------------------------------------------------------

# Number of clusters

nclusters <- 4

# Stopwords

stopwords <- TRUE

# Custom stopwords

my_words <- c("lorum ipsum")

# Sparseness of term document matrices (from 0.00 to 1.00)

sparseness <- .99375

# Weighting of terms in term document matrices (weightTF, weightTfIdf, or weightBin)

weighting <- 
  
# Normalization of terms in term document matrices
  
normalization <- TRUE
  
# Stem document

stem <- TRUE

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

for (i in seq(my_vec)){
  
  myCorpus <- Corpus(DataframeSource(my_vec[[i]]))
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

myTDM <- list()
for (i in seq(length(my_vecs))){
  myTDM[[i]] <- TermDocumentMatrix(my_list[[i]], control = list(weighting = weightTfIdf, normalize = TRUE))
}

# Concatenating the term document matrices

myTDM_all <- myTDM[[1]]
for (i in 2:length(my_vecs)){
  myTDM_all <- c(myTDM_all, myTDM[[i]])
}

# Removing sparse terms

myTDM_common <- removeSparseTerms(myTDM_all, sparseness)

#-------------------------------------------------------
# 4. Analyzing the term document matrices
#-------------------------------------------------------

# Finds the frequency of terms for each term document matrix

my_freqs <- list()
for (i in seq(length(myTDM))){
  my_freqs[[i]] <- rowSums(as.matrix(myTDM[[i]]))
}

# Creating a distance matrix 

d <- dist(myTDM_common, method = "euclidean")

# Calculating clusters from distance matrix

kfit <- kmeans(d, nclusters)

# Creating a list of weighted term document matrices for each cluster 

my_clusters <- list()
my_clust <- list()
for (i in seq(nclusters)){
  my_clusters[[i]] <- myTDM_common[, kfit$cluster == i]
  my_clust[[i]] <- my_clusters[[i]]
  my_clusters[[i]] <- rowSums(as.matrix(my_clusters[[i]][,])) / table(kfit$cluster)[i]
}

# Creating an ordered list of clusters

my_clusters_ordered <- list()
df1 <- sort(my_clusters[[1]], decreasing = TRUE)
df1 <- names(df1[1:10])
my_clusters_df <- data.frame(df1)

for (i in 2:length(my_clusters)){
  my_clusters_ordered[[i]] <- sort(my_clusters[[i]], decreasing = TRUE)
  my_clusters_df <- cbind(my_clusters_df, names(my_clusters_ordered[[i]][1:10]))
}

colnames(my_clusters_df) <- c(seq(nclusters))

# Creating vectors "my terms" for each item in entered list 

my_terms <- list()

for (i in seq(length(myTDM))){
  
  my_terms[[i]] <- vector(length = dim(myTDM_common)[1])
  names(my_terms[[i]]) <- myTDM_common[["dimnames"]][["Terms"]]
  
  for (j in seq(dim(myTDM_common)[1])){
    if (names(my_terms[[i]][j]) %in% names(my_freqs[[i]])){
      my_terms[[i]][j] <- my_freqs[[i]][names(my_freqs[[i]]) == names(my_terms[[i]][j])]
      
    }
    else {
      my_terms[[i]][j] <- 0
      
    }
  }
}

#-------------------------------------------------------
# 5. Calculating similarities
#-------------------------------------------------------

# Computing similarities  

term_cosines <- list()
cosines_list <- list()

for (i in seq(length(myTDM))){
  
  term_cosines[[i]] <- vector()
  
  for (j in seq(length(my_clusters))){
    
    term_cosines[[i]] <- append(term_cosines[[i]], cosine(my_terms[[i]], my_clusters[[j]]))
    
  }
  cosines_list[[i]] <- term_cosines[[i]]
}

# Creating data frame and scaled data frame

my_cosines <- as.data.frame(do.call(rbind, cosines_list))
my_cosines_scaled <- sapply(my_cosines, scale)

# Creating a list of the most frequent terms in each cluster for word clouds

my_clusters_ordered <- list()
for (i in seq(length(my_clusters))){
  my_clusters_ordered [[i]] <- sort(my_clusters[[i]], decreasing = TRUE)
}

wc <- my_clusters_ordered

#-------------------------------------------------------
# 6. Output
#-------------------------------------------------------

# Terms in term document matrix 

print(myTDM_all)

# Terms in term document matrix with sparse terms removed

print(myTDM_common)

# Number of documents in each cluster

print(table(kfit$cluster))

# 10 most frequently included terms in each cluster

print(my_clusters_df)

# Wordclouds

wordclouds <- list()
for (i in 1:length(nclusters)){
  wordclouds[[i]] <- wordcloud(names(wc[[i]][1:40]), wc[[i]][1:40]) # Still need to print wordclouds
}

# Scaled plot

my_cosines_scaled <- as.data.frame(my_cosines_scaled)
y <- my_cosines_scaled[1:length(temp_df), ]
x <- gather(y, cluster, cosines)
x$observation <- rep(1:length(temp_df), length(my_cosines_scaled))
scaled_plot <- ggplot(data = x, aes(x = observation, y = cosines, fill = cluster)) +
  geom_bar(position = "dodge", stat = "identity", width = .75)

# Plot

my_cosines <- as.data.frame(my_cosines)
y <- my_cosines[1:length(temp_df), ]
x <- gather(y, cluster, cosines)
x$observation <- rep(1:length(temp_df), length(my_cosines))
ggplot(data = x, aes(x = observation, y = cosines, fill = cluster)) +
  geom_bar(position = "dodge", stat = "identity", width = .75)
