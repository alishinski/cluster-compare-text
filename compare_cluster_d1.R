#-------------------------------------------------------
# 1. Setting working directory, loading packages, and loading files
#-------------------------------------------------------

# Setting working directory

setwd("~/Dropbox/Current Research/scip/Scientific Practices Josh/longitudinal/csv")

# Loading packages

library(dplyr)
library(tidyr)
library(SnowballC)
library(tm)
library(lsa)
library(cluster)
library(ggplot2)
library(wordcloud)

# Loading data frame

all <- read.csv("all_data.csv")

# Selecting columns of dataframe by question topic

aud <- select(all, audience1, grade, teacher, ID, time)
aud2 <- select(all, audience2, grade, teacher, ID, time)
gen <- select(all, generality, grade, teacher, ID, time)
evid <- select(all, evidence, grade, teacher, ID, time)
purp <- select(all, purpose, grade, teacher, ID, time)
crit <- select(all, criteria, grade, teacher, ID, time)

# Selecting data frame for analysis

str(aud)
doc_vec <- aud$audience1

#-------------------------------------------------------
# 2. Variables to modify
#-------------------------------------------------------

# Number of clusters

n_clusters <- 5

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

# Group factor

# group <- 

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

# Processing files

myCorpus <- Corpus(VectorSource(doc_vec))
myCorpus <- tm_map(myCorpus, content_transformer(tolower), mc.cores = 1)
myCorpus <- tm_map(myCorpus, removePunctuation, mc.cores = 1)
myCorpus <- tm_map(myCorpus, removeNumbers, mc.cores = 1)
myCorpus <- tm_map(myCorpus, removeWords, stopwords("english"), mc.cores = 1)
myCorpus <- tm_map(myCorpus, removeWords, custom_stopwords, mc.cores=1)
myCorpus_copy <- myCorpus
myCorpus <- tm_map(myCorpus, stemDocument, mc.cores = 1)
myCorpus <- tm_map(myCorpus, stemCompletion2, myCorpus_copy)
myCorpus <- tm_map(myCorpus, stripWhitespace, mc.cores = 1)
myCorpus <- tm_map(myCorpus, PlainTextDocument)

print(paste("Processed ", length(doc_vec), " documents", sep = ""))

#-------------------------------------------------------
# 4. For overall term document matrix
#-------------------------------------------------------

# Creating a term document matrix from the corpus

TDM <- TermDocumentMatrix(myCorpus, control = list(weighting = function(x) weightSMART(x, spec = "nnn")))

# Removing sparse terms

TDM_common <- removeSparseTerms(TDM, sparseness)

#-------------------------------------------------------
# 5. For group term document matrices
#-------------------------------------------------------

# Name documents ## Specific to scip data

for (i in 1:length(myCorpus)){
  meta(myCorpus[[i]], "teacher") <- aud$teacher[i]
  meta(myCorpus[[i]], "grade") <- aud$grade[i]
  meta(myCorpus[[i]], "ID") <- aud$ID[i]
  meta(myCorpus[[i]], "time") <- aud$time[i]
}

# Filter term document matrices by group

# By teacher

index_j <- tm_index(myCorpus, function(x) meta(x, "teacher") == "j")
index_paul <- tm_index(myCorpus, function(x) meta(x, "teacher") == "paul")
index_cathy <- tm_index(myCorpus, function(x) meta(x, "teacher") == "cathy")
index_julie <- tm_index(myCorpus, function(x) meta(x, "teacher") == "julie")

# By time

index_1 <- tm_index(myCorpus, function(x) meta(x, "time") == 1)
index_2 <- tm_index(myCorpus, function(x) meta(x, "time") == 2)
index_3 <- tm_index(myCorpus, function(x) meta(x, "time") == 3)
index_4 <- tm_index(myCorpus, function(x) meta(x, "time") == 4)
index_5 <- tm_index(myCorpus, function(x) meta(x, "time") == 5)
index_6 <- tm_index(myCorpus, function(x) meta(x, "time") == 6)

# By grade

index_grade5 <- tm_index(myCorpus, function(x) meta(x, "grade") == 5)
index_grade6 <- tm_index(myCorpus, function(x) meta(x, "grade") == 6)

# By id # Need to fix

index_MC <- tm_index(myCorpus, function(x) meta(x, "ID") == 301120) 
index_AD <- tm_index(myCorpus, function(x) meta(x, "ID") == 301181)
index_DN <- tm_index(myCorpus, function(x) meta(x, "ID") == 301134)
index_JS <- tm_index(myCorpus, function(x) meta(x, "ID") == 301142)
index_KH <- tm_index(myCorpus, function(x) meta(x, "ID") == 301212)

# Saving documents based on groups to a list



# # Creating a list of term document document matrices for each group of corpora 
# 
# TDM <- list()
# 
# for (i in seq(length(doc_vec))){
#   TDM[[i]] <- TermDocumentMatrix(corpora_list[[i]], control = list(weighting = weightTfIdf, normalize = TRUE))
# }
# 
# # Concatenating the term document matrices
# 
# TDM_all <- myTDM[[1]]
# 
# for (i in 2:length(doc_vec)){
#   TDM_all <- c(TDM_all, TDM[[i]])
# }
# 
# # Removing sparse terms
# 
# TDM_common <- removeSparseTerms(TDM_all, sparseness)
# 
# # Creating a list of term document document matrices for each group of corpora 
# 
# TDM <- TermDocumentMatrix(myCorpus, control = list(weighting = function(x) weightSMART(x, spec = "nnn")))

#-------------------------------------------------------
# 6. Preparing data for clustering
#-------------------------------------------------------

# Removing outliers

# Creates booleans for outliers

doc_len_M <- mean(sort(colSums(as.matrix(TDM_common))))
doc_len_SD <- sd(sort(colSums(as.matrix(TDM_common))))
plusCI <- doc_len_M + 1.65*(doc_len_SD) # need to change this 
minusCI <- doc_len_M - 1.65*(doc_len_SD) # need to change this 

# Removes outliers

plusCI_bool <- colSums(as.matrix(TDM_common)) > plusCI
minusCI_bool <- colSums(as.matrix(TDM_common)) < minusCI
adjminusCI_bool <- colSums(as.matrix(TDM_common)) < 2 # need to change this 

TDM_common <- TDM_common[, !plusCI_bool]
TDM_common <- TDM_common[, !adjminusCI_bool]
TDM_common <- TDM_common[, !minusCI_bool]

# Creates initial matrix which needs to have deviation vectors calculated

mat <- t(as.matrix(TDM_common))

# Weights by deviation vectors

adj_mat <- matrix(nrow = nrow(mat), ncol = ncol(mat))
for (i in seq(nrow(mat))){
  adj_mat[i, ] <- mat[i, ] - freq_terms[[1]] / nrow(mat)
  adj_mat[i, ] <- adj_mat[i, ] / sum(mat[i, ]) # weights words by proportion in the document - need to change
}

!~~~~~!!!!! CLUSTERING!!!
  
  # Fits kmeans algorithm
  
  #kfit <- kmeans(adj_mat, centers = n_clusters, nstart = 25, iter.max = 1000)
  
  kfit <- kmeans(adj_mat, centers = n_clusters)

# Creating a list of weighted term document matrices for each cluster

clusters <- list()
unadj_clusters <- list()

for (i in seq(n_clusters)){
  clusters[[i]] <- TDM_common[, kfit$cluster == i]
  unadj_clusters[[i]] <- rowSums(as.matrix(clusters[[i]])) # not sure I need this
  clusters[[i]] <- rowSums(as.matrix(clusters[[i]])) / table(kfit$cluster)[i]
}







~~~ OUTPUT AND OUTPUT FOR LATER PROCESSING

# Finds the frequency of terms for each term document matrix ## Need to fix

freq_terms <- list()
for (i in seq(length(TDM))){
  freq_terms[[i]] <- rowSums(as.matrix(TDM[[i]])) # Need to output this
}

# Creating an ordered list of clusters

ordered_clusters <- list()
clusters_df <- matrix(nrow = 10, ncol = n_clusters)

for (i in seq(length(clusters))){
  ordered_clusters[[i]] <- sort(clusters[[i]], decreasing = TRUE)
  clusters_df[, i] <- names(ordered_clusters[[i]][1:10])
}

clusters_df <- as.data.frame(clusters_df)
colnames(clusters_df) <- paste("Cluster ", c(seq(n_clusters)), sep="")

### Creating an adjusted (by relevance) ordered list of clusters

my_list <- list()
adjclusters_df <- matrix(nrow = 10, ncol = n_clusters)

for (i in seq(length(clusters))){
  
  my_list[[i]] <- clusters[[i]] - freq_terms[[1]] / nrow(mat)
  my_list[[i]] <- sort(my_list[[i]], decreasing = TRUE)
  adjclusters_df[, i] <- names(my_list[[i]][1:10])
  
}

adjclusters_df <- as.data.frame(adjclusters_df)
colnames(adjclusters_df) <- paste("Cluster ", c(seq(n_clusters)), sep="")


~~~~!!!! PREPARING INDIVIDUAL TERM DOCUMENT MATRICES FOR COMPARISONS!!! AND FREQUENCIES!!!
  
  # Creating vectors "terms" for each item in entered list 
  
  freq_terms <- list()
for (i in seq(length(TDM))){
  freq_terms[[i]] <- rowSums(as.matrix(TDM[[i]])) # Need to output this
}

inspect(TDM[1:10, 1:10])

meta(myCorpus[[1]])
meta(TDM[[,1]])

?Corpus

group_terms <- list()

for (i in seq(length(TDM))){
  
  group_terms[[i]] <- vector(length = dim(TDM_common)[1])
  names(group_terms[[i]]) <- TDM_common[["dimnames"]][["Terms"]]
  
  for (j in seq(dim(TDM_common)[1])){
    
    if (names(group_terms[[i]][j]) %in% names(freq_terms[[1]])){
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

# For wordclouds 

wc <- ordered_clusters

#-------------------------------------------------------
# 6. Output
#-------------------------------------------------------

# Terms in term document matrix 

print(TDM)

# Terms in term document matrix with sparse terms removed

print(TDM_common)

# Number of documents in each cluster

print(table(kfit$cluster))

# 10 most frequently included terms in each cluster and adjusted terms

print(clusters_df)
print(adjclusters_df)

# Wordclouds ## need to fix

# wordclouds <- list()

# for (i in 1:length(clusters)){
#   wordclouds[[i]] <- wordcloud(names(wc[[i]][1:40]), wc[[i]][1:40]) # Still need to print wordclouds
# }

# Scaled plot

# cosines_scaled <- as.data.frame(cosines_scaled)
# y <- cosines_scaled[1:length(doc_vec), ]
# x <- gather(y, cluster, cosines)
# x$observation <- rep(1:length(doc_vec), length(cosines))
# ggplot(data = x, aes(x = observation, y = cosines, fill = cluster)) +
# geom_bar(position = "dodge", stat = "identity", width = .75)

# Plot

cosines <- as.data.frame(cosines)
y <- cosines[1:length(doc_vec), ]
x <- gather(y, cluster, cosines)
x$observation <- rep(1:length(doc_vec), length(cosines))
ggplot(data = x, aes(x = observation, y = cosines, fill = cluster)) +
  geom_bar(position = "dodge", stat = "identity", width = .75)

# findAssocs(myTDM_common, )

?cutree
