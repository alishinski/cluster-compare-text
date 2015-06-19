#-------------------------------------------------------
# 1. Setting working directory, loading packages, and loading files
#-------------------------------------------------------

# Clearing environment 

rm(list = ls(all = TRUE))

# Setting working directory

setwd("~/Dropbox/statistics/cluster-compare-text/")

# Loading packages - if packages are not already installed, call install.packages() first, e.g. for dplyr install.packages("dplyr")

library(dplyr)
library(tidyr)
library(SnowballC)
library(tm)
library(lsa)
library(cluster)
library(ggplot2)
library(wordcloud)

# Loading data frame

all <- read.csv("scip_data.csv")

# Selecting columns of dataframe by question topic

aud1 <- select(all, audience1, grade, teacher, ID, time)
aud2 <- select(all, audience2, grade, teacher, ID, time)
gen <- select(all, generality, grade, teacher, ID, time)
evid <- select(all, evidence, grade, teacher, ID, time)
purp <- select(all, purpose, grade, teacher, ID, time)
crit <- select(all, criteria, grade, teacher, ID, time)

# Selecting data frame for analysis

doc_vec <- evid$evidence

#-------------------------------------------------------
# 2. Variables to modify
#-------------------------------------------------------

# Number of clusters

n_clusters <- 5

# Stopwords

#stopwords <- TRUE

# Custom stopwords

custom_stopwords <- c("thatïûªs") # need to fix - not sure why apostrophes aren't being encoded properly - try to save data again?

# Sparseness of term document matrices (from 0.00 to 1.00)

sparseness <- .995 ## is there a way to improve this to make it more intuitive? currently, lower sparseness leads to more terms

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

# Creating a term document matrix from the corpus ## need to fix documents with no text (or earlier)
 
TDM <- TermDocumentMatrix(myCorpus, control = list(weighting = function(x) weightSMART(x, spec = "nnn")))

# Removing sparse terms

TDM_common <- removeSparseTerms(TDM, sparseness)

#-------------------------------------------------------
# 5. For group term document matrices
#-------------------------------------------------------

# Name documents ## Specific to scip data

for (i in 1:length(myCorpus)){
  meta(myCorpus[[i]], "teacher") <- aud1$teacher[i]
  meta(myCorpus[[i]], "grade") <- aud1$grade[i]
  meta(myCorpus[[i]], "ID") <- aud1$ID[i]
  meta(myCorpus[[i]], "time") <- aud1$time[i]
}

# Filter term document matrices by group ## need to fix - loop these!

# By teacher

index_T1 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 1)
index_T2 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 2)
index_T3 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 3)
index_T4 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 4)

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

# Saving documents based on groups to a list ## need to fix when looped

# Teachers
# doc_list <- list(index_T1, index_T2, index_T3, index_T4)

# # Time
doc_list <- list(index_1, index_2, index_3, index_4, index_5, index_6)

# Grade

# doc_list <- list (index_grade5, index_grade6)

# Students

# doc_list <- list(index_MC, index_AD, index_DN, index_JS, index_KH)

#-------------------------------------------------------
# 6. Preparing data for clustering
#-------------------------------------------------------

library(ppls)

dev_vector <- function(vect_list){
        norm_vects <- lapply(vect_list, normalize.vector)
        sum_vect <- sum(do.call(rbind, norm_vects))
        norm_sum <- normalize.vector(sum_vect)
        projects <- lapply(norm_vects, vect_project, norm_sum)
        difference <- mapply('-', norm_vects, projects)
        dev_vects <-  apply(difference, MARGIN = 2, FUN = normalize.vector)
        dev_vects
}

vect_project <- function(a,b){
        project <- crossprod(a,b) * b
        project
}

# Removing outliers

# Creates booleans for outliers

doc_len_M <- mean(sort(colSums(as.matrix(TDM_common))))
doc_len_SD <- sd(sort(colSums(as.matrix(TDM_common))))

plusCI <- doc_len_M + 1.65*(doc_len_SD) # need to change this 
minusCI <- doc_len_M - 1.65*(doc_len_SD) # need to change this 

# Removes outliers ## need to fix - not a valid method

plusCI_bool <- colSums(as.matrix(TDM_common)) > plusCI
minusCI_bool <- colSums(as.matrix(TDM_common)) < minusCI

print(sum(plusCI_bool))
print(sum(minusCI_bool))

adjminusCI_bool <- colSums(as.matrix(TDM_common)) <= 1 # need to fix - change this 
print(sum(adjminusCI_bool))

doc_outliers <- plusCI_bool + minusCI_bool + adjminusCI_bool
  
# Creates initial matrix which needs to have deviation vectors calculated

mat <- as.matrix(TDM_common) ## need to fix - move?

# Filters initial matrix by boolean vectors to remove document outliers

TDM_cleaned <- TDM_common[, !doc_outliers]

# mat <- mat[, !doc_outliers]

# Filters matrix by boolean vectors to remove term outliers resulting from removing document outliers

term_outliers <- rowSums(as.matrix(TDM_cleaned)) == 0

# mat <- mat[!term_outliers, ]

TDM_cleaned <- TDM_cleaned[!term_outliers, ]

# Finds freq terms for use in deviation vectors

freq_terms <- rowSums(as.matrix(TDM_cleaned))

# Weights by deviation vectors

mat <- as.matrix(TDM_cleaned)

for (i in seq(ncol(mat))){
  
  mat[, i] <- mat[, i] - freq_terms / nrow(mat)
  #mat[, i ] <- mat[, i] / colSums(as.matrix(TDM_cleaned))[i] # weights words by proportion in the document ## need to fix -- change
}

#----------------------------------
# 7. Clustering and post-processing of clusters
#-------------------------------------------------------
  
# Set seed for reproducibility

set.seed(06132015)

# Fits kmeans algorithm

mat_t <- t(mat)

kfit <- kmeans(mat_t, centers = n_clusters) # need to fix - find a way to stablize this - too random

# kfit <- kmeans(adj_mat, centers = n_clusters, nstart = 25, iter.max = 1000) ## need to fix this doesn't help

table(kfit$cluster)

# Creating a list of weighted term document matrices for each cluster

clusters <- list()

for (i in seq(n_clusters)){
  clusters[[i]] <- TDM_cleaned[, kfit$cluster == i]
}

# Creating an ordered list of clusters

ordered_clusters <- list()
cluster_freqs <- list()
clusters_df <- matrix(nrow = 10, ncol = n_clusters)

for (i in seq(length(clusters))){
  ordered_clusters[[i]] <- rowSums(as.matrix(clusters[[i]]) / ncol(clusters[[i]]))
  cluster_freqs[[i]] <- ordered_clusters[[i]]
  ordered_clusters[[i]] <- names(sort(ordered_clusters[[i]], decreasing = TRUE))
  clusters_df[, i] <- ordered_clusters[[i]][1:10]
}

clusters_df <- as.data.frame(clusters_df)
colnames(clusters_df) <- paste("Cluster ", c(seq(n_clusters)), sep="")

### Creating an adjusted (by relevance) ordered list of clusters ## need to fix - this is not working (I think because of freq_terms)

adjordered_clusters <- list()
adjclusters_df <- matrix(nrow = 10, ncol = n_clusters)

for (i in seq(length(clusters))){
  adjordered_clusters[[i]] <- rowSums(as.matrix(clusters[[i]]) / ncol(clusters[[i]]))
  adjordered_clusters[[i]] <- adjordered_clusters[[i]] - freq_terms / length(freq_terms)
  adjordered_clusters[[i]] <- names(sort(adjordered_clusters[[i]], decreasing = TRUE))
  adjclusters_df[, i] <- adjordered_clusters[[i]][1:10]
}

adjclusters_df <- as.data.frame(adjclusters_df)
colnames(adjclusters_df) <- paste("Cluster ", c(seq(n_clusters)), sep="")

#-------------------------------------------------------
# 8. Output for further processing and general output
#-------------------------------------------------------

# Filter out outliers from group booleans using outlier booleans

plusCI <- !plusCI_bool
minusCI <- !minusCI_bool
adjminusCI <- !adjminusCI_bool

doc_list_cleaned <- list()

for (i in seq(doc_list)){
  doc_list_cleaned[[i]] <- doc_list[[i]] & plusCI & minusCI & adjminusCI
}

# Creates groups from TDM_common using group booleans 

TDM_group <- list()

for (i in seq(doc_list)){
  TDM_group[[i]] <- TDM_cleaned[, doc_list_cleaned[[i]]]
}

# Calculates term frequencies for each group 

group_freqs <- list()

for (i in seq(doc_list)){
  group_freqs[[i]] <- rowSums(as.matrix(TDM_group[[i]])) / ncol(TDM_group[[i]]) # Need to fix - will want to add group freqs
}

#-------------------------------------------------------
# 9. Calculating similarities
#-------------------------------------------------------

str(group_freqs)
str(cluster_freqs)

# Computing similarities  

cosines <- list()
cosines_list <- list()

for (i in seq(length(TDM_group))){ ## change this
  
  cosines[[i]] <- vector()
  
  for (j in seq(length(clusters))){
    
    cosines[[i]] <- append(cosines[[i]], cosine(group_freqs[[i]], cluster_freqs[[j]]))
    
  }
  cosines_list[[i]] <- cosines[[i]]
}

# Creating data frame and scaled data frame

cosines_df <- as.data.frame(do.call(rbind, cosines_list))
cosines_df
# cosines_df
cosines_df_scaled <- as.data.frame(sapply(cosines_df, scale))
cosines_df_scaled

# # Creating a list of the most frequent terms in each cluster for word clouds
# 
# ordered_clusters <- list()
# for (i in seq(length(clusters))){
#   ordered_clusters[[i]] <- sort(clusters[[i]], decreasing = TRUE)
# }
# 
# # For wordclouds 
# 
# wc <- ordered_clusters

#-------------------------------------------------------
# 10. Output
#-------------------------------------------------------

# Terms in term document matrix 

# print(TDM)

# Terms in term document matrix with sparse terms removed

# print(TDM_common)

# Number of documents in each cluster

print(table(kfit$cluster))

# 10 most frequently included terms in each cluster and adjusted terms

print(clusters_df)
print(adjclusters_df)

# Cosines

print(cosines_df)

# Wordclouds ## need to fix

# wordclouds <- list()

# for (i in 1:length(clusters)){
#   wordclouds[[i]] <- wordcloud(names(wc[[i]][1:40]), wc[[i]][1:40]) # Still need to print wordclouds
# }

# Plot

cos_plot <- gather(cosines_df, cluster, cosines)
cos_plot$group <- rep(1:length(doc_list), length(cosines_df))
ggplot(data = cos_plot, aes(x = group, y = cosines, fill = cluster)) +
  geom_bar(position = "dodge", stat = "identity", width = .75)

<<<<<<< HEAD
# hello, world
=======
# Testing 
>>>>>>> josh

# Scaled plot

# cosines_scaled <- as.data.frame(cosines_scaled)
# y <- cosines_scaled[1:length(doc_vec), ]
# x <- gather(y, cluster, cosines)
# x$observation <- rep(1:length(doc_vec), length(cosines))
# ggplot(data = x, aes(x = observation, y = cosines, fill = cluster)) +
# geom_bar(position = "dodge", stat = "identity", width = .75)

print(adjclusters_df)
print(table(kfit$cluster))
print(TDM)
print(TDM_common)

cos_plot <- gather(cosines_df_scaled, cluster, cosines)
cos_plot$group <- rep(1:length(doc_list), length(cosines_df))
ggplot(data = cos_plot, aes(x = group, y = cosines, fill = cluster)) +
  geom_bar(position = "dodge", stat = "identity", width = .75)
# Plot
# 
# cos_plot <- gather(cosines_df, cluster, cosines)
# cos_plot$group <- rep(1:length(doc_list), length(cosines_df))
# ggplot(data = cos_plot, aes(x = group, y = cosines, fill = cluster)) +
#   geom_bar

# Interesting results

# audience2, 4 clusters or 5 clusters
# audience1, 5 clusters, .995 sparseness
# criteria, 4 clusters, .995 sparseness
# generality, 7 clusters
# purpose, 4 or 5 clusters
# need to fix evidence 