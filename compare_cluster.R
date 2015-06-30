#-------------------------------------------------------
# 1. Setting working directory, loading packages, and loading files
#-------------------------------------------------------

# Clearing environment 

rm(list = ls(all = TRUE))

# Setting working directory

setwd("~/Dropbox/statistics/cluster-compare-text/")

# Loading packages - if packages are not already installed, call install.packages() first, e.g. for dplyr install.packages("dplyr")

library(ppls)
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

#data$async <- iconv(data$async, "macintosh", "UTF-8")

# all <- lapply(all, function(x) iconv(x, "macintosh", "UTF-8"))

# Selecting columns of dataframe by question topic

aud1 <- select(all, audience1, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)
aud2 <- select(all, audience2, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)
gen <- select(all, generality, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)
evid <- select(all, evidence, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)
purp <- select(all, purpose, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)
crit <- select(all, criteria, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)

# Selecting data frame for analysis
# 
# doc_vec <- aud1$audience1
# doc_vec <- aud2$audience2
# doc_vec <- gen$generality
# doc_vec <- evid$evidence
# doc_vec <- purp$purpose
doc_vec <- crit$criteria

#-------------------------------------------------------
# 2. Variables to modify
#-------------------------------------------------------

# Number of clusters

n_clusters <- 4

# Stopwords

#stopwords <- TRUE

# Custom stopwords

custom_stopwords <- c("lorum ipsum") # need to fix - not sure why apostrophes aren't being encoded properly - try to save data again?

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
  
  meta(myCorpus[[i]], "T1") <- aud1$T1[i]
  meta(myCorpus[[i]], "T2") <- aud1$T2[i]
  meta(myCorpus[[i]], "T3") <- aud1$T3[i]
  meta(myCorpus[[i]], "T4") <- aud1$T4[i]
  
  meta(myCorpus[[i]], "S1") <- aud1$S1[i]
  meta(myCorpus[[i]], "S2") <- aud1$S2[i]
  meta(myCorpus[[i]], "S3") <- aud1$S3[i]
  meta(myCorpus[[i]], "S4") <- aud1$S4[i]
  meta(myCorpus[[i]], "S5") <- aud1$S5[i]
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

# By teacher by time ## Need to fix

# T1

T1Xtime1 <- tm_index(myCorpus, function(x) meta (x, "T1") == 1)
T1Xtime2 <- tm_index(myCorpus, function(x) meta (x, "T1") == 2)
T1Xtime3 <- tm_index(myCorpus, function(x) meta (x, "T1") == 3)

# T2

T2Xtime1 <- tm_index(myCorpus, function(x) meta (x, "T2") == 1)
T2Xtime2 <- tm_index(myCorpus, function(x) meta (x, "T2") == 2)
T2Xtime3 <- tm_index(myCorpus, function(x) meta (x, "T2") == 3)

# T3

T3Xtime1 <- tm_index(myCorpus, function(x) meta (x, "T3") == 1)
T3Xtime2 <- tm_index(myCorpus, function(x) meta (x, "T3") == 2)
T3Xtime3 <- tm_index(myCorpus, function(x) meta (x, "T3") == 3)

# T4

T4Xtime1 <- tm_index(myCorpus, function(x) meta (x, "T4") == 1)
T4Xtime2 <- tm_index(myCorpus, function(x) meta (x, "T4") == 2)
T4Xtime3 <- tm_index(myCorpus, function(x) meta (x, "T4") == 3)

indexT1Xtime <- list(T1Xtime1, T1Xtime2, T1Xtime3)
indexT2Xtime <- list(T2Xtime1, T2Xtime2, T2Xtime3)

# By grade

index_grade5 <- tm_index(myCorpus, function(x) meta(x, "grade") == 5)
index_grade6 <- tm_index(myCorpus, function(x) meta(x, "grade") == 6)

# Student by time

# S1

S1Xtime1 <- tm_index(myCorpus, function(x) meta (x, "S1") == 1)
S1Xtime2 <- tm_index(myCorpus, function(x) meta (x, "S1") == 2)
S1Xtime3 <- tm_index(myCorpus, function(x) meta (x, "S1") == 3)
S1Xtime4 <- tm_index(myCorpus, function(x) meta (x, "S1") == 4)
S1Xtime5 <- tm_index(myCorpus, function(x) meta (x, "S1") == 5)
S1Xtime6 <- tm_index(myCorpus, function(x) meta (x, "S1") == 6)

# S2

S2Xtime1 <- tm_index(myCorpus, function(x) meta (x, "S2") == 1)
S2Xtime2 <- tm_index(myCorpus, function(x) meta (x, "S2") == 2)
S2Xtime3 <- tm_index(myCorpus, function(x) meta (x, "S2") == 3)
S2Xtime4 <- tm_index(myCorpus, function(x) meta (x, "S2") == 4)
S2Xtime5 <- tm_index(myCorpus, function(x) meta (x, "S2") == 5)
S2Xtime6 <- tm_index(myCorpus, function(x) meta (x, "S2") == 6)

# S3

S3Xtime1 <- tm_index(myCorpus, function(x) meta (x, "S3") == 1)
S3Xtime2 <- tm_index(myCorpus, function(x) meta (x, "S3") == 2)
S3Xtime3 <- tm_index(myCorpus, function(x) meta (x, "S3") == 3)
S3Xtime4 <- tm_index(myCorpus, function(x) meta (x, "S3") == 4)
S3Xtime5 <- tm_index(myCorpus, function(x) meta (x, "S3") == 5)
S3Xtime6 <- tm_index(myCorpus, function(x) meta (x, "S3") == 6)

# S4

S4Xtime1 <- tm_index(myCorpus, function(x) meta (x, "S4") == 1)
S4Xtime2 <- tm_index(myCorpus, function(x) meta (x, "S4") == 2)
S4Xtime3 <- tm_index(myCorpus, function(x) meta (x, "S4") == 3)
S4Xtime4 <- tm_index(myCorpus, function(x) meta (x, "S4") == 4)
S4Xtime5 <- tm_index(myCorpus, function(x) meta (x, "S4") == 5)
S4Xtime6 <- tm_index(myCorpus, function(x) meta (x, "S4") == 6)

# S5

S5Xtime1 <- tm_index(myCorpus, function(x) meta (x, "S5") == 1)
S5Xtime2 <- tm_index(myCorpus, function(x) meta (x, "S5") == 2)
S5Xtime3 <- tm_index(myCorpus, function(x) meta (x, "S5") == 3)
S5Xtime4 <- tm_index(myCorpus, function(x) meta (x, "S5") == 4)
S5Xtime5 <- tm_index(myCorpus, function(x) meta (x, "S5") == 5)
S5Xtime6 <- tm_index(myCorpus, function(x) meta (x, "S5") == 6)


# Saving documents based on groups to a list ## need to fix when looped

# Student  

# index_MC <- tm_index(myCorpus, function(x) meta(x, "ID") == 301120) 
# index_AD <- tm_index(myCorpus, function(x) meta(x, "ID") == 301181)
# index_DN <- tm_index(myCorpus, function(x) meta(x, "ID") == 301134)
# index_JS <- tm_index(myCorpus, function(x) meta(x, "ID") == 301142)
# index_KH <- tm_index(myCorpus, function(x) meta(x, "ID") == 301212)

S3Xtime1 <- tm_index(myCorpus, function(x) meta (x, "S3") == 1)

index_MC <- list(S1Xtime1, S1Xtime2, S1Xtime3, S1Xtime4, S1Xtime5, S1Xtime6)
index_AD <- list(S2Xtime1, S2Xtime2, S2Xtime3, S2Xtime4, S2Xtime5, S2Xtime6)
index_DN <- list(S3Xtime1, S3Xtime2, S3Xtime3, S3Xtime4, S3Xtime5, S3Xtime6)
index_JS <- list(S4Xtime1, S4Xtime2, S4Xtime3, S4Xtime4, S4Xtime5, S4Xtime6)
index_KH <- list(S5Xtime1, S5Xtime2, S5Xtime3, S5Xtime4, S5Xtime5, S5Xtime6)

# Teachers
teachers <- list(index_T1, index_T2, index_T3, index_T4)

# # Time
time <- list(index_1, index_2, index_3, index_4, index_5, index_6)

# Grade

# doc_list <- list (index_grade5, index_grade6)

# Students

# doc_list <- list(index_MC, index_AD, index_DN, index_JS, index_KH)

# Selecting doc_list

doc_list <- time

#-------------------------------------------------------
# 6. Preparing data for clustering
#-------------------------------------------------------

# Removing outliers

# Creates booleans for outliers

doc_len_M <- mean(sort(colSums(as.matrix(TDM_common))))
doc_len_SD <- sd(sort(colSums(as.matrix(TDM_common))))

plusCI <- doc_len_M + 3.00*(doc_len_SD) # need to change this 
minusCI <- doc_len_M - 3.00*(doc_len_SD) # need to change this 

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

mat_t <- t(mat)

# Processing data for vectors

mat_list <- apply(mat, 2, list)

# Functions for deviation vectors

vect_project <- function(a,b){
  project <- crossprod(a,b) * b
  project
}

dev_vector <- function(vect_list){
  norm_vects <- lapply(vect_list, normalize.vector)
  sum_vect <- colSums(do.call(rbind, norm_vects))
  norm_sum <- normalize.vector(sum_vect)
  projects <- lapply(norm_vects, vect_project, norm_sum)
  difference <- mapply('-', norm_vects, projects)
  dev_vects <-  apply(difference, MARGIN = 2, FUN = normalize.vector)
  dev_vects
}

# Calculating deviation vectors

mat_vec <- lapply(mat_list, unlist)
mat_dev <- dev_vector(mat_vec)

#----------------------------------
# 7. Clustering and post-processing of clusters
#-------------------------------------------------------
  
# Set seed for reproducibility

set.seed(06132015)

# Calculating number of clusters

wss <- (nrow(mat)- 1) * sum(apply(mat, 2, var)) # need to change
for (i in 2:18) wss[i] <- sum(kmeans(mat,
                                     centers=i)$withinss)
plot(1:18, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

# Transposes matrix

mat_dev_t <- t(mat_dev)

# Fits Ward's 

distance <- dist(mat_dev_t, method = "euclidean")

mat_dev_t_clust <- hclust(distance)
hclust_cut <- cutree(mat_dev_t_clust, n_clusters)

# Clusters for Ward's

clusters1 <- list()

for (i in seq(n_clusters)){
  clusters1[[i]] <- mat_dev_t[hclust_cut == i,]
}

ordered_clusters1 <- list()
cluster_freqs1 <- list()

for (i in seq(length(clusters1))){
  
  ordered_clusters1[[i]] <- colSums(as.matrix(clusters1[[i]]) / nrow(clusters1[[i]]))
  
  cluster_freqs1[[i]] <- ordered_clusters1[[i]]
}

# Fits kmeans algorithm

set.seed(06132015)

start <- data.frame(matrix(unlist(cluster_freqs1), nrow=length(cluster_freqs1[[1]]), byrow=T),stringsAsFactors=FALSE)
start <- t(as.matrix(start))
kfit <- kmeans(mat_dev_t, start)

# for each of the nclusters (4) as rows in a matrix you'll have values for all of the y's as columns in a matrix

# Need to fix - compare this to euclidean distance

# cos.sim <- function(ix) 
# {
#   A = X[ix[1],]
#   B = X[ix[2],]
#   return( sum(A*B)/sqrt(sum(A^2)*sum(B^2)) )
# }   
# n <- nrow(X) 
# cmb <- expand.grid(i=1:n, j=1:n) 
# C <- matrix(apply(cmb,1,cos.sim),n,n)

# kfit <- kmeans(adj_mat, centers = n_clusters, nstart = 25, iter.max = 1000) ## need to fix this doesn't help

# table(kfit$cluster)
# kfit$tot.withinss
# kfit$withinss
# kfit$betweenss
# kfit$totss

# Creating a list of weighted term document matrices for each cluster

clusters <- list()

for (i in seq(n_clusters)){
  clusters[[i]] <- mat_dev_t[kfit$cluster == i, ]
}

# Creating an ordered list of clusters

ordered_clusters <- list()
cluster_freqs <- list()
clusters_df <- matrix(nrow = 10, ncol = n_clusters)

for (i in seq(length(clusters))){
  ordered_clusters[[i]] <- colSums(as.matrix(clusters[[i]]) / nrow(clusters[[i]]))
  cluster_freqs[[i]] <- ordered_clusters[[i]]
  ordered_clusters[[i]] <- names(sort(ordered_clusters[[i]], decreasing = TRUE))
  clusters_df[, i] <- ordered_clusters[[i]][1:10]
}

clusters_df <- as.data.frame(clusters_df)
colnames(clusters_df) <- paste("Cluster ", c(seq(n_clusters)), sep="")

### Creating an adjusted (by relevance) ordered list of clusters ## need to fix - this is not working (I think because of freq_terms)

# adjordered_clusters <- list()
# adjclusters_df <- matrix(nrow = 10, ncol = n_clusters)
# 
# for (i in seq(length(clusters))){
#   adjordered_clusters[[i]] <- colSums(as.matrix(clusters[[i]]) / nrow(clusters[[i]]))
#   adjordered_clusters[[i]] <- adjordered_clusters[[i]] - freq_terms / length(freq_terms)
#   adjordered_clusters[[i]] <- names(sort(adjordered_clusters[[i]], decreasing = TRUE))
#   adjclusters_df[, i] <- adjordered_clusters[[i]][1:10]
# }
# 
# adjclusters_df <- as.data.frame(adjclusters_df)
# colnames(adjclusters_df) <- paste("Cluster ", c(seq(n_clusters)), sep="")

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
  print(table(doc_list_cleaned[[i]]))
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

TDM_common

# Number of documents in each cluster
# 
# print(table(kfit$cluster))

# 10 most frequently included terms in each cluster and adjusted terms

print(clusters_df)
sort(table(hclust_cut))
sort(table(kfit$cluster))
# print(adjclusters_df)

# Cosines

print(cosines_df)

# Wordclouds ## need to fix

# wordclouds <- list()

# for (i in 1:length(clusters)){
#   wordclouds[[i]] <- wordcloud(names(wc[[i]][1:40]), wc[[i]][1:40]) # Still need to print wordclouds
# }

# Plot
# 
# cos_plot <- gather(cosines_df, cluster, cosines)
# cos_plot$group <- rep(1:length(doc_list), length(cosines_df))
# ggplot(data = cos_plot, aes(x = group, y = cosines, fill = cluster)) +
#   geom_bar(position = "dodge", stat = "identity", width = .75)

# Scaled plot

# cosines_scaled <- as.data.frame(cosines_scaled)
# y <- cosines_scaled[1:length(doc_vec), ]
# x <- gather(y, cluster, cosines)
# x$observation <- rep(1:length(doc_vec), length(cosines))
# ggplot(data = x, aes(x = observation, y = cosines, fill = cluster)) +
# geom_bar(position = "dodge", stat = "identity", width = .75)

# For teachers

# cos_plot <- gather(cosines_df, Cluster, cosines)
# 
# cos_plot$group <- rep(c("1_J", "2_Paul", "3_Cathy", "4_Julie"), length(cosines_df_scaled))
# 
# ggplot(data = cos_plot, aes(x = group, y = cosines, fill = Cluster)) +
#   geom_bar(position = "dodge", stat = "identity", width = .75) +
#   xlab("Teacher") +
#   ylab("Cosines (z-score)") +
#   ggtitle("Convince audience cosines (z-score) by teachers")

# For time

cos_plot <- gather(cosines_df, Cluster, cosines)

cos_plot$group <- rep(1:length(doc_list), length(cosines_df))

ggplot(data = cos_plot, aes(x = group, y = cosines, fill = Cluster)) +
  geom_bar(position = "dodge", stat = "identity", width = .75) +
  xlab("Time") +
  ylab("Cosines") +
  ggtitle("Criteria cosines by time")

# Plot
# 
# cos_plot <- gather(cosines_df, cluster, cosines)
# cos_plot$group <- rep(1:length(doc_list), length(cosines_df))
# ggplot(data = cos_plot, aes(x = group, y = cosines, fill = cluster)) +
#   geom_bar
