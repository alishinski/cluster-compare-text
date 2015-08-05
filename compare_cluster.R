#-------------------------------------------------------
# 1. Setting working directory, loading packages, and loading files
#-------------------------------------------------------

rm(list = ls(all = TRUE))

setwd("~/Dropbox/research/cluster-compare-text/")

library(dplyr)
library(tidyr)
library(SnowballC)
library(tm)
library(lsa)
library(ggplot2)
library(ppls)

# Loading data frame
all <- read.csv("scip_data.csv")

# Selecting columns of dataframe by question topic
aud1 <- select(all, audience1, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)
aud2 <- select(all, audience2, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)
gen <- select(all, generality, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)
evid <- select(all, evidence, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)
purp <- select(all, purpose, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)
crit <- select(all, criteria, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5)

# Other variables to modify
n_clusters <- 3
minimum_groups <- 3
minimum_term_frequency <- 3
term <- "audience"

# Selecting data frame

text_vector <- aud1$audience1 # 3, groups, freqs
# text_vector <- aud2$audience2 # 3, groups, freqs
# text_vector <- gen$generality # 4, groups, freqs
# text_vector <- evid$evidence # 4, groups, freqs
# text_vector <- purp$purpose # 5, groups, freqs
# text_vector <- crit$criteria # 3, groups, freqs

print_path1 <- paste0("results/png/", term, ".png")
print_path2 <- paste0("results/png/term", term, ".csv")
print_path3 <- paste0("results/png/freq", term, ".csv")

# Custom stopwords

# Standard stopwords 

standard_stopwords <- c("a", "an", "the", "to", "of", "and", "for", "by", "on", "is", "I", "all", "this", "with", 
                        "it", "at", "from", "or", "you", "as", "your", "are", "be", "that", "not", "have", "was",
                        "we", "what", "which", "there", "they", "he", "she", "his", "hers", "had", "word", "our", 
                        "you", "about", "that", "this", "but", "not", "what")

# Additional stopwords

additional_stopwords <- c("water")

# Combine standard and additional stopwords

all_stopwords <- append(standard_stopwords, additional_stopwords)

# -----------------------------------------------------------
# 2. Process text
# -----------------------------------------------------------

# Modified stemCompletion function as stemCompletion included with the tm package was not working

stemCompletionMod <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  PlainTextDocument(stripWhitespace(x))
}

# Creating and processing corpus

myCorpus <- Corpus(VectorSource(text_vector))
myCorpus <- tm_map(myCorpus, content_transformer(tolower), mc.cores = 1) # makes text lowercase
myCorpus <- tm_map(myCorpus, removePunctuation, mc.cores = 1) # removes punctuation
myCorpus <- tm_map(myCorpus, removeNumbers, mc.cores = 1) # removes numbers
myCorpus <- tm_map(myCorpus, removeWords, all_stopwords, mc.cores=1) # removes stopwords
myCorpus_copy <- myCorpus # makes a copy of the corpus for the stepCompletionMod function to compare to 
myCorpus <- tm_map(myCorpus, stemDocument, mc.cores = 1) # stems text
myCorpus <- tm_map(myCorpus, stemCompletionMod, myCorpus_copy) # completes stemmed text
myCorpus <- tm_map(myCorpus, stripWhitespace, mc.cores = 1) # removes whitespace
myCorpus <- tm_map(myCorpus, PlainTextDocument)

print(paste("Processed ", length(text_vector), " documents", sep = ""))

# -----------------------------------------------------------
# 3. Creates a term document matrix from the processed text
# -----------------------------------------------------------

TDM <- TermDocumentMatrix(myCorpus)

# Names documents

for (i in 1:length(myCorpus)){
  meta(myCorpus[[i]], "teacher") <- aud1$teacher[i] 
  meta(myCorpus[[i]], "grade") <- aud1$grade[i]
  meta(myCorpus[[i]], "ID") <- aud1$ID[i]
  meta(myCorpus[[i]], "time") <- aud1$time[i]
}

# Creates logicals based on names of documents

# By teacher

index_T1 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 1)
index_T2 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 2)
index_T3 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 3)
index_T4 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 4)

teacher <- list(index_T1, index_T2, index_T3, index_T4)

# By time

index_1 <- tm_index(myCorpus, function(x) meta(x, "time") == 1)
index_2 <- tm_index(myCorpus, function(x) meta(x, "time") == 2)
index_3 <- tm_index(myCorpus, function(x) meta(x, "time") == 3)
index_4 <- tm_index(myCorpus, function(x) meta(x, "time") == 4)
index_5 <- tm_index(myCorpus, function(x) meta(x, "time") == 5)
index_6 <- tm_index(myCorpus, function(x) meta(x, "time") == 6)

time <- list(index_1, index_2, index_3, index_4, index_5, index_6)

# By grade

index_grade5 <- tm_index(myCorpus, function(x) meta(x, "grade") == 5)
index_grade6 <- tm_index(myCorpus, function(x) meta(x, "grade") == 6)

grade <- list(index_grade5, index_grade6)

doc_list <- teacher # choose between teacher, time, and grade

# -----------------------------------------------------------
# 4. Cleans term document matrices 
# -----------------------------------------------------------

# # For time
# 
# time_list <- list()
# matrix_list <- list()
# index_list <- list()
# 
# for (i in 1:length(doc_list)){
#   time_list[[i]] <- TDM[, doc_list[[i]]]
#   matrix_list[[i]] <- as.matrix(time_list[[i]])
#   index_list[[i]] <- rowSums(matrix_list[[i]]) > 0
# }
# 
# new_index <- index_list[[1]] + index_list[[2]] + index_list[[3]] + index_list[[4]] + index_list[[5]] + index_list[[6]]
# new_index <- new_index >= minimum_groups

# For teacher

teacher_list <- list()
matrix_list <- list()
index_list <- list()

for (i in 1:length(doc_list)){
  teacher_list[[i]] <- TDM[, doc_list[[i]]]
  matrix_list[[i]] <- as.matrix(teacher_list[[i]])
  index_list[[i]] <- rowSums(matrix_list[[i]]) > 0
}

new_index <- index_list[[1]] + index_list[[2]] + index_list[[3]] + index_list[[4]]
new_index <- new_index >= minimum_groups

# Filters

term_sums <- rowSums(as.matrix(TDM))
term_logical <- term_sums >= minimum_term_frequency  # change these parameters - this has to do with the minimum frequencies to be included in vocabulary

TDM_common <- TDM[term_logical & new_index, ] # indexes TDM based on minimum frequencies and maximum frequencies and occuring at least once in both hashtags

# Finds and removes documents with no terms

adjminusCI_bool <- colSums(as.matrix(TDM_common)) < 1 # need to fix - change this 
doc_outliers <- adjminusCI_bool
TDM_cleaned <- TDM_common[, !doc_outliers]

# Filters matrix by boolean vectors to remove term outliers resulting from removing document outliers

term_outliers <- rowSums(as.matrix(TDM_cleaned)) == 0
TDM_cleaned <- TDM_cleaned[!term_outliers, ]

# Filters doc_list

adjminusCI <- !adjminusCI_bool
doc_list_cleaned <- list()
for (i in seq(doc_list)){
  doc_list_cleaned[[i]] <- doc_list[[i]] & adjminusCI
  doc_list_cleaned[[i]] <- doc_list_cleaned[[i]][!doc_outliers]
}



# -----------------------------------------------------------
# 5. Creates deviation vectors to prepare for clustering
# -----------------------------------------------------------

# Creates matrix which needs to have deviation vectors calculated

mat <- as.matrix(TDM_cleaned)

# Processing data for vectors into list

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

# -----------------------------------------------------------
# 6. Clusters documents
# -----------------------------------------------------------

# Estimates number of clusters using within-cluster ss - change to between-cluster (e.g., residual) ss?

wss <- (nrow(mat)- 1) * sum(apply(mat, 2, var)) # need to change
for (i in 2:18) wss[i] <- sum(kmeans(mat,
                                     centers=i)$withinss)

# Transposes matrix

mat_dev_t <- t(mat_dev)

# Fits Ward's hierarchical algorithm

distance <- dist(mat_dev_t, method = "euclidean") 
mat_dev_t_clust <- hclust(distance) 
hclust_cut <- cutree(mat_dev_t_clust, n_clusters) # cuts the results of the hierarchical cluster at the specified # of clusters

# Processes clusters from Ward's for start values for k-means algorithm 

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

# Fits k-means algorithm using results from hierarchical algorithm as start values

start <- data.frame(matrix(unlist(cluster_freqs1), nrow=length(cluster_freqs1[[1]]), byrow=T),stringsAsFactors=FALSE)
start <- as.matrix(start)
start <- t(start)
kfit <- kmeans(mat_dev_t, start)
ss_explained <- sum(kfit$betweenss) / kfit$totss

# -----------------------------------------------------------
# 7. Finds cosine similarity between clusters and groups
# -----------------------------------------------------------

# Creates clusters using cluster logicals

clusters <- list()
for (i in seq(n_clusters)){
  clusters[[i]] <- t(as.matrix(TDM_cleaned))[kfit$cluster == i, ]
}

# Creates term frequencies for each cluster

cluster_freqs <- list()
for (i in seq(n_clusters)){
  cluster_freqs[[i]] <- colSums(clusters[[i]]) / nrow(clusters[[i]]) # Need to fix - will want to add group freqs
}

# Creates groups using group logicals

TDM_group <- list()
for (i in seq(doc_list_cleaned)){
  TDM_group[[i]] <- t(as.matrix(TDM_cleaned))[doc_list_cleaned[[i]],]
}

# Calculates term frequencies for each group 

group_freqs <- list()
for (i in seq(doc_list)){
  group_freqs[[i]] <- colSums(as.matrix(TDM_group[[i]])) / nrow(TDM_group[[i]]) # Need to fix - will want to add group freqs
}

# Computes similarities using the cosine function

cosines <- list()
cosines_list <- list()
for (i in seq(length(TDM_group))){ 
  cosines[[i]] <- vector()
  for (j in seq(length(clusters))){
    cosines[[i]] <- append(cosines[[i]], cosine(group_freqs[[i]], cluster_freqs[[j]]))
  }
  cosines_list[[i]] <- cosines[[i]]
}

# -----------------------------------------------------------
# 8. Final processing
# -----------------------------------------------------------

create_term_results <- function(){
  term_results_df <- matrix(nrow = 10, ncol = n_clusters)
  freq_results_df <- matrix(nrow = 10, ncol = n_clusters)
  for (i in 1:n_clusters){
    term_results_df[, i] <- names(sort(cluster_freqs[[i]], decreasing = T))[1:10]
    freq_results_df[, i] <- sort(cluster_freqs[[i]], decreasing = T)[1:10]
  }
  term_results_df
}

term_results_df <- create_term_results()

write.csv(term_results_df, print_path2)

create_freq_results <- function(){
  freq_results_df <- matrix(nrow = 10, ncol = n_clusters)
  for (i in 1:n_clusters){
    freq_results_df[, i] <- sort(cluster_freqs[[i]], decreasing = T)[1:10]
  }
  freq_results_df
}

write.csv(freq_results_df, print_path3)

freq_results_df <- create_freq_results()


doc_by_index <- function(x){
  my_matrix <- matrix(nrow = n_clusters, ncol = length(doc_list_cleaned))
  for (i in 1:length(doc_list_cleaned)){
    my_matrix[,i] <- table(factor(kfit$cluster[doc_list_cleaned[[i]]], levels = 1:n_clusters))
  }
    my_matrix
}

doc_by_index_matrix <- doc_by_index()

colnames(doc_by_index_matrix) <- c("Paul", "J", "Cathy", "Julie") # teacher
# colnames(doc_by_index_matrix) <- paste0("Time, ", 1:length(doc_list_cleaned)) # time
rownames(doc_by_index_matrix) <- paste0("Cluster ", 1:n_clusters)

chisq <- chisq.test(doc_by_index_matrix)
chisq_p <- chisq$stdres > 1.96 | chisq$stdres < -1.96

# -----------------------------------------------------------
# 9. Preparing plots
# -----------------------------------------------------------

library(ggthemes)
doc_by_index_df <- as.data.frame(doc_by_index_matrix)
doc_plot <- gather(doc_by_index_df, Group, N)
doc_plot <- cbind(doc_plot, as.vector(chisq_p))
doc_plot[, 4] <- rep(paste0(1:n_clusters), length(doc_list_cleaned))
doc_plot[, 3][(doc_plot[, 3] == TRUE)] <- "*"
doc_plot[, 3][(doc_plot[, 3] == FALSE)] <- ""
doc_plot
names(doc_plot)[3:4] <- c("ChiSq", "Cluster")

dodge = position_dodge(.9)
plot <- ggplot(doc_plot, aes(x = Group, y = N, fill = Cluster, ymax = max(N))) + 
  geom_bar(width = .825, position = dodge, stat = "identity") + 
  geom_text(aes(label = ChiSq), position = dodge, vjust = .25) +
  theme_minimal()
  theme(text = element_text(size = 12, family = "Times New Roman"))

ggsave(print_path1)

# -----------------------------------------------------------
# 10. Output
# -----------------------------------------------------------

# Diagnostics

print(TDM_cleaned)
print(plot(1:18, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares"))
print(ss_explained)
print(table(kfit$cluster))

# Results

print(term_results_df)
print(freq_results_df)
print(doc_by_index_matrix)
print(chisq_p)
print(plot)
