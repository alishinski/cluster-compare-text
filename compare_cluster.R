#-------------------------------------------------------
# 1. Setting working directory, loading packages, and loading files
#-------------------------------------------------------

rm(list = ls(all = TRUE))

setwd("~/Dropbox/research/cluster-compare-text/")

library(tidyr)
library(SnowballC)
library(tm)
library(lsa)
library(ggplot2)
library(ppls)
library(ggthemes)
library(quanteda)
library(dplyr)

# Loading data frame
all <- read.csv("scip_data.csv")

all <- mutate(all,
              doc_num = 1:nrow(all))

# Selecting columns of dataframe by question topic
aud1 <- select(all, audience1, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5, doc_num)
aud2 <- select(all, audience2, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5, doc_num)
gen <- select(all, generality, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5, doc_num)
evid <- select(all, evidence, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5, doc_num)
purp <- select(all, purpose, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5, doc_num)
crit <- select(all, criteria, grade, teacher, ID, time, T1, T2, T3, T4, S1, S2, S3, S4, S5, doc_num)

# Other variables to modify
n_clusters <- 6
minimum_groups <- 2 # minimum groups a term has to be in for it to be included
minimum_term_frequency <- 4 # minimum times a term has to occur overall for it to be included
min_terms <- 2 # minimum terms in a document for it to be included
term <- "purpose"

# Selecting data frame

# text_vector <- aud1$audience1 # 6 clusters, 1 group: 1/2/3 # msu / teacher / research class, content, content, help understand, anyone, people who want 
# text_vector <- aud2$audience2 # 3, groups: 1/2/3/4 # show them model
# text_vector <- gen$generality # 5 clusters, 2 groups: 2/3/4 # gen, spec, diff, spec, spec - content dep.
# text_vector <- evid$evidence # 4 clusters, 1 group: 1/2/3 # interesting distinction between info. and fit evid.
text_vector <- purp$purpose # 6 clusters, 2 groups: 2/3/4 # shows distinct knowledge-building purposes
# text_vector <- crit$criteria # 2 clusters, 1 group: 1/2/3 # good, two clear groups

print_path1 <- paste0("results/png/all/", term, ".png")
print_path2 <- paste0("results/png/teacher/", term, ".png")
print_path5 <- paste0("results/png/time/", term, ".png")
print_path6 <- paste0("results/png/grade/", term, ".png")
print_path3 <- paste0("results/csv/terms/", term, ".csv")
print_path4 <- paste0("results/csv/freqs/", term, ".csv")

# Custom stopwords

# Standard stopwords 

standard_stopwords <- c("a", "an", "the", "to", "of", "and", "for", "by", "on", "is", "I", "all", "this", "with", 
                        "it", "at", "from", "or", "you", "as", "your", "are", "be", "that", "not", "have", "was",
                        "we", "what", "which", "there", "they", "he", "she", "his", "hers", "had", "word", "our", 
                        "you", "about", "that", "this", "but", "not", "what")

# Additional stopwords

additional_stopwords <- c("lorum")

# Combine standard and additional stopwords

all_stopwords <- append(standard_stopwords, additional_stopwords)

# -----------------------------------------------------------
# 2. Process text
# -----------------------------------------------------------

dfm <- dfm(text_vector, removeTwitter = T, stem = T, ignoredFeatures = all_stopwords)
TDM <- t(convert(dfm, "tm"))
myCorpus <- Corpus(VectorSource(text_vector)) # for indexing - will remove

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
  meta(myCorpus[[i]], "doc_num") <- aud1$doc_num[i]
}

# Creates logicals based on names of documents

# By teacher

index_T1 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 1)
index_T2 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 2)
index_T3 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 3)
index_T4 <- tm_index(myCorpus, function(x) meta(x, "teacher") == 4)

teacher <- list(index_T1, index_T2, index_T3, index_T4)

# By grade

index_grade5 <- tm_index(myCorpus, function(x) meta(x, "grade") == 5)
index_grade6 <- tm_index(myCorpus, function(x) meta(x, "grade") == 6)

grade <- list(index_grade5, index_grade6)

# By time

index_1 <- tm_index(myCorpus, function(x) meta(x, "time") == 1)
index_2 <- tm_index(myCorpus, function(x) meta(x, "time") == 2)
index_3 <- tm_index(myCorpus, function(x) meta(x, "time") == 3)
index_4 <- tm_index(myCorpus, function(x) meta(x, "time") == 4)
index_5 <- tm_index(myCorpus, function(x) meta(x, "time") == 5)
index_6 <- tm_index(myCorpus, function(x) meta(x, "time") == 6)

doc_list <- list(index_1, index_2, index_3, index_4, index_5, index_6)

# Doc num

doc_names <- vector()
for (i in 1:length(myCorpus)){
  doc_names[i] <- meta(myCorpus[[i]])$doc_num
}

# -----------------------------------------------------------
# 4. Cleans term document matrices 
# -----------------------------------------------------------

# For time

time_list <- list()
matrix_list <- list()
index_list <- list()

for (i in 1:length(doc_list)){
  time_list[[i]] <- TDM[, doc_list[[i]]]
  matrix_list[[i]] <- as.matrix(time_list[[i]])
  index_list[[i]] <- rowSums(matrix_list[[i]]) > 0
}

new_index <- index_list[[1]] + index_list[[2]] + index_list[[3]] + index_list[[4]] + index_list[[5]] + index_list[[6]]
new_index <- new_index >= minimum_groups

# Filters

term_sums <- rowSums(as.matrix(TDM))
term_logical <- term_sums >= minimum_term_frequency  # change these parameters - this has to do with the minimum frequencies to be included in vocabulary

TDM_common <- TDM[term_logical & new_index, ] # indexes TDM based on minimum frequencies and maximum frequencies and occuring at least once in both hashtags

# Finds and removes documents with no terms

adjminusCI_bool <- colSums(as.matrix(TDM_common)) <= min_terms
doc_outliers <- adjminusCI_bool
TDM_cleaned <- TDM_common[, !doc_outliers]

# Filters matrix by boolean vectors to remove term outliers resulting from removing document outliers

term_outliers <- rowSums(as.matrix(TDM_cleaned)) == 0
TDM_cleaned <- TDM_cleaned[!term_outliers, ]

# Filters doc_list

adjminusCI <- !adjminusCI_bool
doc_list_cleaned <- list()
doc_list_cleaned_teacher <- list()
doc_list_cleaned_grade <- list()
doc_list_cleaned_student <- list()

for (i in seq(doc_list)){
  doc_list_cleaned[[i]] <- doc_list[[i]] & adjminusCI
  doc_list_cleaned[[i]] <- doc_list_cleaned[[i]][!doc_outliers]
}

for (i in seq(teacher)){
  doc_list_cleaned_teacher[[i]] <- teacher[[i]] & adjminusCI
  doc_list_cleaned_teacher[[i]] <- teacher[[i]][!doc_outliers]
}

for (i in seq(grade)){
  doc_list_cleaned_grade[[i]] <- grade[[i]] & adjminusCI
  doc_list_cleaned_grade[[i]] <- grade[[i]][!doc_outliers]
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

wss <- list()
for (i in 2:18){
  wss[i] <- sum(kmeans(mat, centers = i)$betweenss) / kmeans(mat, centers = i)$totss
} 

wss <- as.data.frame(unlist(wss))
names(wss) <- "prop_explained"
wss <- mutate(wss, 
              clusters = as.numeric(row.names(wss)))
wss_plot <- ggplot(wss, aes(x = clusters, y = prop_explained)) +
  geom_point()

print(plot(2:18, unlist(wss), type="b", xlab="Number of Clusters", ylab="Proportion of Variance Explained"))

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
# 7. Finds frequenies of clusters for groups
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
TDM_group_teacher <- list()
TDM_group_grade <- list()

for (i in seq(doc_list_cleaned)){
  TDM_group[[i]] <- t(as.matrix(TDM_cleaned))[doc_list_cleaned[[i]],]
}

for (i in seq(doc_list_cleaned_teacher)){
  TDM_group_teacher[[i]] <- t(as.matrix(TDM_cleaned))[doc_list_cleaned_teacher[[i]],]
}

for (i in seq(doc_list_cleaned_grade)){
  TDM_group_grade[[i]] <- t(as.matrix(TDM_cleaned))[doc_list_cleaned_grade[[i]],]
}

# Calculates term frequencies for each group 

group_freqs <- list()
group_freqs_teacher <- list()
group_freqs_grade <- list()

for (i in seq(doc_list_cleaned)){
  group_freqs[[i]] <- colSums(as.matrix(TDM_group[[i]])) / nrow(TDM_group[[i]]) # Need to fix - will want to add group freqs
}

for (i in seq(doc_list_cleaned_teacher)){
  group_freqs_teacher[[i]] <- colSums(as.matrix(TDM_group_teacher[[i]])) / nrow(TDM_group_teacher[[i]]) # Need to fix - will want to add group freqs
}

for (i in seq(doc_list_cleaned_grade)){
  group_freqs_grade[[i]] <- colSums(as.matrix(TDM_group_grade[[i]])) / nrow(TDM_group_grade[[i]]) # Need to fix - will want to add group freqs
}

# -----------------------------------------------------------
# 8. Final processing
# -----------------------------------------------------------

# This makes a data frame with the top 10 words in each response

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
write.csv(term_results_df, print_path3)

# This makes a data frame with the frequencies of the top 10 words in each response

create_freq_results <- function(){
  freq_results_df <- matrix(nrow = 10, ncol = n_clusters)
  for (i in 1:n_clusters){
    freq_results_df[, i] <- sort(cluster_freqs[[i]], decreasing = T)[1:10]
  }
  freq_results_df
}
freq_results_df <- create_freq_results()
write.csv(freq_results_df, print_path4)

# This makes the results for time 

doc_by_index <- function(doc){
  my_matrix <- matrix(nrow = n_clusters, ncol = length(doc))
  for (i in 1:length(doc)){
    my_matrix[,i] <- table(factor(kfit$cluster[doc[[i]]], levels = 1:n_clusters))
  }
    my_matrix
}

doc_by_index_time <- doc_by_index(doc_list_cleaned)
doc_by_index_teacher <- doc_by_index(doc_list_cleaned_teacher)
doc_by_index_grade <- doc_by_index(doc_list_cleaned_grade)

colnames(doc_by_index_time) <- paste0("Time, ", 1:length(doc_list_cleaned)) # time
colnames(doc_by_index_teacher) <- c("Paul", "J", "Cathy", "Julie") # teacher
colnames(doc_by_index_grade) <- c("5th Grade", "6th Grade")

rownames(doc_by_index_time) <- paste0("Cluster ", 1:n_clusters)
rownames(doc_by_index_teacher) <- paste0("Cluster ", 1:n_clusters)
rownames(doc_by_index_grade) <- paste0("Cluster ", 1:n_clusters)

# Identifies ID with each cluster

ID_by_cluster <- data.frame(cluster = kfit$cluster, doc_num = doc_names[adjminusCI])

# -----------------------------------------------------------
# 9. Preparing plots
# -----------------------------------------------------------

# For all

doc_plot <- as.data.frame(table(kfit$cluster))

chisq_p <- chisq.test(doc_plot[, 2])
chisq_p <- chisq_p$stdres > 1.96 | chisq_p$stdres < -1.96

doc_plot <- cbind(doc_plot, as.vector(chisq_p))
names(doc_plot) <- c("Cluster", "N", "ChiSq")
doc_plot[, 3][(doc_plot[, 3] == TRUE)] <- "*"
doc_plot[, 3][(doc_plot[, 3] == FALSE)] <- ""

plot <- ggplot(doc_plot, aes(x = Cluster, y = N, ymax = max(N))) + 
  geom_bar(width = .825, stat = "identity") + 
  geom_text(aes(label = ChiSq), vjust = -.25) +
  theme_minimal()
theme(text = element_text(size = 12, family = "Times New Roman"))

print(plot)
ggsave(print_path1)

# For all plots

create_plot <- function(doc_by_index){
  
  chisq_p <- chisq.test(doc_by_index)
  asterisk <- as.vector(chisq_p$stdres > 1.96 | chisq_p$stdres < -1.96)
  asterisk[asterisk == TRUE] <- "*"
  asterisk[asterisk == FALSE] <- ""
  
  doc_plot <- gather(as.data.frame(doc_by_index), Group, N)
  doc_plot <- cbind(doc_plot, asterisk)

  doc_plot[, 4] <- rep(paste0(1:n_clusters), ncol(doc_by_index))
  names(doc_plot)[3:4] <- c("ChiSq", "Cluster")
  
  dodge = position_dodge(.9)
  
  plot <- ggplot(doc_plot, aes(x = Group, y = N, fill = Cluster, ymax = max(N))) + 
    geom_bar(width = .825, position = dodge, stat = "identity") + 
    geom_text(aes(label = ChiSq), position = dodge, vjust = .25) +
    theme_minimal()
  
  theme(text = element_text(size = 12, family = "Times New Roman"))
  
  plot
  
}

# -----------------------------------------------------------
# 10. Output
# -----------------------------------------------------------

# Diagnostics

print(TDM_cleaned)
print(wss_plot)

# Qualitative analysis

find_docs <- function(which_cluster){
  
  ID_vector <- vector()
  for (i in 1:length(myCorpus)) {
    ID_vector <- append(ID_vector, meta(myCorpus[[i]])$doc_num)
  }
  
  indexed_ID_vector <- vector()
  for (i in 1:nrow(ID_by_cluster)){
    if (ID_by_cluster$cluster[i] == which_cluster){
      indexed_ID_vector <- append(indexed_ID_vector, ID_by_cluster$doc_num[i])
    } 
  }
  
  all$doc_num %in% indexed_ID_vector
  
}

find_docs(1)
print(as.character(all$audience1[doc_index]))
# print(as.character(all$audience2[doc_index]))
# print(as.character(all$generality[doc_index]))
# print(as.character(all$purpose[doc_index]))
# print(as.character(all$evidence[doc_index]))
# print(as.character(all$criteria[doc_index]))

# Results

print(term_results_df)
print(freq_results_df)

print(plot)
create_plot(doc_by_index_time)
create_plot(doc_by_index_teacher)
create_plot(doc_by_index_grade)

mean(colSums(as.matrix(TDM)))
mean(colSums(as.matrix(TDM_common)))

my_text_vector <- as.character(text_vector)
my_dfm <- dfm(my_text_vector)
mean(ntoken(my_dfm))
