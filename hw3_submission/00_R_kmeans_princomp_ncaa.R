dataset <- "ncaa" #ncaa, sensor

#Load libraries -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(dplyr)
library(parallel)
library(doMC)
library(foreach)
library(ggplot2)
library(mclust)
library(fastICA)
library(RandPro)
library(e1071)
library(ranger)

#Set directories -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
data_directory <- "/Users/mikepecorino/Documents/machine_learning/HW3/"

#Source functions -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Normalization to have 0 mean and standard deviation of 1
normalize_data <- function(data, features) {
  
  #Debugging
  #data <- data_all
  #features <- features
  
  #Normalize
  data[, features] <- apply(data[, features], FUN = function(x) (x - mean(x))/sd(x), MARGIN = 2)
  
  #Return the data
  return(data)
}

#kmeans clustering with parallel
kmeans_search <- function(data, features, algorithms, centers, iter.max, nstarts, trace, cores) {
  
  #Debugging
  #data <- data_all
  #algorithms <- c("Hartigan-Wong")#, "Lloyd", "Forgy", "MacQueen")
  #centers <- seq(1, 15, 1)
  #iter.max <- 300
  #nstarts <- seq(1, 10, 1)
  #trace <- FALSE
  #cores <- 6
  
  #Parallel
  doMC::registerDoMC(cores)
  kmeans_results <- foreach(algorithm = algorithms, .combine = "rbind") %:%
    foreach(center = centers, .combine = "rbind") %:%
    foreach(nstart = nstarts, .combine = "rbind") %dopar% {
      
      #Fit
      fit_start_time <- Sys.time()
      set.seed(nstart)
      kmeans_clustering <- kmeans(x = data[, features], centers = center, iter.max = iter.max, nstart = nstart, algorithm = algorithm, trace = FALSE)
      fit_end_time <- Sys.time()
      fit_total_time <- fit_end_time - fit_start_time
      
      #Collect output
      kmeans_clustering_output <- data.frame(algorithm = algorithm,
                                             centers = center,
                                             nstarts = nstart,
                                             iter.max = iter.max,
                                             fit_time = fit_total_time,
                                             totss = kmeans_clustering$totss,
                                             withinss = kmeans_clustering$tot.withinss,
                                             betweenss = kmeans_clustering$betweenss)
      
      #Output for rbind
      kmeans_clustering_output
    }
}

#Load inputs -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#1. All data
data_all <- read.csv(paste0(data_directory, dataset, "_all.csv"))

#Define features and response -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
if (dataset == "sensor") {
  features <- names(data_all)[!(names(data_all) %in% c("subject", "activity_raw", "activity", "tag", "fold", "response_prop"))]
  response <- "activity"
} else if (dataset == "ncaa") {
  features <- c("game_win_perc_prop",
                "game_starters_total_minutes_prop", "game_starters_prop_minutes_prop", "game_player_pts_10plus_prop",
                "game_player_pts_15plus_prop", "game_player_pts_16plus_prop", "game_player_pts_17plus_prop",
                "game_player_pts_18plus_prop", "game_player_pts_19plus_prop", "game_player_pts_20plus_prop",
                "game_player_pts_21plus_prop", "game_player_pts_22plus_prop", "game_player_ast_3plus_prop",
                "game_player_ast_5plus_prop", "game_player_ast_7plus_prop",
                "game_player_orb_1plus_prop", "game_player_orb_2plus_prop", "game_player_orb_3plus_prop",
                "game_player_drb_5plus_prop", "game_player_drb_7plus_prop", "game_player_drb_10plus_prop",
                "game_gs_mean_prop", "game_gs_max_prop", "game_pos_prop", "game_pts_prop", "game_efficiency_prop",
                "game_fg_attempted_prop", "game_ft_attempted_prop", "game_ft_made_prop", "game_stl_prop",
                "game_tov_prop","game_stl_tov_ratio_diff", "game_stl_tov_ratio_prop", "game_blk_prop",
                "game_orb_prop", "game_drb_prop", "game_trb_prop", "game_ast_prop", "game_pf_diff", "game_pf_prop",
                "home_indicator.x", "neutral_indicator")
  response <- "win_indicator"
}

#Normalize data -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Normalize by subtracting the mean and dividing by the standard deviation
data_all <- normalize_data(data = data_all, features = features)

#Split into train and test -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
data_all_train <- data_all[data_all$tag %in% c("train", "valid"), ]
data_all_test <- data_all[data_all$tag == "test", ]

#K-Means clustering -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
kmeans_results <- kmeans_search(data = data_all_train,
                                features = features,
                                algorithms = c("Hartigan-Wong"), #"Lloyd", "Forgy", "MacQueen"
                                centers = seq(1, 15, 1),
                                iter.max = 300,
                                nstarts = seq(1, 10, 1),
                                trace = FALSE,
                                cores = 6)

#Visualize
png(filename = paste0(data_directory, dataset, "_kmeans_elbow.png"), width = 500, height = 350)
kmeans_elbow <- ggplot2::ggplot(data = kmeans_results %>% filter(nstarts == 10), aes(x = centers, y = withinss)) +
                ggplot2::geom_line() +
                ggplot2::geom_point() +
                ggplot2::ggtitle(label = paste0(dataset, " dataset K-Means Clustering:\ntotal within cluster sum of squares by number of clusters")) +
                ggplot2::xlab("Number of clusters") +
                ggplot2::ylab("Total within cluster sum of squares")
kmeans_elbow
dev.off()

#Clusters to choose
kmeans_choice <- kmeans_results %>%
                 dplyr::filter(nstarts == 10)
kmeans_choice <- max(which(kmeans_choice$withinss >= min(kmeans_choice$withinss)*1.1) + 1)

#Compare to labels
set.seed(28)
kmeans_labeling <- kmeans(x = data_all[, features], centers = dplyr::n_distinct(data_all[, response]), iter.max = 5000, nstart = 50, algorithm = "MacQueen", trace = FALSE)
data_all$kmc_cluster <- as.numeric(kmeans_labeling$cluster)
round(prop.table(table(data_all[, response], data_all$kmc_cluster), margin = 1)*100, 2)

#Principal Component Analysis (PCA) -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Data is already scaled at the beginning
set.seed(28)
princomp <- stats::prcomp(x = data_all[, features], scale = FALSE)
#princomp_train <- stats::prcomp(x = data_all_train[, features], scale = FALSE)
#princomp_test <- predict(princomp_train, data_all_test[, features])

#Get thte explained variance
princomp_explained_variance <- data.frame(proportion = princomp$sdev/sum(princomp$sdev))
#Get the cumulative explained variance
princomp_explained_variance$cumulative_proportion <- cumsum(princomp_explained_variance$proportion)
#Add a dimension for plotting purposes
princomp_explained_variance$dimension <- seq(1, nrow(princomp_explained_variance), 1)

#Visualize
#Proportion of variance explained
png(filename = paste0(data_directory, dataset, "_princomp_proportion_plot.png"), width = 500, height = 350)
princomp_proportion_plot <- ggplot2::ggplot(data = princomp_explained_variance, aes(x = dimension, y = proportion)) +
                            ggplot2::geom_line() +
                            ggplot2::geom_point() +
                            ggplot2::ggtitle(paste0(dataset, " dataset PCA:\nproportion of the variance explained by each principal component")) +
                            ggplot2::xlab("Principal component number") +
                            ggplot2::ylab("Proportion of the variance explained")
princomp_proportion_plot
dev.off()

#Cumulative variance explained
png(filename = paste0(data_directory, dataset, "_princomp_cumulative_plot.png"), width = 500, height = 350)
princomp_cumulative_plot <- ggplot2::ggplot(data = princomp_explained_variance, aes(x = dimension, y = cumulative_proportion)) +
                            ggplot2::geom_line() +
                            ggplot2::geom_point() +
                            ggplot2::ggtitle(paste0(dataset, " dataset PCA:\ncumulative proportion of the variance explained by the principal components")) +
                            ggplot2::xlab("Principal component number") +
                            ggplot2::ylab("Cumulative proportion of the variance explained")
princomp_cumulative_plot
dev.off()

#Choose the number of variables giving 90% of the variance
princomp_number <- which(princomp_explained_variance$cumulative_proportion >= 0.9)[1]
#Reduction in number of features
1-princomp_number/length(features)
#Take the first x variables
princomp_features <- data.frame(princomp$x[, 1:princomp_number],
                                response = data_all[, response],
                                tag = data_all$tag)
#princomp_features <- rbind(data.frame(princomp_train$x[, 1:princomp_number],
#                                      response = data_all_train[, response],
#                                      tag = data_all_train$tag),
#                           data.frame(princomp_test[, 1:princomp_number],
#                                     response = data_all_test[, response],
#                                     tag = data_all_test$tag))
#Output for NN
write.csv(princomp_features, paste0(data_directory, dataset, "_princomp_nn_input.csv"), row.names = FALSE)

#Repeat the kmeans clustering with the principal components
princomp_kmeans <- kmeans_search(data = princomp_features[, colnames(princomp$x)[1:princomp_number]],
                                 features = colnames(princomp$x)[1:princomp_number],
                                 algorithms = c("Hartigan-Wong"), #"Lloyd", "Forgy", "MacQueen"
                                 centers = seq(1, 15, 1),
                                 iter.max = 300,
                                 nstarts = seq(1, 10, 1),
                                 trace = FALSE,
                                 cores = 6)

#Visualize
png(filename = paste0(data_directory, dataset, "_princomp_kmeans_elbow.png"), width = 500, height = 350)
princomp_kmeans_elbow <- ggplot2::ggplot(data = princomp_kmeans %>% filter(nstarts == 10), aes(x = centers, y = withinss)) +
                         ggplot2::geom_line() +
                         ggplot2::geom_point() +
                         ggplot2::ggtitle(label = paste0(dataset, " K-Means Clustering with PCA output:\ntotal within cluster sum of squares by number of centers")) +
                         ggplot2::xlab("Number of clusters") +
                         ggplot2::ylab("Total within cluster sum of squares")
princomp_kmeans_elbow
dev.off()

#Clusters to choose
kmeans_choice <- princomp_kmeans %>%
  dplyr::filter(nstarts == 10)
kmeans_choice <- max(which(kmeans_choice$withinss >= min(kmeans_choice$withinss)*1.1) + 1)

#Compare to labels
set.seed(28)
princomp_labeling <- kmeans(x = princomp_features[, colnames(princomp$x)[1:princomp_number]], centers = dplyr::n_distinct(data_all[, response]), iter.max = 5000, nstart = 50, algorithm = "MacQueen", trace = FALSE)
data_all$princomp_cluster <- as.numeric(princomp_labeling$cluster)
round(prop.table(table(data_all[, response], data_all$princomp_cluster), margin = 1)*100, 2)

#Determine the number of clusters to use
neural_net_centers <- princomp_kmeans %>% dplyr::filter(nstarts == 10)
neural_net_centers <- min(neural_net_centers$centers[neural_net_centers$withinss <= min(neural_net_centers$withinss) * 1.10])

#Use cluster assignments as the input to the Neural Network
nn_kmeans_clustering <- kmeans(x = princomp_features[, colnames(princomp$x)[1:princomp_number]], centers = neural_net_centers, iter.max = 300, nstart = 10, algorithm = "Hartigan-Wong", trace = FALSE)
princomp_features$cluster <- as.numeric(nn_kmeans_clustering$cluster)
#Convert cluster to dummy variables
cluster_dummys <- c()
for (cluster in sort(unique(princomp_features$cluster))) {
  cluster_name <- paste0("cluster_", cluster)
  princomp_features[, cluster_name] <- ifelse(princomp_features$cluster == cluster, 1, 0)
  cluster_dummys <- c(cluster_dummys, cluster_name)
}

#Keep only the cluster dummys and the tag
princomp_features <- princomp_features[, c(cluster_dummys, "tag", "response")]
#Output to Python for modeling
write.csv(princomp_features, paste0(data_directory, dataset, "_princomp_nn_input_with_cluster.csv"), row.names = FALSE)
