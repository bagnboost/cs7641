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

#Random Forest -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rf_formula <- as.formula(paste(response, paste(features, collapse = "+"), sep = "~"))
max.depth <- ceiling(length(features)^(1/4))
rf_model <- ranger::ranger(formula = rf_formula, data = data_all[, c(features, response)], num.trees = 1000, mtry = sqrt(length(features)), replace = TRUE, max.depth = max.depth, min.node.size = 5, importance = "impurity", seed = 28, num.threads = 6)
rf_importance <- ranger::importance(x = rf_model)
rf_importance_df <- data.frame(var = names(rf_importance),
                               importance = rf_importance) %>%
                    dplyr::arrange(desc(importance)) %>%
                    dplyr::mutate(var_number = seq_along(var),
                                  cumulative_importance = cumsum(importance),
                                  proportion_of_total = cumulative_importance/sum(importance))
#Select only those variables that explain 95% of the information
rf_important_variables <- sort(rf_importance_df$var[rf_importance_df$proportion_of_total <= 0.95])
length(rf_important_variables)
1 - length(rf_important_variables)/length(features)
write.csv(rf_important_variables, paste0(data_directory, dataset, "_rf_important_variables.csv"), row.names = FALSE)

#Visualize
png(filename = paste0(data_directory, dataset, "_rf_importance.png"), width = 500, height = 350)
rf_plot <- ggplot2::ggplot(data = rf_importance_df, aes(x = var_number, y = proportion_of_total)) +
                    ggplot2::geom_line() +
                    ggplot2::geom_point() +
                    ggplot2::ggtitle(paste0(dataset, " dataset cumulative relative variable importance\naccording to Random Forest model")) +
                    ggplot2::xlab("Number of features (sorted by importance)") +
                    ggplot2::ylab("Cumulative relative importance")
rf_plot
dev.off()
