#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Summary: this script loads the csv output from the Jupyter Notebooks and outputs plots for the report
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Load libraries -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
library(dplyr)
library(ggplot2)

#Set directory -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
directory <- "/Users/mikepecorino/Documents/machine_learning/HW2/"

#Source functions -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Random Hill Climbing
random_opt_rhc_plot <- function(df, problem_name, plot_by) {
  
  #For output
  problem_name_output <- case_when(problem_name == "four_peaks" ~ "Four Peaks",
                                   problem_name == "knapsack" ~ "Knapsack",
                                   problem_name == "flip_flop" ~ "Flip Flop")
  
  #Plot by name
  plot_by_name_output <- case_when(plot_by == "max_iter" ~ "max iterations",
                            plot_by == "function_evaluations" ~ "function evaluations")
  
  #Plot by variable
  df$plot_by_var <- df[, plot_by]

  #Take observations with the max random restarts
  df_max <- df %>%
            filter(problem == problem_name,
                   random_restart == max(random_restart)) %>%
            rename(fitness = best_fitness)

  #Plot fitness by number of attempts
  plot_data <- ggplot(data = df_max, aes(x = plot_by_var, y = fitness)) +
                      geom_line() +
                      ggtitle(paste0(problem_name_output, " using RHC, fitness over ", plot_by_name_output)) +
                      xlab(plot_by)
  
  return(plot_data)
}

#Simulated Annealing
random_opt_sa_plot <- function(df, problem_name, plot_by, color_by) {
  
  #For output
  problem_name_output <- case_when(problem_name == "four_peaks" ~ "Four Peaks",
                                   problem_name == "knapsack" ~ "Knapsack",
                                   problem_name == "flip_flop" ~ "Flip Flop")
  
  #Plot by name
  plot_by_name_output <- case_when(plot_by == "max_iter" ~ "max iterations",
                                   plot_by == "function_evaluations" ~ "function evaluations")
  
  #Plot by variable
  df$plot_by_var <- df[, plot_by]
  #Color by variable
  df[, color_by] <- as.factor(df[, color_by])
  
  #Take observations with the max random restarts
  df_max <- df %>%
    filter(problem == problem_name) %>%
    rename(fitness = best_fitness)
  
  #Plot fitness by number of attempts
  plot_data <- ggplot(data = df_max, aes(x = plot_by_var, y = fitness)) +
    geom_line(aes_string(color = color_by)) +
    ggtitle(paste0(problem_name_output, " using SA, fitness over ", plot_by_name_output)) +
    xlab(plot_by)
  
  return(plot_data)
}

#Genetic Algorithm
random_opt_ga_plot <- function(df, problem_name, plot_by, color_by) {
  
  #For output
  problem_name_output <- case_when(problem_name == "four_peaks" ~ "Four Peaks",
                                   problem_name == "knapsack" ~ "Knapsack",
                                   problem_name == "flip_flop" ~ "Flip Flop")
  
  #Plot by name
  plot_by_name_output <- case_when(plot_by == "max_iter" ~ "max iterations",
                                   plot_by == "function_evaluations" ~ "function evaluations")
  
  #Plot by variable
  df$plot_by_var <- df[, plot_by]
  #Color by variable
  df[, color_by] <- as.factor(df[, color_by])
  
  #Take observations with the max random restarts
  df_max <- df %>%
    filter(problem == problem_name,
           pop_size == max(pop_size)) %>%
    rename(fitness = best_fitness)
  
  #Plot fitness by number of attempts
  plot_data <- ggplot(data = df_max, aes(x = plot_by_var, y = fitness)) +
    geom_line(aes_string(color = color_by)) +
    ggtitle(paste0(problem_name_output, " using GA, fitness over ", plot_by_name_output)) +
    xlab(plot_by)
  
  return(plot_data)
}

#Mimic
random_opt_mim_plot <- function(df, problem_name, plot_by, color_by) {
  
  #For output
  problem_name_output <- case_when(problem_name == "four_peaks" ~ "Four Peaks",
                                   problem_name == "knapsack" ~ "Knapsack",
                                   problem_name == "flip_flop" ~ "Flip Flop")
  
  #Plot by name
  plot_by_name_output <- case_when(plot_by == "max_iter" ~ "max iterations",
                                   plot_by == "function_evaluations" ~ "function evaluations")
  
  #Plot by variable
  df$plot_by_var <- df[, plot_by]
  #Color by variable
  df[, color_by] <- as.factor(df[, color_by])
  
  #Take observations with the max random restarts
  df_max <- df %>%
    filter(problem == problem_name) %>%
    rename(fitness = best_fitness)
  
  #Plot fitness by number of attempts
  plot_data <- ggplot(data = df_max, aes(x = plot_by_var, y = fitness)) +
    geom_line(aes_string(color = color_by)) +
    ggtitle(paste0(problem_name_output, " using MIMIC, fitness over ", plot_by_name_output)) +
    xlab(plot_by)
  
  return(plot_data)
}

#Get df of best results
best_results_all_algos <- function(rhc, sa, ga, mim, random_restart_value, init_temp_value, pop_size_ga_value, mutation_prob_value, pop_size_mim_value, keep_pct_value) {
  
  rhc_best <- rhc %>%
    filter(random_restart == random_restart_value) %>%
    dplyr::select(problem, algorithm, max_iter, best_fitness, function_evaluations, time) %>%
    dplyr::arrange(max_iter)
  
  #Simulated Annealing take the maximum temperature
  sa_best <- sa %>%
    filter(init_temp == init_temp_value) %>%
    dplyr::select(problem, algorithm, max_iter, best_fitness, function_evaluations, time) %>%
    dplyr::arrange(max_iter)
  
  #Genetic Algorithm take the maximum population size and maximum keep pct
  ga_best <- ga %>%
    filter(pop_size == pop_size_ga_value,
           mutation_prob == mutation_prob_value) %>%
    dplyr::select(problem, algorithm, max_iter, best_fitness, function_evaluations, time) %>%
    dplyr::arrange(max_iter)
  
  #Genetic Algorithm take the maximum population size and maximum keep pct
  mim_best <- mim %>%
    filter(pop_size == pop_size_mim_value,
           keep_pct == keep_pct_value) %>%
    dplyr::select(problem, algorithm, max_iter, best_fitness, function_evaluations, time) %>%
    dplyr::arrange(max_iter)
  
  #Combine
  all_algos <- rbind(rhc_best, sa_best, ga_best, mim_best)
  
  return(all_algos)
  
}

#Plot by problem type
random_opt_problem_plot <- function(df, problem_name, plot_by, color_by) {
  
  #For output
  problem_name_output <- case_when(problem_name == "four_peaks" ~ "Four Peaks",
                                   problem_name == "knapsack" ~ "Knapsack",
                                   problem_name == "flip_flop" ~ "Flip Flop")
  
  #Plot by name
  plot_by_name_output <- case_when(plot_by == "max_iter" ~ "max iterations",
                                   plot_by == "function_evaluations" ~ "function evaluations")
  
  #Plot by variable
  df$plot_by_var <- df[, plot_by]
  #Color by variable
  df[, color_by] <- as.factor(df[, color_by])
  
  #Take observations with the max random restarts
  df_max <- df %>%
    filter(problem == problem_name) %>%
    rename(fitness = best_fitness)
  
  #Plot fitness by number of attempts
  plot_data <- ggplot(data = df_max, aes(x = plot_by_var, y = fitness)) +
    geom_line(aes_string(color = color_by)) +
    ggtitle(paste0(problem_name_output, ", all algorithms fitness over ", plot_by_name_output)) +
    xlab(plot_by)
  
  return(plot_data)
}

#Plot the Neural Net results
#Plot by problem type
random_opt_neural_net_plot <- function(df, plot_by, color_by) {
  
  #Plot by name
  plot_by_name_output <- case_when(plot_by == "max_iter" ~ "max iterations",
                                   plot_by == "function_evaluations" ~ "function evaluations")
  
  #Plot by variable
  df$plot_by_var <- df[, plot_by]
  #Color by variable
  df[, color_by] <- as.factor(df[, color_by])
  
  #Plot fitness by number of attempts
  plot_data <- ggplot(data = df, aes(x = plot_by_var, y = test_score)) +
    geom_line(aes_string(color = color_by)) +
    ggtitle(paste0("Randomized Optimization performance on the SENSOR Neural Network by ", plot_by_name_output)) +
               xlab(plot_by)
  
  return(plot_data)
}

#Load inputs -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Four peaks
four_peaks_rhc <- read.csv(paste0(directory, "four_peaks/four_peaks_rhc.csv"))
four_peaks_sa <- read.csv(paste0(directory, "four_peaks/four_peaks_sa.csv"))
four_peaks_ga <- read.csv(paste0(directory, "four_peaks/four_peaks_ga.csv"))
four_peaks_mim <- read.csv(paste0(directory, "four_peaks/four_peaks_mim.csv"))

#Knapsack
knapsack_rhc <- read.csv(paste0(directory, "knapsack/knapsack_rhc.csv"))
knapsack_sa <- read.csv(paste0(directory, "knapsack/knapsack_sa.csv"))
knapsack_ga <- read.csv(paste0(directory, "knapsack/knapsack_ga.csv"))
knapsack_mim <- read.csv(paste0(directory, "knapsack/knapsack_mim.csv"))

#Flip flop
flip_flop_rhc <- read.csv(paste0(directory, "flip_flop/flip_flop_rhc.csv"))
flip_flop_sa <- read.csv(paste0(directory, "flip_flop/flip_flop_sa.csv"))
flip_flop_ga <- read.csv(paste0(directory, "flip_flop/flip_flop_ga.csv"))
flip_flop_mim <- read.csv(paste0(directory, "flip_flop/flip_flop_mim.csv"))

#One Max
one_max_rhc <- read.csv(paste0(directory, "one_max/one_max_rhc.csv"))
one_max_sa <- read.csv(paste0(directory, "one_max/one_max_sa.csv"))
one_max_ga <- read.csv(paste0(directory, "one_max/one_max_ga.csv"))
one_max_mim <- read.csv(paste0(directory, "one_max/one_max_mim.csv"))

#Nural Network
mlrose_nn <- read.csv(paste0(directory, "sensor_randomized_opt_neural_net.csv"))

#Add problem type -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
four_peaks_rhc$problem <- "four_peaks"
four_peaks_sa$problem <- "four_peaks"
four_peaks_ga$problem <- "four_peaks"
four_peaks_mim$problem <- "four_peaks"

knapsack_rhc$problem <- "knapsack"
knapsack_sa$problem <- "knapsack"
knapsack_ga$problem <- "knapsack"
knapsack_mim$problem <- "knapsack"

flip_flop_rhc$problem <- "flip_flop"
flip_flop_sa$problem <- "flip_flop"
flip_flop_ga$problem <- "flip_flop"
flip_flop_mim$problem <- "flip_flop"

one_max_rhc$problem <- "one_max"
one_max_sa$problem <- "one_max"
one_max_ga$problem <- "one_max"
one_max_mim$problem <- "one_max"

#Combine data by algorithm type
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
rhc <- rbind(four_peaks_rhc, knapsack_rhc, flip_flop_rhc, one_max_rhc)
sa <- rbind(four_peaks_sa, knapsack_sa, flip_flop_sa, one_max_sa)
ga <- rbind(four_peaks_ga, knapsack_ga, flip_flop_ga, one_max_ga)
mim <- rbind(four_peaks_mim, knapsack_mim, flip_flop_mim, one_max_mim)

#Plot by algorithm type -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Random Hill Climbing
algorithm <- "rhc"
plot_by <- "max_iter"

problem <- "four_peaks"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_rhc_plot(df = rhc, problem_name = problem, plot_by = plot_by)
dev.off()

problem <- "knapsack"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_rhc_plot(df = rhc, problem_name = problem, plot_by = plot_by)
dev.off()

problem <- "flip_flop"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_rhc_plot(df = rhc, problem_name = problem, plot_by = plot_by)
dev.off()

problem <- "one_max"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_rhc_plot(df = rhc, problem_name = problem, plot_by = plot_by)
dev.off()

#Simulated Annealing
algorithm <- "sa"
plot_by <- "max_iter"
color_by <- "init_temp"

problem <- "four_peaks"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_sa_plot(df = sa, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "knapsack"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_sa_plot(df = sa, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "flip_flop"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_sa_plot(df = sa, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "one_max"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_sa_plot(df = sa, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

#Genetic Algorithm
algorithm <- "ga"
plot_by <- "max_iter"
color_by <- "mutation_prob"

problem <- "four_peaks"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_ga_plot(df = ga, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "knapsack"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_ga_plot(df = ga, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "flip_flop"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_ga_plot(df = ga, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "one_max"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_ga_plot(df = ga, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

#Mimic
algorithm <- "mim"
plot_by <- "max_iter"
color_by <- "keep_pct"

problem <- "four_peaks"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_mim_plot(df = mim, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "knapsack"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_mim_plot(df = mim, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "flip_flop"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_mim_plot(df = mim, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "one_max"
png(filename = paste0(directory, problem, "_", algorithm, "_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_mim_plot(df = mim, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

#Plot the best fitness from each algorithm -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#Four peaks
all_algos_four_peaks <- best_results_all_algos(rhc = rhc, sa = sa, ga = ga, mim = mim, random_restart_value = 100, init_temp_value = 10, pop_size_ga_value = 300, mutation_prob_value = 0.1, pop_size_mim_value = 100, keep_pct_value = 0.2)
all_algos_knapsack <- best_results_all_algos(rhc = rhc, sa = sa, ga = ga, mim = mim, random_restart_value = 100, init_temp_value = 200, pop_size_ga_value = 100, mutation_prob_value = 0.2, pop_size_mim_value = 100, keep_pct_value = 0.3)
all_algos_flip_flop <- best_results_all_algos(rhc = rhc, sa = sa, ga = ga, mim = mim, random_restart_value = 100, init_temp_value = 10, pop_size_ga_value = 300, mutation_prob_value = 0.3, pop_size_mim_value = 100, keep_pct_value = 0.2)

#Plot by problem type -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
color_by <- "algorithm"

#Plot by max iterations
plot_by <- "max_iter"

problem <- "four_peaks"
png(filename = paste0(directory, problem, "_all_algos_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_problem_plot(df = all_algos_four_peaks, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "knapsack"
png(filename = paste0(directory, problem, "_all_algos_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_problem_plot(df = all_algos_knapsack, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "flip_flop"
png(filename = paste0(directory, problem, "_all_algos_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_problem_plot(df = all_algos_flip_flop, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "one_max"
png(filename = paste0(directory, problem, "_all_algos_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_problem_plot(df = all_algos_one_max, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

#Plot by function evaluations
plot_by <- "function_evaluations"

problem <- "four_peaks"
png(filename = paste0(directory, problem, "_all_algos_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_problem_plot(df = all_algos_four_peaks, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "knapsack"
png(filename = paste0(directory, problem, "_all_algos_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_problem_plot(df = all_algos_knapsack, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "four_peaks"
png(filename = paste0(directory, problem, "_all_algos_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_problem_plot(df = all_algos_flip_flop, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

problem <- "one_max"
png(filename = paste0(directory, problem, "_all_algos_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_problem_plot(df = all_algos, problem_name = problem, plot_by = plot_by, color_by = color_by)
dev.off()

#Plot the Neural Net results -----
#- - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
color_by <- "algorithm"

#Plot by max iteration
plot_by <- "max_iter"
png(filename = paste0(directory, "neural_network_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_neural_net_plot(df = mlrose_nn, plot_by = plot_by, color_by = color_by)
dev.off()

#Plot by time
plot_by <- "function_evaluations"
png(filename = paste0(directory, "neural_network_by_", plot_by, ".png"), height = 480*.6, width = 480*1.3)
random_opt_neural_net_plot(df = mlrose_nn, plot_by = plot_by, color_by = color_by)
dev.off()
