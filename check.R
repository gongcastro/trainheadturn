# check: Check results from head-turn preference procedure training
# Gonzalo García-Castro
# gonzalo.garciadecastro@estudiant.upf.edu
###############################################################
# This function compares the looking time measured in each trial by a trainee
# with those measured by an experienced experimenter. It results in a graph that allows
# a quick visual assessment of the performance of the trainee.
###############################################################

check <-
  function(id, # baby/babies identifier/s (as character vector)
           graph = TRUE){
    
    library(ggplot2) # data visualization
    library(magrittr) # data manipulation
    library(gridExtra) # data visualization
    
    
    n <- length(id) # number of datasets to check
    print(n)
    
    # import training dataset (postprocessed WISP .txt file)
    train.data <- list()
    colClasses <- c("numeric", "factor", "factor", "factor", "factor",
                    "numeric", "numeric", "numeric", "numeric", "character",
                    "character", "character", "factor", "numeric", "character", "character", "NULL")
    col.names <- c("trial", "phase", "item", "location", "block",
                   "lookingtime", "looksaway", "prelook", "postlook", "protocol",
                   "id", "tester", "gender", "age", "comments", "familiarization", "NULL")
    for (i in 1:n){
      train.data[[i]] <-
        paste(id[i], ".txt", sep = "") %>%
        read.delim(., header = FALSE,
                   sep = "\t",
                   na.strings = " ",
                   skip = 2,
                   colClasses = colClasses,
                   col.names = col.names) %>%
        dplyr::filter(phase == 3)
    }
    
    # store the training dataset in the global environment as "train"
    assign("training_data.txt", train.data, envir = .GlobalEnv)
    
    # import training dataset (postprocessed WISP file)
    reference.data <- list()
    
    for (i in 1:n){
      reference.data[[i]] <-
        paste(id[i], "_ref.txt", sep = "") %>%
        read.delim(., header = FALSE,
                   sep = "\t",
                   na.strings = " ",
                   skip = 2,
                   colClasses = colClasses,
                   col.names = col.names) %>%
        dplyr::filter(phase == 3)
    }
      
    # store the reference dataset in the global environment as "reference"
    assign("reference_data.txt", reference.data, envir = .GlobalEnv)
    
    # calculate error
    time <- list()
    time.long <- list()
    
    for (i in 1:n){
      time[[i]] <-
        data.frame(trial = reference.data[[i]] %$% trial,
                   reference = reference.data[[i]] %$% lookingtime,
                   train = train.data[[i]] %$% lookingtime,
                   error = reference.data[[i]]$lookingtime - train.data[[i]]$lookingtime) 
      time.long[[i]] <-
        time[[i]] %>% tidyr::gather() %>% dplyr::filter(key == "reference" | key == "train")
      time.long[[i]]$trial <- time[[i]] %$% trial %>%  rep(., 2)
      time.long[[i]]$error <- time[[i]] %$% error %>% rep(., 2)
      colnames(time.long[[i]]) <- c("dataset", "time", "trial", "error")
      
      }
    "time" %>% assign(., time, envir = .GlobalEnv)
    "time.long" %>% assign(., time.long, envir = .GlobalEnv)
    
    # plot looking time measured against trial number
    # comparing train vs. reference
    if(graph){
      graph <- list()
      for (i in 1:n){
        if (i == n){ # if last graph, display x-axis title
          graph[[i]] <- 
            ggplot() +
            geom_line(data = time.long[[i]],
                      aes(x = trial,
                          y = time,
                          color = dataset),
                      size = 2,
                      alpha = 0.5,
                      show.legend = FALSE) +
            geom_point(data = time.long[[i]],
                       aes(x = trial,
                           y = time,
                           color = dataset),
                       size = 2,
                       alpha = 0.5,
                       show.legend = FALSE) +
            geom_text(data = time.long[[i]][1:(nrow(time.long[[i]])/2),],
                      aes(x = trial,
                          y = time,
                          label = error),
                      vjust = "inward",
                      size = 3) +
            labs(title = paste(paste(id[[i]], ":", sep = ""),
                               "Mean error = ", 
                               time.long[[i]] %$% mean(error) %>% round(., 2),
                               "ms,",
                               "SD error = ",
                               time.long[[i]] %$% sd(error) %>% round(., 2),
                               "ms",
                               sep = " "),
                 x = "Trial",
                 y = "Looking time (ms)",
                 caption = "Error = reference time - train time") +
            theme(plot.title = element_text(size = 10),
                  panel.grid.minor = element_blank(),
                  axis.title.x = element_blank()) +
            theme_minimal() +
            scale_x_discrete("Trial") 
          
        }
        
        else if (i == 1){ # if first graph, display legend
          graph[[i]] <- 
            ggplot(data = time.long[[i]]) +
            geom_line(data = time.long[[i]],
                      aes(x = trial,
                          y = time,
                          color = dataset),
                      size = 2,
                      alpha = 0.5) +
            geom_point(data = time.long[[i]],
                       aes(x = trial,
                           y = time,
                           color = dataset),
                       size = 2,
                       alpha = 0.5) +
            geom_text(data = time.long[[i]][1:(nrow(time.long[[i]])/2),],
                      aes(x = trial,
                          y = time,
                          label = error),
                      vjust = "inward",
                      size = 3) +
            labs(title = paste(paste(id[[i]], ":", sep = ""),
                               "Mean error = ", 
                               time.long[[i]] %$% mean(error) %>% round(., 2),
                               "ms,",
                               "SD error = ",
                               time.long[[i]] %$% sd(error) %>% round(., 2),
                               "ms",
                               sep = " "),
                 y = "Looking time (ms)") +
            theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(size = 10),
                  legend.position = "top") +
            scale_x_discrete(NULL)  
        }
        else{ # if rest of graphs
          graph[[i]] <- 
            ggplot(data = time.long[[i]]) +
            geom_line(aes(x = trial,
                          y = time,
                          color = dataset),
                      size = 2,
                      alpha = 0.5,
                      show.legend = FALSE) +
            geom_point(aes(x = trial,
                           y = time,
                           color = dataset),
                       size = 2,
                       alpha = 0.5,
                       show.legend = FALSE) +
            geom_text(data = time.long[[i]][1:(nrow(time.long[[i]])/2),],
                      aes(x = trial,
                          y = time,
                          label = error),
                      vjust = "inward",
                      size = 3) +
            labs(title = paste(paste(id[[i]], ":", sep = ""),
                               "Mean error = ", 
                               time.long[[i]] %$% mean(error) %>% round(., 2),
                               "ms,",
                               "SD error = ",
                               time.long[[i]] %$% sd(error) %>% round(., 2),
                               "ms",
                               sep = " "),
                 y = "Looking time (ms)") +
            theme_minimal() +
            theme(panel.grid.minor = element_blank(),
                  plot.title = element_text(size = 10)) +
            scale_x_discrete(NULL)
        }
      }
    }
      do.call(grid.arrange, c(graph, list(ncol = 1)))
  }
        