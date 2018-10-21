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
           graph = TRUE){ # should a graph be displayed?
    
    library(ggplot2) # data visualization
    library(magrittr) # data manipulation

    ref.files <- list.files(pattern = "_ref")
    train.files <- list.files(pattern = "_train")
    
    infants <- sub("\\_ref.txt","", ref.files)
    n <- length(id) # number of datasets to check
    
    col.classes <- c("numeric", "factor", "factor", "factor", "factor",
                     "numeric", "numeric", "numeric", "numeric", "character",
                     "character", "character", "factor", "numeric", "character", "character", "NULL")
    col.names <- c("trial", "phase", "item", "location", "block",
                   "lookingtime", "looksaway", "prelook", "postlook", "protocol",
                   "id", "tester", "gender", "age", "comments", "familiarization", "NULL")
    
    # import reference dataset (postprocessed WISP .txt file)
    ref.data <- lapply(ref.files,
                       read.delim,
                       header = FALSE,
                       colClasses = col.classes,
                       col.names = col.names,
                       row.names = NULL,
                       blank.lines.skip = TRUE,
                       na.strings = c(" ", "NA", "\t", "  ", "-", ""),
                       skip = 2,
                       stringsAsFactors = TRUE)
    for (i in 1:n) ref.data[[i]] %<>% dplyr::filter(phase == 3) %>% tibble::as_tibble()

    # import training dataset (postprocessed WISP file)
    train.data <- lapply(train.files,
                         read.delim,
                         header = FALSE,
                         colClasses = col.classes,
                         col.names = col.names,
                         row.names = NULL,
                         blank.lines.skip = TRUE,
                         na.strings = c(" ", "NA", "\t", "  ", "-", ""),
                         skip = 2,
                         stringsAsFactors = TRUE)
    for (i in 1:n) train.data[[i]] %<>% dplyr::filter(phase == 3) %>% tibble::as_tibble()
    
    # calculate error
    time <- list()
    time.long <- list()
    
    for (i in 1:n){
      time[[i]] <- tibble::tibble(trial = ref.data[[i]] %$% trial,
                                  reference = ref.data[[i]] %$% lookingtime,
                                  train = train.data[[i]] %$% lookingtime) %>%
        dplyr::mutate(error = reference - train)
      
      time.long[[i]] <- time[[i]] %>%
        tidyr::gather(key = "dataset", value = "time") %>%
        dplyr::filter(dataset == "reference" | dataset == "train") %>%
        tibble::add_column(trial = time[[i]] %$% trial %>%  rep(., 2),
                           error = time[[i]] %$% error %>% rep(., 2))
      
    }
    
    time %>% print()
    
    performance <- list()
    for (i in 1:n){
      performance[[i]] <- paste(id[[i]], 
                                "Mean error =",
                                time[[i]] %$% error %>% mean %>% round(., 2),
                                "ms,",
                                "accuracy =",
                                round(sum(time[[i]]$train)/sum(time[[i]]$reference), 2),
                                sep = " ")}
    performance %>% print()

    # plot looking time measured against trial number
    # comparing train vs. reference
    if(graph){
      graph <- list()
      for (i in 1:n){
        if (i == n & n > 1){ # if last graph, display x-axis title
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
        else if (i == 1){
          if (i == n & n > 1){ # if last graph, display x-axis title
            graph <- list()
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
                         show.legend = TRUE) +
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
              theme(panel.grid.minor = element_blank(),
                    plot.title = element_text(size = 10),
                    axis.title.x = element_blank()) +
              theme_minimal() +
              scale_x_discrete("Trial")
        }
        else if (i == 1){ # if first graph, display legend
          graph <- list()
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
          graph <- list()
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
      do.call(gridExtra::grid.arrange, c(graph, list(ncol = 1)))
    }
  }
  }
