# check: Check results from head-turn preference procedure training
# Gonzalo García-Castro
# gonzalo.garciadecastro@upf.edu
###############################################################
# This function compares the looking time measured in each trial by a trainee
# with those measured by an experienced experimenter. It results in a graph that allows
# a quick visual assessment of the trainee's performance.
###############################################################

check <-
  function(id, # baby identifier (as character vector)
           plot = TRUE,
           max.error = 0.10){ # should a graph be displayed?
    
    library(ggplot2)  # data visualization
    library(magrittr) # data manipulation
    library(viridis)  # colors

    ref.file <- list.files(pattern = paste(id, "_ref", sep = ""))
    train.file <- list.files(pattern = paste(id, "_train", sep = ""))
    infant <- sub("\\_ref.txt","", ref.file)

    col.classes <- c("numeric", "factor", "factor", "factor", "factor",
                     "numeric", "numeric", "numeric", "numeric", "character",
                     "character", "character", "factor", "numeric", "character", "character", "NULL")
    col.names <- c("trial", "phase", "item", "location", "block",
                   "lookingtime", "looksaway", "prelook", "postlook", "protocol",
                   "id", "tester", "gender", "age", "comments", "familiarization", "NULL")
    
    # import reference dataset (postprocessed WISP .txt file)
    ref.data <- read.delim(ref.file,
                           header = FALSE,
                           colClasses = col.classes,
                           col.names = col.names,
                           row.names = NULL,
                           blank.lines.skip = TRUE,
                           na.strings = c(" ", "NA", "\t", "  ", "-", ""),
                           skip = 2,
                           stringsAsFactors = TRUE)
    ref.data %<>% dplyr::filter(phase == 3) %>% tibble::as_tibble()

    # import training dataset (postprocessed WISP file)
    train.data <- read.delim(train.file,
                             header = FALSE,
                             colClasses = col.classes,
                             col.names = col.names,
                             row.names = NULL,
                             blank.lines.skip = TRUE,
                             na.strings = c(" ", "NA", "\t", "  ", "-", ""),
                             skip = 2,
                             stringsAsFactors = TRUE)
    train.data %<>% dplyr::filter(phase == 3) %>% tibble::as_tibble()
    
    # calculate error
    time <- tibble::tibble(trial = ref.data %$% trial,
                           reference = ref.data %$% lookingtime,
                           train = train.data %$% lookingtime) %>%
      dplyr::mutate(error = reference - train)
      
    time.long <- time %>%
      tidyr::gather(key = "dataset", value = "time") %>%
      dplyr::filter(dataset == "reference" | dataset == "train") %>%
      tibble::add_column(trial = time %$% trial %>%  rep(., 2),
                         error = time %$% error %>% rep(., 2))

    time %>% print()
    accuracy <- round(sum(time$train)/sum(time$reference), 2)
    performance <- paste(id, 
                         "Mean error =", time %$% error %>% mean %>% round(., 2), "ms,",
                         "accuracy =", accuracy,
                         sep = " ")
    performance %>% print()
    terrors <- tibble::tibble(trial = time$trial, error = time$error)
      
    assign("time.long", time.long, envir = .GlobalEnv)
    assign("time", time, envir = .GlobalEnv)

    # plot looking time measured against trial number
    # comparing train vs. reference
    
    if(plot == TRUE) {
      
      gerror <-
        ggplot(data = time, aes(trial, rep(1, max(trial)), fill = abs(error))) +
        geom_tile(show.legend = FALSE) +
        labs(x = "Trial", y = "Error") +
        scale_fill_viridis() +
        scale_x_continuous(breaks = seq(1, max(time$trial), 1)) +
        theme_minimal() +
        theme(panel.grid = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank())
      
      graph <- 
        ggplot(data = time.long) +
        geom_line(aes(x = trial, y = time, linetype = dataset), size = 1, alpha = 0.8, show.legend = TRUE) +
        geom_point(aes(x = trial, y = time, shape = dataset), size = 3) +
        geom_text(data = time, aes(x = trial, y = 0, label = error), size = 3) +
        labs(title = paste("Mean error = ", time.long %$% mean(error) %>% round(., 2), "ms,",
                           "SD error = ", time.long %$% sd(error) %>% round(., 2), "ms",
                           sep = " "),
             x = "Trial",
             y = "Looking time (ms)",
             color = "Coder",
             caption = "Error = reference time - train time") +
        scale_x_continuous(breaks = seq(1, max(time$trial), 1)) +
        scale_y_continuous(limits = c(-2000, 20000)) +
        theme_minimal() +
        theme(plot.title = element_text(size = 10),
              axis.text.x = element_text(size = 10),
              panel.grid = element_blank(),
              axis.text.y = element_text(size = 12, color = "Black"),
              legend.position = "top") +
        cowplot::draw_plot(gerror, x = 0, y = -2000, height = 2000, width = max(time$trial)+1.25)
      print(graph)
    }
    noice <- c("Congratulations!", "Nice job!", "Awesome!", "Well done!")
    bad <- c("Try again!", "Don't give up!", "You can do better...")
    if (accuracy %in% seq(1-max.error, 1+max.error, by = 0.001)){
      print(sample(noice,1 ))
    } else if (accuracy %!in% seq(1-max.error, 1+max.error, by = 0.001)){
      print(sample(bad, 1))
    }
  }
