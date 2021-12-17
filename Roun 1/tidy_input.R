# This will contain all the necessary functions and libraries for the exec_script
# to run

# Libraries --------------------------------------------------------------------
library(tidyverse) # all manipulation
library(stringr) # To handle filenames
library(TTR) # Moving average
library(changepoint) # To determine changepoints
library(cowplot) #Built over ggplot2 provides grid plot and useful save plot function
library(moments) # For Kurtosis and skewness

## Style - Use df as input and df1 as output in a function
# Data import ------
get_exp_data <- function(){
  # Job    - It reads data from csv and creates a new exp_data list
  
  exp_data <- list()
  
  for(i in 1:nExp){
    exp_data[[i]] <- read_csv(str_c("exp",i,".csv"), 
                              col_names = FALSE)
    names(exp_data[[i]]) <- c("t", "Fx", "Fy", "Fz", "Mx", "My", "Mz")
  }
  exp_data
}

# Data wrangling ---

cal_Fr <- function(df){
  # Calculate resultant force of Fx and Fy
  # and stores in Fr
  df1 <- df %>% mutate(Fr = sqrt(Fx^2+Fy^2)) 
}


get_comp <- function(df, comp){
  # For a dataframe it selects t, component
  # Mention comp with column name.
  # String is not necessary
  comp <- enquo(comp)
  df1 <- df %>% select(t, !!comp)
}


get_zone <- function(df, start, end){
  #Slices the data frame using start and end as limits
  df[start:end, ]
}

# Extracting machining zone ----


get_sma_vector <- function(df){
  # Input - Fz data frame
  # Output - Smoothed moving average vector
  sma_vec <- SMA(df$Fz, n = 20)
  sma_vec[is.na(sma_vec)] <- 0
  sma_vec
}

get_changepoints <- function(df){
  # Input - Fz data frame
  # Output - Index of change points
  sma_vec <- get_sma_vector(df);
  cp <- cpts(
    cpt.mean(sma_vec, method = "PELT",
             penalty = "Manual", pen.value = 1000000)
  )
}
#Take a slice and also add frame number
get_slice <- function(start, end, frame, df){
  df1 <- df %>%
    slice(., start:end) %>%
    mutate(Frame = frame)
  df1
}

get_mc_data <- function(df, cp){
  #Input: Data frame and change point vector
  #Output: Machining zone data
  start_p <- seq(1, length(cp) - 1, by = 2)
  end_p <- start_p + 1
  start_h <- cp[start_p]
  end_h <- cp[end_p] - 20
  fr_num <- 1:length(start_h)
  trial <- pmap(list(start_h, end_h, fr_num), ~get_slice(..1, ..2, ..3, df))
  
  df1 <-bind_rows(trial)
}


get_mc_data_entry <- function(df, cp){
  #Input: Data frame and change point vector
  #Output: Machining zone data
  start_p <- seq(1, length(cp) - 1, by = 2)
  end_p <- start_p + 1
  start_h <- cp[start_p]
  end_h <- cp[end_p]
  length_fr <- end_h - start_h
  
  # Finding out entry and continuosu zones
  start_entry <-  start_h
  end_entry <- start_h + floor(0.2 * length_fr)
  
  fr_num <- 1:length(start_h)
  
  trial <- pmap(list(start_entry, end_entry, fr_num), ~get_slice(..1, ..2, ..3, df))
  
  df1 <-bind_rows(trial)
}

get_mc_data_steady <- function(df, cp){
  #Input: Data frame and change point vector
  #Output: Machining zone data
  start_p <- seq(1, length(cp) - 1, by = 2)
  end_p <- start_p + 1
  start_h <- cp[start_p]
  end_h <- cp[end_p]
  length_fr <- end_h - start_h
  
  # Finding out entry and continuosu zones
  start_cont <- start_h + floor(0.2 * length_fr)
  end_cont <- start_h + floor(0.8 * length_fr)
  
  fr_num <- 1:length(start_h)
  
  trial <- pmap(list(start_cont, end_cont, fr_num), ~get_slice(..1, ..2, ..3, df))
  
  df1 <-bind_rows(trial)
}

# Statistical calculations ----
## Uses dplyr function approach
frame_stat <- function(df, stat_func, F_comp){
  F_comp <- enquo(F_comp)
  df1 <- df %>% 
    group_by(Frame) %>%
    summarize(stat = stat_func(!!F_comp))
}

full_stat <- function(df, stat_func, F_comp){
  #Use this only for debugging
  # df1 <- stat_func(df$Fr)
  F_comp <- enquo(F_comp)
  stat_func <- enquo(stat_func)
  df1 <- df %>%
    select(!!F_comp) %>%
    summarize_all(stat_func) %>%
    as.numeric
} 
# The range function returns a vector of max and min values,
# so writing a small help function
calc_range <- function(x){
  # x is a vector
  r <- max(x) - min(x)
}

trend  <- function(df){
  x <- select(df, stat, Frame)
  k <- coef(lm(x))
  k[[2]]
} 
# Plotting ----

find_range <- function(ilist,col_num){
  # Helper function for stat_plot
  rlist <- list(c(NA,NA))
  for(i in 1:nExp){
    rlist[[i]] <- range(ilist[[i]][,col_num])
  }
  range(rlist)
}

make_frames_as_factors <- function(ilist){
  # Input  - List with frame numbers and Component data
  # Output - Suitable for developing violin plots 
  # Job    - Arranges data as (Frame_number, Fz and then converts Frame_number
  #          into a factor)
  olist <- list()
  for(i in 1:nExp){
    olist[[i]] <- ilist[[i]] %>%
      select(Frame, Fr) %>%
      mutate(Frame = as.factor(Frame))
  }
  olist
}

stat_plot <- function(ilist, xlabel, ylabel){
  #Plot all the graphs in a single page and save it 
  axis_name <- names(ilist[[1]])
  x_scale <- scale_x_continuous(limits = find_range(ilist, 1))
  y_scale <- scale_y_continuous(limits = find_range(ilist, 2))
  
  plot_list <- list()
  for(i in 1:nExp){
    plot_list[[i]]<- ggplot(ilist[[i]],
                            aes_string(x = axis_name[1], y = axis_name[2])) +
      geom_point()+
      xlab(xlabel)+ylab(ylabel)+
      x_scale +
      y_scale
  }
  p <- plot_grid(plotlist = plot_list, labels = "AUTO",ncol = 2)
  
  p
}

plot_violin <- function(ilist){
  
  axis_name <- names(ilist[[1]])
  y_scale <- scale_y_continuous(limits = find_range(ilist, 2))
  
  plot_list <- list()
  for(i in 1:nExp){
    p <- ggplot(ilist[[i]],aes(x = Frame, y = Fr))+
      geom_violin() +
      #geom_boxplot(width=.1, fill="black", outlier.colour=NA) +
      stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)
    
    plot_list[[i]]<- p +
      xlab("Frame Number")+ylab(axis_name[2])+
      #   x_scale +
      y_scale
  }
  plot_grid(plotlist = plot_list, labels = "AUTO",ncol = 2)
}

