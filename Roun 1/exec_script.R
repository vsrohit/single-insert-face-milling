# Global details for analysis ----
nExp <- 10 # Number of experiments
sf <- 10000 # Sampling Frequency


# Get the data and select the required component ----

#Read raw data from csv ----
exp_data <- get_exp_data()
#Use map function to calculate Fr and store the value
Fr <- exp_data %>%
  map(~cal_Fr(.x)) %>% #Calculate Fr
  map(~get_comp(.x, Fr)) #Keep only t and Fr

#Step 1 - separate entry, steady and end parts of data ----
#These are determined by looking at the graph plots

start_entry <- list(1, 20000, 55000, 1, 80000,
                    100000, 95000, 40000, 90000, 80000)

end_entry   <- list(150000,50000, 90000, 25000, 125000,
                    130000, 140000, 90000, 140000, 140000)

start_exit  <- list(1380000, 718000, 490000, 665000,765000,
                    975000, 1501000, 478800, 960000, 777900)

end_exit    <- list(1550000, 800000, 552500, 762500, 860000,
                    1100000, 1600000, 550000, 1080000, 870000)
# pmap helps in applying multiple arguments and .f placeholder
# makes it easy to pass the function rather than the normal
# way of using placeholders. The arguments are passed in order

entry <- pmap(list(Fr, start_entry, end_entry), .f = get_zone) 
cont  <- pmap(list(Fr, end_entry, start_exit), .f = get_zone)
exit  <- pmap(list(Fr, start_exit, end_exit), .f = get_zone)

# Analyze only continuous zone
# Step 2 - Get the frames of data using change point detection ---

#Fz is easy for change point detection than Fr.
# Hence, we use this for determining change points

#A. Get Fz data
Fz <- exp_data %>%
  map(~get_comp(.x, Fz)) #Keep only t and Fr
cont_Fz  <- pmap(list(Fz, end_entry, start_exit), .f = get_zone)


#B1. Get change points
cp_list <- map(cont_Fz, get_changepoints)

#B2. Get machining Data 
mc_data <- map2(cont, cp_list, ~get_mc_data(.x, .y))

mc_data_entry <- map2(cont, cp_list, ~get_mc_data_entry(.x, .y))

mc_data_steady <- map2(cont, cp_list, ~get_mc_data_steady(.x, .y))

#Entry data
# Find range of mc_data entry
jerk_list <- map(mc_data_entry, ~frame_stat(.x, calc_range, Fr))
p_jerk <- stat_plot(jerk_list, "Frame number", "Range")
save_plot("Range entry plot.pdf",p_jerk,
          base_height = 11.69, base_width = 8.27)
# Find the trend in jerk
jerk_trend <- map(jerk_list, ~trend(.x))

#Steady data

mean_list <- map(mc_data_steady, ~frame_stat(.x, mean, Fr))
p_mean <- stat_plot(mean_list, "Frame number", "Mean")
save_plot("Mean plot.pdf",p_mean,
          base_height = 11.69, base_width = 8.27)

sd_list <- map(mc_data_steady, ~frame_stat(.x, sd, Fr))
p_sd <- stat_plot(sd_list, "Frame number", "Std dev")
save_plot("sd plot.pdf",p_sd,
          base_height = 11.69, base_width = 8.27)

skew_list <- map(mc_data_steady, ~frame_stat(.x, skewness, Fr))
p_skew <- stat_plot(skew_list, "Frame number", "Skewness")
save_plot("Skewness plot.pdf",p_skew,
          base_height = 11.69, base_width = 8.27)

ks_list <- map(mc_data_steady, ~frame_stat(.x, kurtosis, Fr))
p_kurt <- stat_plot(ks_list, "Frame number", "Kurtosis")
save_plot("Kurtosis plot.pdf",p_kurt,
          base_height = 11.69, base_width = 8.27)
#Trend of stats
mean_trend <- map(mean_list, ~trend(.x))
sd_trend <- map(sd_list, ~trend(.x))
skew_trend <- map(skew_list, ~trend(.x))
ks_trend <- map(ks_list, ~trend(.x))

trend_summary <- tibble("mean_trend" = mean_trend,
                        "sd_trend" = sd_trend,
                        "skew_trend" = skew_trend,
                        "ks_trend" = ks_trend,
                        "entry_range_trend" = jerk_trend)
write.csv(as.matrix(trend_summary), "trend_summary.csv")
#Summary of the Round
full_mean <- map(mc_data_steady, ~full_stat(.x, mean, Fr))
full_sd <- map(mc_data_steady, ~full_stat(.x, sd, Fr))
full_skew <- map(mc_data_steady, ~full_stat(.x, skewness, Fr))
full_ks <- map(mc_data_steady, ~full_stat(.x, kurtosis, Fr))
jerk_mean <- map(jerk_list, ~full_stat(.x, mean, stat))

final_data <- tibble("Mean" = full_mean,
                         "Std.dev" = full_sd,
                        "Skewness" = full_skew,
                         "Kurtosis" = full_ks,
                     "Entry_range_mean" = jerk_mean)

write.csv(as.matrix(final_data), "final_data.csv") 

#Plotting violin
violin_data <- make_frames_as_factors(mc_data_steady)
pvio <- plot_violin(violin_data)
save_plot("Violinplot.pdf",pvio,
          base_height = 11.69, base_width = 8.27)