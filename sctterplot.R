library(ggplot2)
library(tidyverse)
library(cowplot)
library(readr)

#Import experment data
milling_exp_data <- read_csv("F:/R/milling_new/milling_exp_data1.csv")
View(milling_exp_data)

#Convert s,f,d into factors
exp_data <- milling_exp_data %>%
  mutate(s = as.factor(s)) %>%
  mutate(f = as.factor(f)) %>%
  mutate(d = as.factor(d)) %>%
  mutate(Round = as.factor(Round))

# Mean
#mean_plot1
mp1 <- ggplot(data = exp_data) +
  geom_point(mapping = aes(x = f, y = Mean, shape = s, size = d, color = Round))
save_plot("Mean vs feed.pdf",mp1,
          base_height = 4, base_width = 5)
#mean_plot2
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = s, y = Mean, shape = f, size = d, alpha = Round))


# Standard deviation
#sd_plot1
sd1 <- ggplot(data = exp_data) +
  geom_point(mapping = aes(x = f, y = Std.dev, shape = s, size = d, color = Round))
save_plot("sd vs feed.pdf",sd1,
          base_height = 4, base_width = 5)

#sd_plot2
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = s, y = Std.dev, shape = f, size = d, alpha = Round))


# Skewness
#skew_plot1
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = f, y = Skewness, shape = s, size = d, alpha = Round))
#skew_plot2
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = s, y = Skewness, shape = f, size = d, alpha = Round))

#Kurtosis
#ks_plot1
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = f, y = Kurtosis, shape = s, size = d, alpha = Round))
#ks_plot2
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = s, y = Kurtosis, shape = f, size = d, alpha = Round))

# Mean trend
#mean_trend1
mt1 <- ggplot(data = exp_data) +
  geom_point(mapping = aes(x = f, y = mean_trend, shape = s, size = d, color = Round))
save_plot("Mean trend vs feed.pdf",mt1,
          base_height = 4, base_width = 5)

#mean_trend2
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = s, y = mean_trend, shape = f, size = d, alpha = Round))

# Standard deviation trend
#sd_trend1
sdt1 <- ggplot(data = exp_data) +
  geom_point(mapping = aes(x = f, y = sd_trend, shape = s, size = d, color = Round))
save_plot("Standard deviation trends vs feed.pdf",sdt1,
          base_height = 4, base_width = 5)

#sd_trend2
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = s, y = sd_trend, shape = f, size = d, alpha = Round))

# Entry range trend
#entry_range_trend1
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = f, y = entry_range_trend, shape = s, size = d, alpha = Round))
#entry_range_trend2
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = s, y = entry_range_trend, shape = f, size = d, alpha = Round))

# Entry range mean
#entry_range_mean1
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = f, y = Entry_range_mean, shape = s, size = d, alpha = Round))
#entry_range_mean2
ggplot(data = exp_data) +
  geom_point(mapping = aes(x = s, y = Entry_range_mean, shape = f, size = d, alpha = Round))


sc <- ggplot(data = exp_data) +
  geom_point(mapping = aes(x = Mean, y = Std.dev, shape = f, color = s,size = d))
save_plot("Standard deviation vs Mean.pdf",sc,
          base_height = 5, base_width = 5)
