library(TTR)
library(changepoint)
Fz <- exp_data %>%
  map(~get_comp(.x, Fz)) #Keep only t and Fr
cont_Fz  <- pmap(list(Fz, end_entry, start_exit), .f = get_zone)

# exploring through plots
library(plotly)

exp <- cont_Fz[[1]]
ma <- a[7260:7795,]
p <- ggplot(a, aes(x = t, y = Fz)) + geom_line()
p
q <- plot_ly(ma, x = ~t, y = ~Fz)%>% add_lines()
q
# The noise in Fr is very high. For simplifying the issue,
# I am using Fz for change point detection

# Testing change point for every experiment

# Exp 1

## Identifying change points
# A. Find SMA
exp <- cont_Fz[[1]]
a <- exp
Fz_mas <- SMA(a$Fz, n = 20)
Fz_mas[is.na(Fz_mas)] <- 0
# plot(Fz_mas, type = "l")

# New - using function
Fz_mas1 <- get_sma_vector(a)

# Get change points
cpts_Fz_mas <- cpt.mean(Fz_mas, method = "PELT",
                        penalty = "Manual", pen.value = 100000)

# plot(cpts_Fz_mas)
cp <- cpts(cpts_Fz_mas) # Final output
plot(a[2704:3190,], type = "l")

cp1 <- get_changepoints(a)
cp_list <- map(cont_Fz, get_changepoints)


## Using change points to extract the machining zone
start_p <- seq(1, length(cp), by = 2)
end_p <- start_p + 1
start <- cp[start_p]
end <- cp[end_p] - 20
fr_num <- 1:length(start)
fis <- function(start, end, frame, df, ...){
  df1 <- df %>%
    slice(., start:end) %>%
    mutate(Frame = frame)
}

trial <- pmap(list(start, end, fr_num), ~fis(..1, ..2, ..3, cont[[1]]))

tr <-bind_rows(trial)
# New - Using function
tr1 <- get_mc_data_steady(cont[[1]], cp1)


mc_data <- map2(cont, cp_list, ~get_mc_data(.x, .y))

mc_data_entry <- map2(cont, cp_list, ~get_mc_data_entry(.x, .y))

mc_data_steady <- map2(cont, cp_list, ~get_mc_data_steady(.x, .y))



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
mc_vio <- make_frames_as_factors(mc_data)

for(i in 1:10){
  p <- ggplot(mc_vio[[i]],aes(x = Frame, y = Fr))+
    geom_violin() +
    stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)
  save_plot(str_c("Violinplot",i,".pdf"),p,
            base_height = 8.27, base_width = 11.69)
  
}
mc_steady_vio <- make_frames_as_factors(mc_data_steady)


for(i in 1:10){
  p <- ggplot(mc_steady_vio[[i]],aes(x = Frame, y = Fr))+
    geom_violin() +
    stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)
  save_plot(str_c("Violinplot",i,".pdf"),p,
            base_height = 8.27, base_width = 11.69)
  
}
ase <- mc_data_steady[[8]]


mc_entry_vio <- make_frames_as_factors(mc_data_entry)


for(i in 1:10){
  p <- ggplot(mc_entry_vio[[i]],aes(x = Frame, y = Fr))+
    geom_violin() +
    stat_summary(fun.y=median, geom="point", fill="white", shape=21, size=2.5)
  save_plot(str_c("Violinplot",i,".pdf"),p,
            base_height = 8.27, base_width = 11.69)
  
}
asdf <- mc_data_entry[[8]]
asdf1 <- asdf[1:40,]
ggplot(asdf1,aes(x = t, y = Fr))+
  geom_line()

#Calculate and plot mean

tr_mean <- tr %>%
  group_by(Frame) %>%
  summarize(stat = mean(Fr))

plot(tr_mean, type = "l")

