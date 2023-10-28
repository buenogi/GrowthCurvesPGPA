# Normalização

library(tidyverse)
library(ggplot2)

#Loading data

DataGC <- read.csv(file = "Data/Processed/DataGC.csv")

abs <- 0
pop <- NULL
exp <- NULL
conc <- NULL


abs_norm <- DataGC%>%
  group_by(pop,conc, experiment)%>%
  summarise(abs_norm = mean(abs))
abs_norm$n_base <-500000 


DataGC_N <- left_join(DataGC, abs_norm, by = c("conc", "experiment", "pop"))

for(i in 1:nrow(DataGC_N)){
  DataGC_N$n_parasitos[i] <-  DataGC_N$abs[i]*DataGC_N$n_base[i]/DataGC_N$abs_norm[i]
  }

write_csv(DataGC_N, file = "Data/Processed/DataGC_N.csv")
