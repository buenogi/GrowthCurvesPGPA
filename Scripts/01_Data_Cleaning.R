################################################################################
############## Data cleaning  - Growth Curves PGPA HKO/KO clones  ##############
################################################################################

library(tidyverse)
library(dplyr)

#Loading data

# Experiment 1

D1_EXP1 <- read.csv(file = "Data/Raw/D1_EXP1.csv", sep = ",")
colnames(D1_EXP1) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D1_EXP1 <- D1_EXP1[,-1]
D1_EXP1 <- D1_EXP1[,-1]
D1_EXP1 <- D1_EXP1[,-5]
D1_EXP1 <- D1_EXP1[,-5]
D1_EXP1 <- D1_EXP1[,-5]
D1_EXP1 <- D1_EXP1[,-5]
D1_EXP1 <- D1_EXP1[,-5]
D1_EXP1 <- D1_EXP1[,-5]
D1_EXP1 <- D1_EXP1[,-5]
D1_EXP1 <- D1_EXP1[-1,]
D1_EXP1 <- D1_EXP1[-7,]

D1_EXP1 <- D1_EXP1 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D1_EXP1$tempo <- "0" 
D1_EXP1$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D1_EXP1$experiment <- "EXP1"  

D2_EXP1 <- read.csv(file = "Data/Raw/D2_EXP1.csv", sep = ",")
colnames(D2_EXP1) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D2_EXP1 <- D2_EXP1[,-1]
D2_EXP1 <- D2_EXP1[,-1]
D2_EXP1 <- D2_EXP1[,-5]
D2_EXP1 <- D2_EXP1[,-5]
D2_EXP1 <- D2_EXP1[,-5]
D2_EXP1 <- D2_EXP1[,-5]
D2_EXP1 <- D2_EXP1[,-5]
D2_EXP1 <- D2_EXP1[,-5]
D2_EXP1 <- D2_EXP1[,-5]
D2_EXP1 <- D2_EXP1[-1,]
D2_EXP1 <- D2_EXP1[-7,]

D2_EXP1 <- D2_EXP1 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D2_EXP1$tempo <- "24" 
D2_EXP1$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D2_EXP1$experiment <- "EXP1" 

D3_EXP1 <- read.csv(file = "Data/Raw/D3_EXP1.csv", sep = ",")
colnames(D3_EXP1) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D3_EXP1 <- D3_EXP1[,-1]
D3_EXP1 <- D3_EXP1[,-1]
D3_EXP1 <- D3_EXP1[,-5]
D3_EXP1 <- D3_EXP1[,-5]
D3_EXP1 <- D3_EXP1[,-5]
D3_EXP1 <- D3_EXP1[,-5]
D3_EXP1 <- D3_EXP1[,-5]
D3_EXP1 <- D3_EXP1[,-5]
D3_EXP1 <- D3_EXP1[,-5]
D3_EXP1 <- D3_EXP1[-1,]
D3_EXP1 <- D3_EXP1[-7,]

D3_EXP1 <- D3_EXP1 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D3_EXP1$tempo <- "48" 
D3_EXP1$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D3_EXP1$experiment <- "EXP1" 

D4_EXP1 <- read.csv(file = "Data/Raw/D4_EXP1.csv", sep = ",")
colnames(D4_EXP1) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D4_EXP1 <- D4_EXP1[,-1]
D4_EXP1 <- D4_EXP1[,-1]
D4_EXP1 <- D4_EXP1[,-5]
D4_EXP1 <- D4_EXP1[,-5]
D4_EXP1 <- D4_EXP1[,-5]
D4_EXP1 <- D4_EXP1[,-5]
D4_EXP1 <- D4_EXP1[,-5]
D4_EXP1 <- D4_EXP1[,-5]
D4_EXP1 <- D4_EXP1[,-5]
D4_EXP1 <- D4_EXP1[-1,]
D4_EXP1 <- D4_EXP1[-7,]

D4_EXP1 <- D4_EXP1 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D4_EXP1$tempo <- "0" 
D4_EXP1$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D4_EXP1$experiment <- "EXP1"  
D5_EXP1 <- read.csv(file = "Data/Raw/D5_EXP1.csv", sep = ",")
colnames(D5_EXP1) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D5_EXP1 <- D5_EXP1[,-1]
D5_EXP1 <- D5_EXP1[,-1]
D5_EXP1 <- D5_EXP1[,-5]
D5_EXP1 <- D5_EXP1[,-5]
D5_EXP1 <- D5_EXP1[,-5]
D5_EXP1 <- D5_EXP1[,-5]
D5_EXP1 <- D5_EXP1[,-5]
D5_EXP1 <- D5_EXP1[,-5]
D5_EXP1 <- D5_EXP1[,-5]
D5_EXP1 <- D5_EXP1[-1,]
D5_EXP1 <- D5_EXP1[-7,]

D5_EXP1 <- D5_EXP1 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D5_EXP1$tempo <- "96" 
D5_EXP1$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D5_EXP1$experiment <- "EXP1" 


D6_EXP1 <- read.csv(file = "Data/Raw/D6_EXP1.csv", sep = ",")
colnames(D6_EXP1) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D6_EXP1 <- D6_EXP1[,-1]
D6_EXP1 <- D6_EXP1[,-1]
D6_EXP1 <- D6_EXP1[,-5]
D6_EXP1 <- D6_EXP1[,-5]
D6_EXP1 <- D6_EXP1[,-5]
D6_EXP1 <- D6_EXP1[,-5]
D6_EXP1 <- D6_EXP1[,-5]
D6_EXP1 <- D6_EXP1[,-5]
D6_EXP1 <- D6_EXP1[,-5]
D6_EXP1 <- D6_EXP1[-1,]
D6_EXP1 <- D6_EXP1[-7,]

D6_EXP1 <- D6_EXP1 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D6_EXP1$tempo <- "120" 
D6_EXP1$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D6_EXP1$experiment <- "EXP1"  

D7_EXP1 <- read.csv(file = "Data/Raw/D7_EXP1.csv", sep = ",")
colnames(D7_EXP1) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D7_EXP1 <- D7_EXP1[,-1]
D7_EXP1 <- D7_EXP1[,-1]
D7_EXP1 <- D7_EXP1[,-5]
D7_EXP1 <- D7_EXP1[,-5]
D7_EXP1 <- D7_EXP1[,-5]
D7_EXP1 <- D7_EXP1[,-5]
D7_EXP1 <- D7_EXP1[,-5]
D7_EXP1 <- D7_EXP1[,-5]
D7_EXP1 <- D7_EXP1[,-5]
D7_EXP1 <- D7_EXP1[-1,]
D7_EXP1 <- D7_EXP1[-7,]

D7_EXP1 <- D7_EXP1 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D7_EXP1$tempo <- "144" 
D7_EXP1$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D7_EXP1$experiment <- "EXP1" 

# Experiment 2


D1_EXP2 <- read.csv(file = "Data/Raw/D1_EXP2.csv", sep = ",")
colnames(D1_EXP2) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D1_EXP2 <- D1_EXP2[,-1]
D1_EXP2 <- D1_EXP2[,-1]
D1_EXP2 <- D1_EXP2[,-5]
D1_EXP2 <- D1_EXP2[,-5]
D1_EXP2 <- D1_EXP2[,-5]
D1_EXP2 <- D1_EXP2[,-5]
D1_EXP2 <- D1_EXP2[,-5]
D1_EXP2 <- D1_EXP2[,-5]
D1_EXP2 <- D1_EXP2[,-5]
D1_EXP2 <- D1_EXP2[-1,]
D1_EXP2 <- D1_EXP2[-7,]

D1_EXP2 <- D1_EXP2 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D1_EXP2$tempo <- "0" 
D1_EXP2$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D1_EXP2$experiment <- "EXP2"  

D2_EXP2 <- read.csv(file = "Data/Raw/D2_EXP2.csv", sep = ",")
colnames(D2_EXP2) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D2_EXP2 <- D2_EXP2[,-1]
D2_EXP2 <- D2_EXP2[,-1]
D2_EXP2 <- D2_EXP2[,-5]
D2_EXP2 <- D2_EXP2[,-5]
D2_EXP2 <- D2_EXP2[,-5]
D2_EXP2 <- D2_EXP2[,-5]
D2_EXP2 <- D2_EXP2[,-5]
D2_EXP2 <- D2_EXP2[,-5]
D2_EXP2 <- D2_EXP2[,-5]
D2_EXP2 <- D2_EXP2[-1,]
D2_EXP2 <- D2_EXP2[-7,]

D2_EXP2 <- D2_EXP2 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D2_EXP2$tempo <- "24" 
D2_EXP2$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D2_EXP2$experiment <- "EXP2" 

D3_EXP2 <- read.csv(file = "Data/Raw/D3_EXP2.csv", sep = ",")
colnames(D3_EXP2) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D3_EXP2 <- D3_EXP2[,-1]
D3_EXP2 <- D3_EXP2[,-1]
D3_EXP2 <- D3_EXP2[,-5]
D3_EXP2 <- D3_EXP2[,-5]
D3_EXP2 <- D3_EXP2[,-5]
D3_EXP2 <- D3_EXP2[,-5]
D3_EXP2 <- D3_EXP2[,-5]
D3_EXP2 <- D3_EXP2[,-5]
D3_EXP2 <- D3_EXP2[,-5]
D3_EXP2 <- D3_EXP2[-1,]
D3_EXP2 <- D3_EXP2[-7,]

D3_EXP2 <- D3_EXP2 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D3_EXP2$tempo <- "48" 
D3_EXP2$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D3_EXP2$experiment <- "EXP2" 


D4_EXP2 <- read.csv(file = "Data/Raw/D4_EXP2.csv", sep = ",")
colnames(D4_EXP2) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D4_EXP2 <- D4_EXP2[,-1]
D4_EXP2 <- D4_EXP2[,-1]
D4_EXP2 <- D4_EXP2[,-5]
D4_EXP2 <- D4_EXP2[,-5]
D4_EXP2 <- D4_EXP2[,-5]
D4_EXP2 <- D4_EXP2[,-5]
D4_EXP2 <- D4_EXP2[,-5]
D4_EXP2 <- D4_EXP2[,-5]
D4_EXP2 <- D4_EXP2[,-5]
D4_EXP2 <- D4_EXP2[-1,]
D4_EXP2 <- D4_EXP2[-7,]

D4_EXP2 <- D4_EXP2 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D4_EXP2$tempo <- "0" 
D4_EXP2$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D4_EXP2$experiment <- "EXP2"  



D5_EXP2 <- read.csv(file = "Data/Raw/D5_EXP2.csv", sep = ",")
colnames(D5_EXP2) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D5_EXP2 <- D5_EXP2[,-1]
D5_EXP2 <- D5_EXP2[,-1]
D5_EXP2 <- D5_EXP2[,-5]
D5_EXP2 <- D5_EXP2[,-5]
D5_EXP2 <- D5_EXP2[,-5]
D5_EXP2 <- D5_EXP2[,-5]
D5_EXP2 <- D5_EXP2[,-5]
D5_EXP2 <- D5_EXP2[,-5]
D5_EXP2 <- D5_EXP2[,-5]
D5_EXP2 <- D5_EXP2[-1,]
D5_EXP2 <- D5_EXP2[-7,]

D5_EXP2 <- D5_EXP2 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D5_EXP2$tempo <- "96" 
D5_EXP2$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D5_EXP2$experiment <- "EXP2" 


D6_EXP2 <- read.csv(file = "Data/Raw/D6_EXP2.csv", sep = ",")
colnames(D6_EXP2) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D6_EXP2 <- D6_EXP2[,-1]
D6_EXP2 <- D6_EXP2[,-1]
D6_EXP2 <- D6_EXP2[,-5]
D6_EXP2 <- D6_EXP2[,-5]
D6_EXP2 <- D6_EXP2[,-5]
D6_EXP2 <- D6_EXP2[,-5]
D6_EXP2 <- D6_EXP2[,-5]
D6_EXP2 <- D6_EXP2[,-5]
D6_EXP2 <- D6_EXP2[,-5]
D6_EXP2 <- D6_EXP2[-1,]
D6_EXP2 <- D6_EXP2[-7,]

D6_EXP2 <- D6_EXP2 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D6_EXP2$tempo <- "120" 
D6_EXP2$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D6_EXP2$experiment <- "EXP2"  

D7_EXP2 <- read.csv(file = "Data/Raw/D7_EXP2.csv", sep = ",")
colnames(D7_EXP2) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D7_EXP2 <- D7_EXP2[,-1]
D7_EXP2 <- D7_EXP2[,-1]
D7_EXP2 <- D7_EXP2[,-5]
D7_EXP2 <- D7_EXP2[,-5]
D7_EXP2 <- D7_EXP2[,-5]
D7_EXP2 <- D7_EXP2[,-5]
D7_EXP2 <- D7_EXP2[,-5]
D7_EXP2 <- D7_EXP2[,-5]
D7_EXP2 <- D7_EXP2[,-5]
D7_EXP2 <- D7_EXP2[-1,]
D7_EXP2 <- D7_EXP2[-7,]

D7_EXP2 <- D7_EXP2 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D7_EXP2$tempo <- "144" 
D7_EXP2$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D7_EXP2$experiment <- "EXP2" 


# Experiment 3


D1_EXP3 <- read.csv(file = "Data/Raw/D1_EXP3.csv", sep = ",")
colnames(D1_EXP3) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D1_EXP3 <- D1_EXP3[,-1]
D1_EXP3 <- D1_EXP3[,-1]
D1_EXP3 <- D1_EXP3[,-5]
D1_EXP3 <- D1_EXP3[,-5]
D1_EXP3 <- D1_EXP3[,-5]
D1_EXP3 <- D1_EXP3[,-5]
D1_EXP3 <- D1_EXP3[,-5]
D1_EXP3 <- D1_EXP3[,-5]
D1_EXP3 <- D1_EXP3[,-5]
D1_EXP3 <- D1_EXP3[-1,]
D1_EXP3 <- D1_EXP3[-7,]

D1_EXP3 <- D1_EXP3 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D1_EXP3$tempo <- "0" 
D1_EXP3$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D1_EXP3$experiment <- "EXP3"  

D2_EXP3 <- read.csv(file = "Data/Raw/D2_EXP3.csv", sep = ",")
colnames(D2_EXP3) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D2_EXP3 <- D2_EXP3[,-1]
D2_EXP3 <- D2_EXP3[,-1]
D2_EXP3 <- D2_EXP3[,-5]
D2_EXP3 <- D2_EXP3[,-5]
D2_EXP3 <- D2_EXP3[,-5]
D2_EXP3 <- D2_EXP3[,-5]
D2_EXP3 <- D2_EXP3[,-5]
D2_EXP3 <- D2_EXP3[,-5]
D2_EXP3 <- D2_EXP3[,-5]
D2_EXP3 <- D2_EXP3[-1,]
D2_EXP3 <- D2_EXP3[-7,]

D2_EXP3 <- D2_EXP3 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D2_EXP3$tempo <- "24" 
D2_EXP3$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D2_EXP3$experiment <- "EXP3" 

D3_EXP3 <- read.csv(file = "Data/Raw/D3_EXP3.csv", sep = ",")
colnames(D3_EXP3) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D3_EXP3 <- D3_EXP3[,-1]
D3_EXP3 <- D3_EXP3[,-1]
D3_EXP3 <- D3_EXP3[,-5]
D3_EXP3 <- D3_EXP3[,-5]
D3_EXP3 <- D3_EXP3[,-5]
D3_EXP3 <- D3_EXP3[,-5]
D3_EXP3 <- D3_EXP3[,-5]
D3_EXP3 <- D3_EXP3[,-5]
D3_EXP3 <- D3_EXP3[,-5]
D3_EXP3 <- D3_EXP3[-1,]
D3_EXP3 <- D3_EXP3[-7,]

D3_EXP3 <- D3_EXP3 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D3_EXP3$tempo <- "48" 
D3_EXP3$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D3_EXP3$experiment <- "EXP3" 

D4_EXP3 <- read.csv(file = "Data/Raw/D4_EXP3.csv", sep = ",")
colnames(D4_EXP3) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D4_EXP3 <- D4_EXP3[,-1]
D4_EXP3 <- D4_EXP3[,-1]
D4_EXP3 <- D4_EXP3[,-5]
D4_EXP3 <- D4_EXP3[,-5]
D4_EXP3 <- D4_EXP3[,-5]
D4_EXP3 <- D4_EXP3[,-5]
D4_EXP3 <- D4_EXP3[,-5]
D4_EXP3 <- D4_EXP3[,-5]
D4_EXP3 <- D4_EXP3[,-5]
D4_EXP3 <- D4_EXP3[-1,]
D4_EXP3 <- D4_EXP3[-7,]

D4_EXP3 <- D4_EXP3 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D4_EXP3$tempo <- "0" 
D4_EXP3$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D4_EXP3$experiment <- "EXP3"  

D5_EXP3 <- read.csv(file = "Data/Raw/D5_EXP3.csv", sep = ",")
colnames(D5_EXP3) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D5_EXP3 <- D5_EXP3[,-1]
D5_EXP3 <- D5_EXP3[,-1]
D5_EXP3 <- D5_EXP3[,-5]
D5_EXP3 <- D5_EXP3[,-5]
D5_EXP3 <- D5_EXP3[,-5]
D5_EXP3 <- D5_EXP3[,-5]
D5_EXP3 <- D5_EXP3[,-5]
D5_EXP3 <- D5_EXP3[,-5]
D5_EXP3 <- D5_EXP3[,-5]
D5_EXP3 <- D5_EXP3[-1,]
D5_EXP3 <- D5_EXP3[-7,]

D5_EXP3 <- D5_EXP3 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D5_EXP3$tempo <- "96" 
D5_EXP3$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D5_EXP3$experiment <- "EXP3" 


D6_EXP3 <- read.csv(file = "Data/Raw/D6_EXP3.csv", sep = ",")
colnames(D6_EXP3) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D6_EXP3 <- D6_EXP3[,-1]
D6_EXP3 <- D6_EXP3[,-1]
D6_EXP3 <- D6_EXP3[,-5]
D6_EXP3 <- D6_EXP3[,-5]
D6_EXP3 <- D6_EXP3[,-5]
D6_EXP3 <- D6_EXP3[,-5]
D6_EXP3 <- D6_EXP3[,-5]
D6_EXP3 <- D6_EXP3[,-5]
D6_EXP3 <- D6_EXP3[,-5]
D6_EXP3 <- D6_EXP3[-1,]
D6_EXP3 <- D6_EXP3[-7,]

D6_EXP3 <- D6_EXP3 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D6_EXP3$tempo <- "120" 
D6_EXP3$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D6_EXP3$experiment <- "EXP3"  

D7_EXP3 <- read.csv(file = "Data/Raw/D7_EXP3.csv", sep = ",")
colnames(D7_EXP3) <-  c("x","x1", "REF", "C76","C67", "C68", "x1", "x1", "x1", "x1","x1", "x1", "x1")
D7_EXP3 <- D7_EXP3[,-1]
D7_EXP3 <- D7_EXP3[,-1]
D7_EXP3 <- D7_EXP3[,-5]
D7_EXP3 <- D7_EXP3[,-5]
D7_EXP3 <- D7_EXP3[,-5]
D7_EXP3 <- D7_EXP3[,-5]
D7_EXP3 <- D7_EXP3[,-5]
D7_EXP3 <- D7_EXP3[,-5]
D7_EXP3 <- D7_EXP3[,-5]
D7_EXP3 <- D7_EXP3[-1,]
D7_EXP3 <- D7_EXP3[-7,]

D7_EXP3 <- D7_EXP3 %>%
  pivot_longer(cols=c("REF", "C76","C67", "C68"),
               names_to='pop',
               values_to='abs')
D7_EXP3$tempo <- "144" 
D7_EXP3$conc  <- rep(c("0", "10","50","75","100","150"), each = 4)
D7_EXP3$experiment <- "EXP3" 

#----------------Binding data

DataGC <- rbind(D1_EXP1,D1_EXP2, D1_EXP3, D2_EXP1, D2_EXP2, D2_EXP3,
                D3_EXP1, D3_EXP2, D3_EXP3, D4_EXP1, D4_EXP2,
                D4_EXP3,D5_EXP1, D5_EXP2, D5_EXP3, D6_EXP1, D6_EXP2, 
                D6_EXP3,D7_EXP1, D7_EXP2, D7_EXP3)

write.csv(DataGC, file = "Data/Processed/DataGC.csv", sep = ",")


