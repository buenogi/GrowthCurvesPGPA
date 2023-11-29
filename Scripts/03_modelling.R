################################################################################
################ Promastigote Growth Curve Dose-response analysis ##############
################################################################################

library(dplyr)
library(ggplot2) 
library(drc)
library(gridExtra)

# Loading data 

DataGC <- read.csv(file = "Data/Processed/DataGC.csv")

# Checking data
head(DataGC)
sapply(DataGC, class)
DataGC$experiment <- as.factor(DataGC$experiment)
DataGC$pop <- as.factor(DataGC$pop)
sapply(DataGC, class)

DataGC <-  DataGC[DataGC$tempo != 0, ]
# DataGC <- DataGC%>%
#   group_by(conc, pop, experiment, tempo)%>%
#   summarise(mean_value = mean(abs), sd_value = sd(abs))


#Splitting populations per conc

REF_C0 <- filter(DataGC, pop == "REF", conc =="0")
C76_C0 <- filter(DataGC, pop == "C76",conc =="0")
C67_C0 <- filter(DataGC, pop == "C67",conc =="0")
C68_C0 <- filter(DataGC, pop == "C68",conc =="0")

# Adjusting the models

REF_C0_LL4 <- drm(abs~tempo, data = REF_C0 , fct =  LL.4())
plot(REF_C0_LL4, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C0_LL4)

C76_C0_LL4 <- drm(abs~ tempo, data = C76_C0 , fct =  LL.4())
plot(C76_C0_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C0_LL4 <- drm(abs~ tempo, data = C67_C0 , fct =  LL.4())
plot(C67_C0_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C0_LL4 <- drm(abs~ tempo, data = C68_C0 , fct =  LL.4())
plot(C68_C0_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C0 analysis

newdata_REF_C0 <- expand.grid(tempo = seq(min(REF_C0$tempo), max(REF_C0$tempo), length.out = 200))
newdata_REF_C0$abs <- predict(REF_C0_LL4, newdata_REF_C0, type = 'response')
newdata_REF_C0$pop <- "REF"

newdata_C76_C0 <- expand.grid(tempo = seq(min(C76_C0$tempo), max(C76_C0$tempo), length.out = 200))
newdata_C76_C0$abs <- predict(C76_C0_LL4, newdata_C76_C0, type = 'response')
newdata_C76_C0$pop <- "C76"

newdata_C67_C0 <- expand.grid(tempo = seq(min(C67_C0$tempo), max(C67_C0$tempo), length.out = 200))
newdata_C67_C0$abs <- predict(C67_C0_LL4, newdata_C67_C0, type = 'response')
newdata_C67_C0$pop <- "C67"

newdata_C68_C0 <- expand.grid(tempo = seq(min(C68_C0$tempo), max(C68_C0$tempo), length.out = 200))
newdata_C68_C0$abs <- predict(C68_C0_LL4, newdata_C68_C0, type = 'response')
newdata_C68_C0$pop <- "C68"

## Exploring predicted model

GrowthCurves_C0 <- full_join(REF_C0, C67_C0)
GrowthCurves_C0 <- full_join(GrowthCurves_C0, C76_C0)
GrowthCurves_C0 <- full_join(GrowthCurves_C0, C68_C0)

newdata_full <- rbind.data.frame(newdata_REF_C0, newdata_C67_C0)
newdata_full <- rbind(newdata_full, newdata_C76_C0)
newdata_full <- rbind(newdata_full, newdata_C68_C0)


DoseResponseCurves_01 <- ggplot() +
  geom_point(data = REF_C0, aes(x = tempo, y = abs), size = 2) +
  geom_line(data = newdata_REF_C0, aes(x = tempo, y = newdata_REF_C0$abs)) +
  geom_point(data = C76_C0, aes(x = tempo, y = abs),size = 2) +
  geom_line(data = newdata_C76_C0, aes(x = tempo, y = newdata_C76_C0$abs))+
  geom_point(data = C67_C0, aes(x = tempo, y = abs),size = 2) +
  geom_line(data = newdata_C67_C0, aes(x = tempo, y = newdata_C67_C0$abs))+
  geom_point(data = C68_C0, aes(x = tempo, y = abs),size = 2) +
  geom_line(data = newdata_C68_C0, aes(x = tempo, y = newdata_C68_C0$abs)) +
  labs(x = "Tempo", y = "Abs (600nm)") +
  scale_x_continuous(breaks = c(24, 48, 72, 96, 120),
                     labels = c("24h", "48h", "72h", "96h", "120h")) +
  theme_bw() +
  theme( text = element_text(size = 18, face = "bold")) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")), nrow = 1)

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_0.png")

summary(REF_C0_LL4)
# Extracting measures

COEFICIENTS <- c("GrowthRate", "c","d","e")
modelREF <- summary(REF_C0_LL4)
confintREF <- confint(REF_C0_LL4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C0", 4)

COEFICIENTS <- c("GrowthRate", "c","d","e")
modelC76 <- summary(C76_C0_LL4)
confintC76 <- confint(C76_C0_LL4)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C0", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC67 <- summary(C67_C0_LL4)
confintC67 <- confint(C67_C0_LL4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C0", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC7 <- summary(C68_C0_LL4)
confintC7 <- confint(C68_C0_LL4)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C0", 4)

summary_DR1 <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR1 , file = "Docs/summary_GC_C0.csv", row.names = FALSE)

# Models comparisons

m1<-drm(abs ~ tempo*conc, pop, data = DataGC, 
        fct =LL.4(names = c("GrowthRate", "e")))
m2<-drm(abs ~ tempo*conc, data = DataGC, 
        fct =LL.4(names = c("GrowthRate", "e")))

anova(m1,m2) 

# Concentration 2

#Splitting populations per conc

REF_C10 <- filter(DataGC, pop == "REF", conc =="10")
C76_C10 <- filter(DataGC, pop == "C76",conc =="10")
C67_C10 <- filter(DataGC, pop == "C67",conc =="10")
C68_C10 <- filter(DataGC, pop == "C68",conc =="10")

# Adjusting the models

REF_C10_LL4 <- drm(abs~tempo, data = REF_C10 , fct =  LL.4())
plot(REF_C10_LL4, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C10_LL4)

C76_C10_LL4 <- drm(abs~ tempo, data = C76_C10 , fct =  LL.4())
plot(C76_C10_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C10_LL4 <- drm(abs~ tempo, data = C67_C10 , fct =  LL.4())
plot(C67_C10_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C10_LL4 <- drm(abs~ tempo, data = C68_C10 , fct =  LL.4())
plot(C68_C10_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C10 analysis

newdata_REF_C10 <- expand.grid(tempo = seq(min(REF_C10$tempo), max(REF_C10$tempo), length.out = 200))
newdata_REF_C10$abs <- predict(REF_C10_LL4, newdata_REF_C10, type = 'response')
newdata_REF_C10$pop <- "REF"

newdata_C76_C10 <- expand.grid(tempo = seq(min(C76_C10$tempo), max(C76_C10$tempo), length.out = 200))
newdata_C76_C10$abs <- predict(C76_C10_LL4, newdata_C76_C10, type = 'response')
newdata_C76_C10$pop <- "C76"

newdata_C67_C10 <- expand.grid(tempo = seq(min(C67_C10$tempo), max(C67_C10$tempo), length.out = 200))
newdata_C67_C10$abs <- predict(C67_C10_LL4, newdata_C67_C10, type = 'response')
newdata_C67_C10$pop <- "C67"

newdata_C68_C10 <- expand.grid(tempo = seq(min(C68_C10$tempo), max(C68_C10$tempo), length.out = 200))
newdata_C68_C10$abs <- predict(C68_C10_LL4, newdata_C68_C10, type = 'response')
newdata_C68_C10$pop <- "C68"

## Exploring predicted model

GrowthCurves_C10 <- full_join(REF_C10, C67_C10)
GrowthCurves_C10 <- full_join(GrowthCurves_C10, C76_C10)
GrowthCurves_C10 <- full_join(GrowthCurves_C10, C68_C10)

newdata_full <- rbind.data.frame(newdata_REF_C10, newdata_C67_C10)
newdata_full <- rbind(newdata_full, newdata_C76_C10)
newdata_full <- rbind(newdata_full, newdata_C68_C10)


DoseResponseCurves_01 <- ggplot() +
  geom_point(data = REF_C10, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_REF_C10, aes(x = tempo, y = newdata_REF_C10$abs)) +
  geom_point(data = C76_C10, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C76_C10, aes(x = tempo, y = newdata_C76_C10$abs))+
  geom_point(data = C67_C10, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C67_C10, aes(x = tempo, y = newdata_C67_C10$abs))+
  geom_point(data = C68_C10, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C68_C10, aes(x = tempo, y = newdata_C68_C10$abs)) +
  labs(x = "Tempo", y = "Abs (600nm)") +
  labs(x = "Tempo", y = "Abs (600nm)") +
  scale_x_continuous(breaks = c(24, 48, 72, 96, 120),
                     labels = c("24h", "48h", "72h", "96h", "120h")) +
  theme_bw() +
  theme( text = element_text(size = 18, face = "bold")) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")), nrow = 1)

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_10.png")

summary(REF_C10_LL4)
# Extracting measures

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelREF <- summary(REF_C10_LL4)
confintREF <- confint(REF_C10_LL4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C10", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC76 <- summary(C76_C10_LL4)
confintC76 <- confint(C76_C10_LL4)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C10", 4)

COEFICIENTS <- c("GrowthRate", "c","d","e")
modelC67 <- summary(C67_C10_LL4)
confintC67 <- confint(C67_C10_LL4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C10",4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC7 <- summary(C68_C10_LL4)
confintC7 <- confint(C68_C10_LL4)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C10", 4)

summary_DR2 <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR2 , file = "Docs/summary_GC_C10.csv", row.names = FALSE)


# Concentration 3

#Splitting populations per conc

REF_C50 <- filter(DataGC, pop == "REF", conc =="50")
C76_C50 <- filter(DataGC, pop == "C76",conc =="50")
C67_C50 <- filter(DataGC, pop == "C67",conc =="50")
C68_C50 <- filter(DataGC, pop == "C68",conc =="50")

# Adjusting the models

REF_C50_LL4 <- drm(abs~tempo, data = REF_C50 , fct =  LL.4())
plot(REF_C50_LL4, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C50_LL4)

C76_C50_LL4 <- drm(abs~ tempo, data = C76_C50 , fct =  LL.4())
plot(C76_C50_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C50_LL4 <- drm(abs~ tempo, data = C67_C50 , fct =  LL.4())
plot(C67_C50_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C50_LL4 <- drm(abs~ tempo, data = C68_C50 , fct =  LL.4())
plot(C68_C50_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C50 analysis

newdata_REF_C50 <- expand.grid(tempo = seq(min(REF_C50$tempo), max(REF_C50$tempo), length.out = 200))
newdata_REF_C50$abs <- predict(REF_C50_LL4, newdata_REF_C50, type = 'response')
newdata_REF_C50$pop <- "REF"

newdata_C76_C50 <- expand.grid(tempo = seq(min(C76_C50$tempo), max(C76_C50$tempo), length.out = 200))
newdata_C76_C50$abs <- predict(C76_C50_LL4, newdata_C76_C50, type = 'response')
newdata_C76_C50$pop <- "C76"

newdata_C67_C50 <- expand.grid(tempo = seq(min(C67_C50$tempo), max(C67_C50$tempo), length.out = 200))
newdata_C67_C50$abs <- predict(C67_C50_LL4, newdata_C67_C50, type = 'response')
newdata_C67_C50$pop <- "C67"

newdata_C68_C50 <- expand.grid(tempo = seq(min(C68_C50$tempo), max(C68_C50$tempo), length.out = 200))
newdata_C68_C50$abs <- predict(C68_C50_LL4, newdata_C68_C50, type = 'response')
newdata_C68_C50$pop <- "C68"

## Exploring predicted model

GrowthCurves_C50 <- full_join(REF_C50, C67_C50)
GrowthCurves_C50 <- full_join(GrowthCurves_C50, C76_C50)
GrowthCurves_C50 <- full_join(GrowthCurves_C50, C68_C50)

newdata_full <- rbind.data.frame(newdata_REF_C50, newdata_C67_C50)
newdata_full <- rbind(newdata_full, newdata_C76_C50)
newdata_full <- rbind(newdata_full, newdata_C68_C50)


DoseResponseCurves_01 <- ggplot() +
  geom_point(data = REF_C50, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_REF_C50, aes(x = tempo, y = newdata_REF_C50$abs)) +
  geom_point(data = C76_C50, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C76_C50, aes(x = tempo, y = newdata_C76_C50$abs))+
  geom_point(data = C67_C50, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C67_C50, aes(x = tempo, y = newdata_C67_C50$abs))+
  geom_point(data = C68_C50, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C68_C50, aes(x = tempo, y = newdata_C68_C50$abs)) +
  labs(x = "Tempo", y = "Abs (600nm)") +
  scale_x_continuous(breaks = c(24, 48, 72, 96, 120),
                     labels = c("24h", "48h", "72h", "96h", "120h")) +
  theme_bw() +
  theme( text = element_text(size = 18, face = "bold")) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")), nrow = 1)

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_50.png")



summary(REF_C50_LL4)
# Extracting measures

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelREF <- summary(REF_C50_LL4)
confintREF <- confint(REF_C50_LL4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C50", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC76 <- summary(C76_C50_LL4)
confintC76 <- confint(C76_C50_LL4)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C50", 4)

COEFICIENTS <- c("GrowthRate", "c","d","e")
modelC67 <- summary(C67_C50_LL4)
confintC67 <- confint(C67_C50_LL4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C50", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC7 <- summary(C68_C50_LL4)
confintC7 <- confint(C68_C50_LL4)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C50", 4)

summary_DR3 <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR3 , file = "Docs/summary_GC_C50.csv", row.names = FALSE)


# Concentration 4

#Splitting populations per conc

REF_C75 <- filter(DataGC, pop == "REF", conc =="75")
C76_C75 <- filter(DataGC, pop == "C76",conc =="75")
C67_C75 <- filter(DataGC, pop == "C67",conc =="75")
C68_C75 <- filter(DataGC, pop == "C68",conc =="75")

# Adjusting the models

REF_C75_LL4 <- drm(abs~tempo, data = REF_C75 , fct =  LL.4())
plot(REF_C75_LL4, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C75_LL4)

C76_C75_LL4 <- drm(abs~ tempo, data = C76_C75 , fct =  LL.4())
plot(C76_C75_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C75_LL4 <- drm(abs~ tempo, data = C67_C75 , fct =  LL.4())
plot(C67_C75_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C75_LL4 <- drm(abs~ tempo, data = C68_C75 , fct =  LL.4())
plot(C68_C75_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C75 analysis

newdata_REF_C75 <- expand.grid(tempo = seq(min(REF_C75$tempo), max(REF_C75$tempo), length.out = 200))
newdata_REF_C75$abs <- predict(REF_C75_LL4, newdata_REF_C75, type = 'response')
newdata_REF_C75$pop <- "REF"

newdata_C76_C75 <- expand.grid(tempo = seq(min(C76_C75$tempo), max(C76_C75$tempo), length.out = 200))
newdata_C76_C75$abs <- predict(C76_C75_LL4, newdata_C76_C75, type = 'response')
newdata_C76_C75$pop <- "C76"

newdata_C67_C75 <- expand.grid(tempo = seq(min(C67_C75$tempo), max(C67_C75$tempo), length.out = 200))
newdata_C67_C75$abs <- predict(C67_C75_LL4, newdata_C67_C75, type = 'response')
newdata_C67_C75$pop <- "C67"

newdata_C68_C75 <- expand.grid(tempo = seq(min(C68_C75$tempo), max(C68_C75$tempo), length.out = 200))
newdata_C68_C75$abs <- predict(C68_C75_LL4, newdata_C68_C75, type = 'response')
newdata_C68_C75$pop <- "C68"

## Exploring predicted model

GrowthCurves_C75 <- full_join(REF_C75, C67_C75)
GrowthCurves_C75 <- full_join(GrowthCurves_C75, C76_C75)
GrowthCurves_C75 <- full_join(GrowthCurves_C75, C68_C75)

newdata_full <- rbind.data.frame(newdata_REF_C75, newdata_C67_C75)
newdata_full <- rbind(newdata_full, newdata_C76_C75)
newdata_full <- rbind(newdata_full, newdata_C68_C75)


DoseResponseCurves_01 <- ggplot() +
  geom_point(data = REF_C75, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_REF_C75, aes(x = tempo, y = newdata_REF_C75$abs)) +
  geom_point(data = C76_C75, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C76_C75, aes(x = tempo, y = newdata_C76_C75$abs))+
  geom_point(data = C67_C75, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C67_C75, aes(x = tempo, y = newdata_C67_C75$abs))+
  geom_point(data = C68_C75, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C68_C75, aes(x = tempo, y = newdata_C68_C75$abs)) +
  labs(x = "Tempo", y = "Abs (600nm)") +
  scale_x_continuous(breaks = c(24, 48, 72, 96, 120),
                     labels = c("24h", "48h", "72h", "96h", "120h")) +
  theme_bw() +
  theme( text = element_text(size = 18, face = "bold")) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")), nrow = 1)

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_75.png")



summary(REF_C75_LL4)
# Extracting measures

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelREF <- summary(REF_C75_LL4)
confintREF <- confint(REF_C75_LL4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C75", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC76 <- summary(C76_C75_LL4)
confintC76 <- confint(C76_C75_LL4)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C75", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC67 <- summary(C67_C75_LL4)
confintC67 <- confint(C67_C75_LL4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C75", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC7 <- summary(C68_C75_LL4)
confintC7 <- confint(C68_C75_LL4)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C75", 4)

summary_DR4 <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR4 , file = "Docs/summary_GC_C75.csv", row.names = FALSE)


# Concentration 5

#Splitting populations per conc

REF_C100 <- filter(DataGC, pop == "REF", conc =="100")
C76_C100 <- filter(DataGC, pop == "C76",conc =="100")
C67_C100 <- filter(DataGC, pop == "C67",conc =="100")
C68_C100 <- filter(DataGC, pop == "C68",conc =="100")

# Adjusting the models

REF_C100_LL4 <- drm(abs~tempo, data = REF_C100 , fct =  LL.4())
plot(REF_C100_LL4, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C100_LL4)

C76_C100_LL4 <- drm(abs~ tempo, data = C76_C100 , fct =  LL.4())
plot(C76_C100_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C100_LL4 <- drm(abs~ tempo, data = C67_C100 , fct =  LL.4())
plot(C67_C100_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C100_LL4 <- drm(abs~ tempo, data = C68_C100 , fct =  LL.4())
plot(C68_C100_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C100 analysis

newdata_REF_C100 <- expand.grid(tempo = seq(min(REF_C100$tempo), max(REF_C100$tempo), length.out = 200))
newdata_REF_C100$abs <- predict(REF_C100_LL4, newdata_REF_C100, type = 'response')
newdata_REF_C100$pop <- "REF"

newdata_C76_C100 <- expand.grid(tempo = seq(min(C76_C100$tempo), max(C76_C100$tempo), length.out = 200))
newdata_C76_C100$abs <- predict(C76_C100_LL4, newdata_C76_C100, type = 'response')
newdata_C76_C100$pop <- "C76"

newdata_C67_C100 <- expand.grid(tempo = seq(min(C67_C100$tempo), max(C67_C100$tempo), length.out = 200))
newdata_C67_C100$abs <- predict(C67_C100_LL4, newdata_C67_C100, type = 'response')
newdata_C67_C100$pop <- "C67"

newdata_C68_C100 <- expand.grid(tempo = seq(min(C68_C100$tempo), max(C68_C100$tempo), length.out = 200))
newdata_C68_C100$abs <- predict(C68_C100_LL4, newdata_C68_C100, type = 'response')
newdata_C68_C100$pop <- "C68"

## Exploring predicted model

GrowthCurves_C100 <- full_join(REF_C100, C67_C100)
GrowthCurves_C100 <- full_join(GrowthCurves_C100, C76_C100)
GrowthCurves_C100 <- full_join(GrowthCurves_C100, C68_C100)

newdata_full <- rbind.data.frame(newdata_REF_C100, newdata_C67_C100)
newdata_full <- rbind(newdata_full, newdata_C76_C100)
newdata_full <- rbind(newdata_full, newdata_C68_C100)


DoseResponseCurves_01 <- ggplot() +
  # geom_point(data = REF_C100, aes(x = tempo, y = abs)) +
  # geom_line(data = newdata_REF_C100, aes(x = tempo, y = newdata_REF_C100$abs)) +
  geom_point(data = C76_C100, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C76_C100, aes(x = tempo, y = newdata_C76_C100$abs))+
  geom_point(data = C67_C100, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C67_C100, aes(x = tempo, y = newdata_C67_C100$abs))+
  geom_point(data = C68_C100, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C68_C100, aes(x = tempo, y = newdata_C68_C100$abs)) +
  labs(x = "Tempo", y = "Abs (600nm)") +
  scale_x_continuous(breaks = c(24, 48, 72, 96, 120),
                     labels = c("24h", "48h", "72h", "96h", "120h")) +
  theme_bw() +
  theme( text = element_text(size = 18, face = "bold")) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")), nrow = 1)

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_100.png")



summary(REF_C100_LL4)
# Extracting measures

COEFICIENTS <- c("GrowthRate", "c","d","e")
modelREF <- summary(REF_C100_LL4)
confintREF <- confint(REF_C100_LL4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C100", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC76 <- summary(C76_C100_LL4)
confintC76 <- confint(C76_C100_LL4)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C100", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC67 <- summary(C67_C100_LL4)
confintC67 <- confint(C67_C100_LL4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C100", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC7 <- summary(C68_C100_LL4)
confintC7 <- confint(C68_C100_LL4)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C100", 4)

summary_DR5 <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR5 , file = "Docs/summary_GC_C100.csv", row.names = FALSE)



# Concentration 6

#Splitting populations per conc

REF_C150 <- filter(DataGC, pop == "REF", conc =="150")
C76_C150 <- filter(DataGC, pop == "C76",conc =="150")
C67_C150 <- filter(DataGC, pop == "C67",conc =="150")
C68_C150 <- filter(DataGC, pop == "C68",conc =="150")

# Adjusting the models

REF_C150_LL4 <- drm(abs~tempo, data = REF_C150 , fct =  LL.4())
plot(REF_C150_LL4, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C150_LL4)

C76_C150_LL4 <- drm(abs~ tempo, data = C76_C150 , fct =  LL.4())
plot(C76_C150_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C150_LL4 <- drm(abs~ tempo, data = C67_C150 , fct =  LL.4())
plot(C67_C150_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C150_LL4 <- drm(abs~ tempo, data = C68_C150 , fct =  LL.4())
plot(C68_C150_LL4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C150 analysis

newdata_REF_C150 <- expand.grid(tempo = seq(min(REF_C150$tempo), max(REF_C150$tempo), length.out = 200))
newdata_REF_C150$abs <- predict(REF_C150_LL4, newdata_REF_C150, type = 'response')
newdata_REF_C150$pop <- "REF"

newdata_C76_C150 <- expand.grid(tempo = seq(min(C76_C150$tempo), max(C76_C150$tempo), length.out = 200))
newdata_C76_C150$abs <- predict(C76_C150_LL4, newdata_C76_C150, type = 'response')
newdata_C76_C150$pop <- "C76"

newdata_C67_C150 <- expand.grid(tempo = seq(min(C67_C150$tempo), max(C67_C150$tempo), length.out = 200))
newdata_C67_C150$abs <- predict(C67_C150_LL4, newdata_C67_C150, type = 'response')
newdata_C67_C150$pop <- "C67"

newdata_C68_C150 <- expand.grid(tempo = seq(min(C68_C150$tempo), max(C68_C150$tempo), length.out = 200))
newdata_C68_C150$abs <- predict(C68_C150_LL4, newdata_C68_C150, type = 'response')
newdata_C68_C150$pop <- "C68"

## Exploring predicted model

GrowthCurves_C150 <- full_join(REF_C150, C67_C150)
GrowthCurves_C150 <- full_join(GrowthCurves_C150, C76_C150)
GrowthCurves_C150 <- full_join(GrowthCurves_C150, C68_C150)

newdata_full <- rbind.data.frame(newdata_REF_C150, newdata_C67_C150)
newdata_full <- rbind(newdata_full, newdata_C76_C150)
newdata_full <- rbind(newdata_full, newdata_C68_C150)


DoseResponseCurves_01 <- ggplot() +
  geom_point(data = REF_C150, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_REF_C150, aes(x = tempo, y = newdata_REF_C150$abs)) +
  geom_point(data = C76_C150, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C76_C150, aes(x = tempo, y = newdata_C76_C150$abs))+
  geom_point(data = C67_C150, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C67_C150, aes(x = tempo, y = newdata_C67_C150$abs))+
  geom_point(data = C68_C150, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C68_C150, aes(x = tempo, y = newdata_C68_C150$abs)) +
  labs(x = "Tempo", y = "Abs (600nm)") +
  scale_x_continuous(breaks = c(24, 48, 72, 96, 120),
                     labels = c("24h", "48h", "72h", "96h", "120h")) +
  theme_bw() +
  theme( text = element_text(size = 18, face = "bold")) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")), nrow = 1)

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_150.png")



summary(REF_C150_LL4)
# Extracting measures

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelREF <- summary(REF_C150_LL4)
confintREF <- confint(REF_C150_LL4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C150", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC76 <- summary(C76_C150_LL4)
confintC76 <- confint(C76_C150_LL4)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C150", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC67 <- summary(C67_C150_LL4)
confintC67 <- confint(C67_C150_LL4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C150", 4)

COEFICIENTS <-  c("GrowthRate", "c","d","e")
modelC7 <- summary(C68_C150_LL4)
confintC7 <- confint(C68_C150_LL4)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C150", 4)

summary_DR6 <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR6, file = "Docs/summary_GC_C150.csv", row.names = FALSE)

# Tempo de duplicação


TempoDuplicacao  <- function(r, k, li){
  DT <- (-1)*(log(((k-(2*li))/(2*(k-li))))/r)
  return(DT)
}

DataGCSUM <- DataGC%>%
  group_by(conc, pop, tempo)%>%
  summarise(mean_value = mean(abs), sd_value = sd(abs))
DataGCSUM <- DataGCSUM%>%
  group_by(conc, pop)%>%
  summarise(minimo = min(mean_value), maximo = max(mean_value))

taxasCresc<- rbind(summary_DR1,summary_DR2, summary_DR3,
                   summary_DR4, summary_DR5, summary_DR6)

taxasCresc <- taxasCresc%>%
  filter(coef == "GrowthRate")

taxasCresc <- taxasCresc%>%
  tidyr::separate(pop, into = c("pop","conc"))%>%
  mutate(conc = stringr::str_replace(conc,"C", ""))

taxasCresc$conc <- as.factor(taxasCresc$conc)
DataGCSUM$conc <- as.factor( DataGCSUM$conc)

resumo <- left_join(taxasCresc, DataGCSUM, by = c("pop", "conc"))

for (i in 1:nrow(resumo)){
  resumo$TempDupliDias[i] <- abs(TempoDuplicacao(resumo$Estimate[i],
                                         0.5,
                                         resumo$minimo[i]))
  resumo$TempDupliHoras[i] <- abs(TempoDuplicacao(resumo$Estimate[i],
                                         0.5,
                                         resumo$minimo[i])*24)
}

write.csv(resumo, file = "Docs/ResumoTaxaeDT.csv", row.names = FALSE)
