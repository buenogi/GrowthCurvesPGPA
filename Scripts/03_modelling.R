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


#Splitting populations per conc

REF_C0 <- filter(DataGC, pop == "REF", conc =="0")
C76_C0 <- filter(DataGC, pop == "C76",conc =="0")
C67_C0 <- filter(DataGC, pop == "C67",conc =="0")
C68_C0 <- filter(DataGC, pop == "C68",conc =="0")

# Adjusting the models

REF_C0_G4 <- drm(abs~tempo, data = REF_C0 , fct =  G.4())
plot(REF_C0_G4, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C0_G4)

C76_C0_G4 <- drm(abs~ tempo, data = C76_C0 , fct =  G.4())
plot(C76_C0_G4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C0_G4 <- drm(abs~ tempo, data = C67_C0 , fct =  G.4())
plot(C67_C0_G4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C0_G4 <- drm(abs~ tempo, data = C68_C0 , fct =  G.4())
plot(C68_C0_G4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C0 analysis

newdata_REF_C0 <- expand.grid(tempo = seq(min(REF_C0$tempo), max(REF_C0$tempo), length.out = 200))
newdata_REF_C0$abs <- predict(REF_C0_G4, newdata_REF_C0, type = 'response')
newdata_REF_C0$pop <- "REF"

newdata_C76_C0 <- expand.grid(tempo = seq(min(C76_C0$tempo), max(C76_C0$tempo), length.out = 200))
newdata_C76_C0$abs <- predict(C76_C0_G4, newdata_C76_C0, type = 'response')
newdata_C76_C0$pop <- "C76"

newdata_C67_C0 <- expand.grid(tempo = seq(min(C67_C0$tempo), max(C67_C0$tempo), length.out = 200))
newdata_C67_C0$abs <- predict(C67_C0_G4, newdata_C67_C0, type = 'response')
newdata_C67_C0$pop <- "C67"

newdata_C68_C0 <- expand.grid(tempo = seq(min(C68_C0$tempo), max(C68_C0$tempo), length.out = 200))
newdata_C68_C0$abs <- predict(C68_C0_G4, newdata_C68_C0, type = 'response')
newdata_C68_C0$pop <- "C68"

## Exploring predicted model

GrowthCurves_C0 <- full_join(REF_C0, C67_C0)
GrowthCurves_C0 <- full_join(GrowthCurves_C0, C76_C0)
GrowthCurves_C0 <- full_join(GrowthCurves_C0, C68_C0)

newdata_full <- rbind.data.frame(newdata_REF_C0, newdata_C67_C0)
newdata_full <- rbind(newdata_full, newdata_C76_C0)
newdata_full <- rbind(newdata_full, newdata_C68_C0)


DoseResponseCurves_01 <- ggplot() +
  geom_point(data = REF_C0, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_REF_C0, aes(x = tempo, y = newdata_REF_C0$abs)) +
  geom_point(data = C76_C0, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C76_C0, aes(x = tempo, y = newdata_C76_C0$abs))+
  geom_point(data = C67_C0, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C67_C0, aes(x = tempo, y = newdata_C67_C0$abs))+
  geom_point(data = C68_C0, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C68_C0, aes(x = tempo, y = newdata_C68_C0$abs)) +
  ggtitle("Promastigotes Growth") +
  labs(x = "Tempo (dias)", y = "Abs") +
  theme_bw() +
  theme( plot.title = element_text(size = 14, face = "bold"),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15)) +
  facet_wrap(~pop)

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_0.png")

summary(REF_C0_G4)
# Extracting measures

COEFICIENTS <- c("GrowthRate", "c", "d", "e")
modelREF <- summary(REF_C0_G4)
confintREF <- confint(REF_C0_G4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C0", 4)

COEFICIENTS <- c("GrowthRate", "c", "d", "e")
modelC76 <- summary(C76_C0_G4)
confintC76 <- confint(C76_C0_G4)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C0", 4)

COEFICIENTS <- c("GrowthRate", "c", "d", "e")
modelC67 <- summary(C67_C0_G4)
confintC67 <- confint(C67_C0_G4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C0", 4)

COEFICIENTS <- c("GrowthRate", "c", "d", "e")
modelC7 <- summary(C68_C0_G4)
confintC7 <- confint(C68_C0_G4)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C0", 4)

summary_DR <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR , file = "Docs/summary_GC_C76_C0.csv", row.names = FALSE)

# Models comparisons

m1<-drm(abs ~ tempo*conc, pop, data = DataGC, 
        fct =G.4(names = c("GrowthRate", "c", "d", "e")))
m2<-drm(abs ~ tempo*conc, data = DataGC, 
        fct =G.4(names = c("GrowthRate", "c", "d", "e")))

anova(m1,m2) 

# Concentration 2

#Splitting populations per conc

REF_C10 <- filter(DataGC, pop == "REF", conc =="10")
C76_C10 <- filter(DataGC, pop == "C76",conc =="10")
C67_C10 <- filter(DataGC, pop == "C67",conc =="10")
C68_C10 <- filter(DataGC, pop == "C68",conc =="10")

# Adjusting the models

REF_C10_G4 <- drm(abs~tempo, data = REF_C10 , fct =  G.4())
plot(REF_C10_G4, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C10_G4)

C76_C10_G4 <- drm(abs~ tempo, data = C76_C10 , fct =  G.4())
plot(C76_C10_G4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C10_G4 <- drm(abs~ tempo, data = C67_C10 , fct =  G.4())
plot(C67_C10_G4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C10_G4 <- drm(abs~ tempo, data = C68_C10 , fct =  G.4())
plot(C68_C10_G4, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C10 analysis

newdata_REF_C10 <- expand.grid(tempo = seq(min(REF_C10$tempo), max(REF_C10$tempo), length.out = 200))
newdata_REF_C10$abs <- predict(REF_C10_G4, newdata_REF_C10, type = 'response')
newdata_REF_C10$pop <- "REF"

newdata_C76_C10 <- expand.grid(tempo = seq(min(C76_C10$tempo), max(C76_C10$tempo), length.out = 200))
newdata_C76_C10$abs <- predict(C76_C10_G4, newdata_C76_C10, type = 'response')
newdata_C76_C10$pop <- "C76"

newdata_C67_C10 <- expand.grid(tempo = seq(min(C67_C10$tempo), max(C67_C10$tempo), length.out = 200))
newdata_C67_C10$abs <- predict(C67_C10_G4, newdata_C67_C10, type = 'response')
newdata_C67_C10$pop <- "C67"

newdata_C68_C10 <- expand.grid(tempo = seq(min(C68_C10$tempo), max(C68_C10$tempo), length.out = 200))
newdata_C68_C10$abs <- predict(C68_C10_G4, newdata_C68_C10, type = 'response')
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
  ggtitle("Promastigotes Growth") +
  labs(x = "Tempo (dias)", y = "Abs") +
  theme_bw() +
  theme( plot.title = element_text(size = 14, face = "bold"),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15)) +
  facet_wrap(~pop)

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_0.png")

summary(REF_C10_G4)
# Extracting measures

COEFICIENTS <- c("GrowthRate", "c", "d", "e")
modelREF <- summary(REF_C10_G4)
confintREF <- confint(REF_C10_G4)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C10", 4)

COEFICIENTS <- c("GrowthRate", "c", "d", "e")
modelC76 <- summary(C76_C10_G4)
confintC76 <- confint(C76_C10_G4)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C10", 4)

COEFICIENTS <- c("GrowthRate", "c", "d", "e")
modelC67 <- summary(C67_C10_G4)
confintC67 <- confint(C67_C10_G4)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C10", 4)

COEFICIENTS <- c("GrowthRate", "c", "d", "e")
modelC7 <- summary(C68_C10_G4)
confintC7 <- confint(C68_C10_G4)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C10", 4)

summary_DR <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR , file = "Docs/summary_GC_C76_C10.csv", row.names = FALSE)

# Models comparisons

m1<-drm(abs ~ tempo*conc, pop, data = DataGC, 
        fct =G.4(names = c("GrowthRate", "c", "d", "e")))
m2<-drm(abs ~ tempo*conc, data = DataGC, 
        fct =G.4(names = c("GrowthRate", "c", "d", "e")))

anova(m1,m2) 


m1<-drm(abs ~ tempo, conc,pop,  data = DataGC, 
        fct =G.4(names = c("GrowthRate", "c", "d", "e")))
m2<-drm(abs ~ tempo,conc,  data = DataGC, 
        fct =G.4(names = c("GrowthRate", "c", "d", "e")))

anova(m1,m2) 
