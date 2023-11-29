################################################################################
################ Promastigote Growth Curve Dose-response analysis ##############
################################################################################

library(dplyr)
library(ggplot2) 
library(drc)
library(gridExtra)
library(patchwork)

# Carregamento dos dados----------------------------------

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

# Ajuste do modelo C0  -------------------------

REF_C0_LL2 <- drm(abs~tempo, data = REF_C0 , fct =  LL.2())
plot(REF_C0_LL2, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C0_LL2)

C76_C0_LL2 <- drm(abs~ tempo, data = C76_C0 , fct =  LL.2())
plot(C76_C0_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C0_LL2 <- drm(abs~ tempo, data = C67_C0 , fct =  LL.2())
plot(C67_C0_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C0_LL2 <- drm(abs~ tempo, data = C68_C0 , fct =  LL.2())
plot(C68_C0_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C0 analysis

newdata_REF_C0 <- expand.grid(tempo = seq(min(REF_C0$tempo), max(REF_C0$tempo), length.out = 200))
newdata_REF_C0$abs <- predict(REF_C0_LL2, newdata_REF_C0, type = 'response')
newdata_REF_C0$pop <- "REF"

newdata_C76_C0 <- expand.grid(tempo = seq(min(C76_C0$tempo), max(C76_C0$tempo), length.out = 200))
newdata_C76_C0$abs <- predict(C76_C0_LL2, newdata_C76_C0, type = 'response')
newdata_C76_C0$pop <- "C76" 

newdata_C67_C0 <- expand.grid(tempo = seq(min(C67_C0$tempo), max(C67_C0$tempo), length.out = 200))
newdata_C67_C0$abs <- predict(C67_C0_LL2, newdata_C67_C0, type = 'response')
newdata_C67_C0$pop <- "C67"

newdata_C68_C0 <- expand.grid(tempo = seq(min(C68_C0$tempo), max(C68_C0$tempo), length.out = 200))
newdata_C68_C0$abs <- predict(C68_C0_LL2, newdata_C68_C0, type = 'response')
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
  labs(x = "Tempo", y = "Abs (600nm)") +
  scale_x_continuous(labels = c("24h", "48h", "72h", "96h"))+
  theme_bw() +
  theme( plot.title = element_text(size = 14, face = "bold"),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15)) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")))

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_0.png")

summary(REF_C0_LL2)
# Extração das estimativas C0 -----------------------------------

COEFICIENTS <- c("GrowthRate", "e")
modelREF <- summary(REF_C0_LL2)
confintREF <- confint(REF_C0_LL2)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C0", 4)

COEFICIENTS <- c("GrowthRate", "e")
modelC76 <- summary(C76_C0_LL2)
confintC76 <- confint(C76_C0_LL2)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C0", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC67 <- summary(C67_C0_LL2)
confintC67 <- confint(C67_C0_LL2)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C0", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC7 <- summary(C68_C0_LL2)
confintC7 <- confint(C68_C0_LL2)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C0", 4)

summary_DR <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR , file = "Docs/summary_GC_C0.csv", row.names = FALSE)

# Comparação dos modelos C0 ----------------------------------------

m1<-drm(abs ~ tempo*conc, pop, data = DataGC, 
        fct =LL.2(names = c("GrowthRate", "e")))
m2<-drm(abs ~ tempo*conc, data = DataGC, 
        fct =LL.2(names = c("GrowthRate", "e")))

anova(m1,m2) 

# Diagnóstico do ajuste ------------------------------------

plot1 <- ggplot(REF_C0, aes(x = fitted(REF_C0_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " REF - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(REF_C0, aes(x = fitted(REF_C0_LL2), y = residuals(REF_C0_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "REF - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(REF_C0, aes(sample = residuals(REF_C0_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

REF_PGPA_C0 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
REF_PGPA_C0
ggsave("Figuras/Fit_REF_C0.png", REF_PGPA_C0)

plot1 <- ggplot(C76_C0, aes(x = fitted(C76_C0_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C76 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C76_C0, aes(x = fitted(C76_C0_LL2), y = residuals(C76_C0_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C76 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C76_C0, aes(sample = residuals(C76_C0_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C76_PGPA_C0 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C76_PGPA_C0
ggsave("Figuras/Fit_C76_C0.png", C76_PGPA_C0)

plot1 <- ggplot(C67_C0, aes(x = fitted(C67_C0_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C67 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C67_C0, aes(x = fitted(C67_C0_LL2), y = residuals(C67_C0_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C67 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C67_C0, aes(sample = residuals(C67_C0_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C67_PGPA_C0 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C67_PGPA_C0
ggsave("Figuras/Fit_C67_C0.png", C67_PGPA_C0)

plot1 <- ggplot(C68_C0, aes(x = fitted(C68_C0_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C68 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C68_C0, aes(x = fitted(C68_C0_LL2), y = residuals(C68_C0_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C68 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C68_C0, aes(sample = residuals(C68_C0_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C68_PGPA_C0 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C68_PGPA_C0
ggsave("Figuras/Fit_C68_C0.png", C68_PGPA_C0)

# Ajuste do modelo C10 -------------------------------------------

#Splitting populations per conc

REF_C10 <- filter(DataGC, pop == "REF", conc =="10")
C76_C10 <- filter(DataGC, pop == "C76",conc =="10")
C67_C10 <- filter(DataGC, pop == "C67",conc =="10")
C68_C10 <- filter(DataGC, pop == "C68",conc =="10")

# Adjusting the models

REF_C10_LL2 <- drm(abs~tempo, data = REF_C10 , fct =  LL.2())
plot(REF_C10_LL2, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C10_LL2)

C76_C10_LL2 <- drm(abs~ tempo, data = C76_C10 , fct =  LL.2())
plot(C76_C10_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C10_LL2 <- drm(abs~ tempo, data = C67_C10 , fct =  LL.2())
plot(C67_C10_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C10_LL2 <- drm(abs~ tempo, data = C68_C10 , fct =  LL.2())
plot(C68_C10_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C10 analysis

newdata_REF_C10 <- expand.grid(tempo = seq(min(REF_C10$tempo), max(REF_C10$tempo), length.out = 200))
newdata_REF_C10$abs <- predict(REF_C10_LL2, newdata_REF_C10, type = 'response')
newdata_REF_C10$pop <- "REF"

newdata_C76_C10 <- expand.grid(tempo = seq(min(C76_C10$tempo), max(C76_C10$tempo), length.out = 200))
newdata_C76_C10$abs <- predict(C76_C10_LL2, newdata_C76_C10, type = 'response')
newdata_C76_C10$pop <- "C76"

newdata_C67_C10 <- expand.grid(tempo = seq(min(C67_C10$tempo), max(C67_C10$tempo), length.out = 200))
newdata_C67_C10$abs <- predict(C67_C10_LL2, newdata_C67_C10, type = 'response')
newdata_C67_C10$pop <- "C67"

newdata_C68_C10 <- expand.grid(tempo = seq(min(C68_C10$tempo), max(C68_C10$tempo), length.out = 200))
newdata_C68_C10$abs <- predict(C68_C10_LL2, newdata_C68_C10, type = 'response')
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
  scale_x_continuous(labels = c("24h", "48h", "72h", "96h"))+
  theme_bw() +
  theme( plot.title = element_text(size = 14, face = "bold"),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15)) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")))

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_10.png")



summary(REF_C10_LL2)
# Extracting measures

COEFICIENTS <-  c("GrowthRate", "e")
modelREF <- summary(REF_C10_LL2)
confintREF <- confint(REF_C10_LL2)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C10", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC76 <- summary(C76_C10_LL2)
confintC76 <- confint(C76_C10_LL2)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C10", 4)

COEFICIENTS <- c("GrowthRate", "e")
modelC67 <- summary(C67_C10_LL2)
confintC67 <- confint(C67_C10_LL2)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C10",4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC7 <- summary(C68_C10_LL2)
confintC7 <- confint(C68_C10_LL2)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C10", 4)

summary_DR <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR , file = "Docs/summary_GC_C10.csv", row.names = FALSE)

# Diagnóstico do ajuste C10 ------------------------------------

plot1 <- ggplot(REF_C10, aes(x = fitted(REF_C10_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " REF - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(REF_C10, aes(x = fitted(REF_C10_LL2), y = residuals(REF_C10_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "REF - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(REF_C10, aes(sample = residuals(REF_C10_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

REF_PGPA_C10 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
REF_PGPA_C10
ggsave("Figuras/Fit_REF_C10.png", REF_PGPA_C10)

plot1 <- ggplot(C76_C10, aes(x = fitted(C76_C10_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C76 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C76_C10, aes(x = fitted(C76_C10_LL2), y = residuals(C76_C10_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C76 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C76_C10, aes(sample = residuals(C76_C10_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C76_PGPA_C10 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C76_PGPA_C10
ggsave("Figuras/Fit_C76_C10.png", C76_PGPA_C10)

plot1 <- ggplot(C67_C10, aes(x = fitted(C67_C10_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C67 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C67_C10, aes(x = fitted(C67_C10_LL2), y = residuals(C67_C10_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C67 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C67_C10, aes(sample = residuals(C67_C10_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C67_PGPA_C10 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C67_PGPA_C10
ggsave("Figuras/Fit_C67_C10.png", C67_PGPA_C10)

plot1 <- ggplot(C68_C10, aes(x = fitted(C68_C10_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C68 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C68_C10, aes(x = fitted(C68_C10_LL2), y = residuals(C68_C10_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C68 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C68_C10, aes(sample = residuals(C68_C10_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C68_PGPA_C10 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C68_PGPA_C10
ggsave("Figuras/Fit_C68_C10.png", C68_PGPA_C10)

# Ajuste do modelo C50 --------------------------------------

#Splitting populations per conc

REF_C50 <- filter(DataGC, pop == "REF", conc =="50")
C76_C50 <- filter(DataGC, pop == "C76",conc =="50")
C67_C50 <- filter(DataGC, pop == "C67",conc =="50")
C68_C50 <- filter(DataGC, pop == "C68",conc =="50")

# Adjusting the models

REF_C50_LL2 <- drm(abs~tempo, data = REF_C50 , fct =  LL.2())
plot(REF_C50_LL2, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C50_LL2)

C76_C50_LL2 <- drm(abs~ tempo, data = C76_C50 , fct =  LL.2())
plot(C76_C50_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C50_LL2 <- drm(abs~ tempo, data = C67_C50 , fct =  LL.2())
plot(C67_C50_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C50_LL2 <- drm(abs~ tempo, data = C68_C50 , fct =  LL.2())
plot(C68_C50_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C50 analysis

newdata_REF_C50 <- expand.grid(tempo = seq(min(REF_C50$tempo), max(REF_C50$tempo), length.out = 200))
newdata_REF_C50$abs <- predict(REF_C50_LL2, newdata_REF_C50, type = 'response')
newdata_REF_C50$pop <- "REF"

newdata_C76_C50 <- expand.grid(tempo = seq(min(C76_C50$tempo), max(C76_C50$tempo), length.out = 200))
newdata_C76_C50$abs <- predict(C76_C50_LL2, newdata_C76_C50, type = 'response')
newdata_C76_C50$pop <- "C76"

newdata_C67_C50 <- expand.grid(tempo = seq(min(C67_C50$tempo), max(C67_C50$tempo), length.out = 200))
newdata_C67_C50$abs <- predict(C67_C50_LL2, newdata_C67_C50, type = 'response')
newdata_C67_C50$pop <- "C67"

newdata_C68_C50 <- expand.grid(tempo = seq(min(C68_C50$tempo), max(C68_C50$tempo), length.out = 200))
newdata_C68_C50$abs <- predict(C68_C50_LL2, newdata_C68_C50, type = 'response')
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
  scale_x_continuous(labels = c("24h", "48h", "72h", "96h"))+
  theme_bw() +
  theme( plot.title = element_text(size = 14, face = "bold"),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15)) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")))

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_10.png")



summary(REF_C50_LL2)
# Extracting measures

COEFICIENTS <-  c("GrowthRate", "e")
modelREF <- summary(REF_C50_LL2)
confintREF <- confint(REF_C50_LL2)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C50", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC76 <- summary(C76_C50_LL2)
confintC76 <- confint(C76_C50_LL2)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C50", 4)

COEFICIENTS <- c("GrowthRate", "e")
modelC67 <- summary(C67_C50_LL2)
confintC67 <- confint(C67_C50_LL2)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C50", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC7 <- summary(C68_C50_LL2)
confintC7 <- confint(C68_C50_LL2)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C50", 4)

summary_DR <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR , file = "Docs/summary_GC_C50.csv", row.names = FALSE)

# Diagnóstico do modelo C50 ------------------------------------

plot1 <- ggplot(REF_C50, aes(x = fitted(REF_C50_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " REF - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(REF_C50, aes(x = fitted(REF_C50_LL2), y = residuals(REF_C50_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "REF - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(REF_C50, aes(sample = residuals(REF_C50_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

REF_PGPA_C50 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
REF_PGPA_C50
ggsave("Figuras/Fit_REF_C50.png", REF_PGPA_C50)

plot1 <- ggplot(C76_C50, aes(x = fitted(C76_C50_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C76 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C76_C50, aes(x = fitted(C76_C50_LL2), y = residuals(C76_C50_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C76 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C76_C50, aes(sample = residuals(C76_C50_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C76_PGPA_C50 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C76_PGPA_C50
ggsave("Figuras/Fit_C76_C50.png", C76_PGPA_C50)

plot1 <- ggplot(C67_C50, aes(x = fitted(C67_C50_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C67 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C67_C50, aes(x = fitted(C67_C50_LL2), y = residuals(C67_C50_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C67 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C67_C50, aes(sample = residuals(C67_C50_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C67_PGPA_C50 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C67_PGPA_C50
ggsave("Figuras/Fit_C67_C50.png", C67_PGPA_C50)

plot1 <- ggplot(C68_C50, aes(x = fitted(C68_C50_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C68 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C68_C50, aes(x = fitted(C68_C50_LL2), y = residuals(C68_C50_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C68 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C68_C50, aes(sample = residuals(C68_C50_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C68_PGPA_C50 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C68_PGPA_C50
ggsave("Figuras/Fit_C68_C50.png", C68_PGPA_C50)
# Ajuste do modelo C75 ------------------------------------

#Splitting populations per conc

REF_C75 <- filter(DataGC, pop == "REF", conc =="75")
C76_C75 <- filter(DataGC, pop == "C76",conc =="75")
C67_C75 <- filter(DataGC, pop == "C67",conc =="75")
C68_C75 <- filter(DataGC, pop == "C68",conc =="75")

# Adjusting the models

REF_C75_LL2 <- drm(abs~tempo, data = REF_C75 , fct =  LL.2())
plot(REF_C75_LL2, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C75_LL2)

C76_C75_LL2 <- drm(abs~ tempo, data = C76_C75 , fct =  LL.2())
plot(C76_C75_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C75_LL2 <- drm(abs~ tempo, data = C67_C75 , fct =  LL.2())
plot(C67_C75_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C75_LL2 <- drm(abs~ tempo, data = C68_C75 , fct =  LL.2())
plot(C68_C75_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C75 analysis

newdata_REF_C75 <- expand.grid(tempo = seq(min(REF_C75$tempo), max(REF_C75$tempo), length.out = 200))
newdata_REF_C75$abs <- predict(REF_C75_LL2, newdata_REF_C75, type = 'response')
newdata_REF_C75$pop <- "REF"

newdata_C76_C75 <- expand.grid(tempo = seq(min(C76_C75$tempo), max(C76_C75$tempo), length.out = 200))
newdata_C76_C75$abs <- predict(C76_C75_LL2, newdata_C76_C75, type = 'response')
newdata_C76_C75$pop <- "C76"

newdata_C67_C75 <- expand.grid(tempo = seq(min(C67_C75$tempo), max(C67_C75$tempo), length.out = 200))
newdata_C67_C75$abs <- predict(C67_C75_LL2, newdata_C67_C75, type = 'response')
newdata_C67_C75$pop <- "C67"

newdata_C68_C75 <- expand.grid(tempo = seq(min(C68_C75$tempo), max(C68_C75$tempo), length.out = 200))
newdata_C68_C75$abs <- predict(C68_C75_LL2, newdata_C68_C75, type = 'response')
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
  scale_x_continuous(labels = c("24h", "48h", "72h", "96h"))+
  theme_bw() +
  theme( plot.title = element_text(size = 14, face = "bold"),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15)) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")))

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_10.png")



summary(REF_C75_LL2)
# Extracting measures

COEFICIENTS <-  c("GrowthRate", "e")
modelREF <- summary(REF_C75_LL2)
confintREF <- confint(REF_C75_LL2)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C75", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC76 <- summary(C76_C75_LL2)
confintC76 <- confint(C76_C75_LL2)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C75", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC67 <- summary(C67_C75_LL2)
confintC67 <- confint(C67_C75_LL2)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C75", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC7 <- summary(C68_C75_LL2)
confintC7 <- confint(C68_C75_LL2)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C75", 4)

summary_DR <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR , file = "Docs/summary_GC_C75.csv", row.names = FALSE)

#  Diagnóstico do ajuste C75 ------------------------------------

plot1 <- ggplot(REF_C75, aes(x = fitted(REF_C75_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " REF - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(REF_C75, aes(x = fitted(REF_C75_LL2), y = residuals(REF_C75_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "REF - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(REF_C75, aes(sample = residuals(REF_C75_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

REF_PGPA_C75 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
REF_PGPA_C75
ggsave("Figuras/Fit_REF_C75.png", REF_PGPA_C75)

plot1 <- ggplot(C76_C75, aes(x = fitted(C76_C75_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C76 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C76_C75, aes(x = fitted(C76_C75_LL2), y = residuals(C76_C75_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C76 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C76_C75, aes(sample = residuals(C76_C75_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C76_PGPA_C75 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C76_PGPA_C75
ggsave("Figuras/Fit_C76_C75.png", C76_PGPA_C75)

plot1 <- ggplot(C67_C75, aes(x = fitted(C67_C75_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C67 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C67_C75, aes(x = fitted(C67_C75_LL2), y = residuals(C67_C75_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C67 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C67_C75, aes(sample = residuals(C67_C75_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C67_PGPA_C75 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C67_PGPA_C75
ggsave("Figuras/Fit_C67_C75.png", C67_PGPA_C75)

plot1 <- ggplot(C68_C75, aes(x = fitted(C68_C75_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C68 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C68_C75, aes(x = fitted(C68_C75_LL2), y = residuals(C68_C75_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C68 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C68_C75, aes(sample = residuals(C68_C75_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C68_PGPA_C75 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C68_PGPA_C75
ggsave("Figuras/Fit_C68_C75.png", C68_PGPA_C75)
# Ajuste C100 -------------------

#Splitting populations per conc

REF_C100 <- filter(DataGC, pop == "REF", conc =="100")
C76_C100 <- filter(DataGC, pop == "C76",conc =="100")
C67_C100 <- filter(DataGC, pop == "C67",conc =="100")
C68_C100 <- filter(DataGC, pop == "C68",conc =="100")

# Adjusting the models

REF_C100_LL2 <- drm(abs~tempo, data = REF_C100 , fct =  LL.2())
plot(REF_C100_LL2, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C100_LL2)

C76_C100_LL2 <- drm(abs~ tempo, data = C76_C100 , fct =  LL.2())
plot(C76_C100_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C100_LL2 <- drm(abs~ tempo, data = C67_C100 , fct =  LL.2())
plot(C67_C100_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C100_LL2 <- drm(abs~ tempo, data = C68_C100 , fct =  LL.2())
plot(C68_C100_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C100 analysis

newdata_REF_C100 <- expand.grid(tempo = seq(min(REF_C100$tempo), max(REF_C100$tempo), length.out = 200))
newdata_REF_C100$abs <- predict(REF_C100_LL2, newdata_REF_C100, type = 'response')
newdata_REF_C100$pop <- "REF"

newdata_C76_C100 <- expand.grid(tempo = seq(min(C76_C100$tempo), max(C76_C100$tempo), length.out = 200))
newdata_C76_C100$abs <- predict(C76_C100_LL2, newdata_C76_C100, type = 'response')
newdata_C76_C100$pop <- "C76"

newdata_C67_C100 <- expand.grid(tempo = seq(min(C67_C100$tempo), max(C67_C100$tempo), length.out = 200))
newdata_C67_C100$abs <- predict(C67_C100_LL2, newdata_C67_C100, type = 'response')
newdata_C67_C100$pop <- "C67"

newdata_C68_C100 <- expand.grid(tempo = seq(min(C68_C100$tempo), max(C68_C100$tempo), length.out = 200))
newdata_C68_C100$abs <- predict(C68_C100_LL2, newdata_C68_C100, type = 'response')
newdata_C68_C100$pop <- "C68"

## Exploring predicted model

GrowthCurves_C100 <- full_join(REF_C100, C67_C100)
GrowthCurves_C100 <- full_join(GrowthCurves_C100, C76_C100)
GrowthCurves_C100 <- full_join(GrowthCurves_C100, C68_C100)

newdata_full <- rbind.data.frame(newdata_REF_C100, newdata_C67_C100)
newdata_full <- rbind(newdata_full, newdata_C76_C100)
newdata_full <- rbind(newdata_full, newdata_C68_C100)


DoseResponseCurves_01 <- ggplot() +
  geom_point(data = REF_C100, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_REF_C100, aes(x = tempo, y = newdata_REF_C100$abs)) +
  geom_point(data = C76_C100, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C76_C100, aes(x = tempo, y = newdata_C76_C100$abs))+
  geom_point(data = C67_C100, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C67_C100, aes(x = tempo, y = newdata_C67_C100$abs))+
  geom_point(data = C68_C100, aes(x = tempo, y = abs)) +
  geom_line(data = newdata_C68_C100, aes(x = tempo, y = newdata_C68_C100$abs)) +
  labs(x = "Tempo", y = "Abs (600nm)") +
  scale_x_continuous(labels = c("24h", "48h", "72h", "96h"))+
  theme_bw() +
  theme( plot.title = element_text(size = 14, face = "bold"),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15)) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")))

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_10.png")



summary(REF_C100_LL2)
# Extracting measures

COEFICIENTS <- c("GrowthRate", "e")
modelREF <- summary(REF_C100_LL2)
confintREF <- confint(REF_C100_LL2)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C100", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC76 <- summary(C76_C100_LL2)
confintC76 <- confint(C76_C100_LL2)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C100", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC67 <- summary(C67_C100_LL2)
confintC67 <- confint(C67_C100_LL2)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C100", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC7 <- summary(C68_C100_LL2)
confintC7 <- confint(C68_C100_LL2)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C100", 4)

summary_DR <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR , file = "Docs/summary_GC_C100.csv", row.names = FALSE)

# Diagnóstico do ajuste C100 ------------------------------------

plot1 <- ggplot(REF_C100, aes(x = fitted(REF_C100_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " REF - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(REF_C100, aes(x = fitted(REF_C100_LL2), y = residuals(REF_C100_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "REF - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(REF_C100, aes(sample = residuals(REF_C100_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

REF_PGPA_C100 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
REF_PGPA_C100
ggsave("Figuras/Fit_REF_C100.png", REF_PGPA_C100)

plot1 <- ggplot(C76_C100, aes(x = fitted(C76_C100_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C76 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C76_C100, aes(x = fitted(C76_C100_LL2), y = residuals(C76_C100_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C76 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C76_C100, aes(sample = residuals(C76_C100_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C76_PGPA_C100 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C76_PGPA_C100
ggsave("Figuras/Fit_C76_C100.png", C76_PGPA_C100)

plot1 <- ggplot(C67_C100, aes(x = fitted(C67_C100_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C67 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C67_C100, aes(x = fitted(C67_C100_LL2), y = residuals(C67_C100_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C67 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C67_C100, aes(sample = residuals(C67_C100_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C67_PGPA_C100 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C67_PGPA_C100
ggsave("Figuras/Fit_C67_C100.png", C67_PGPA_C100)

plot1 <- ggplot(C68_C100, aes(x = fitted(C68_C100_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C68 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C68_C100, aes(x = fitted(C68_C100_LL2), y = residuals(C68_C100_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C68 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C68_C100, aes(sample = residuals(C68_C100_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C68_PGPA_C100 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C68_PGPA_C100
ggsave("Figuras/Fit_C68_C100.png", C68_PGPA_C100)
# Concentration 6 -----------------------

#Splitting populations per conc

REF_C150 <- filter(DataGC, pop == "REF", conc =="150")
C76_C150 <- filter(DataGC, pop == "C76",conc =="150")
C67_C150 <- filter(DataGC, pop == "C67",conc =="150")
C68_C150 <- filter(DataGC, pop == "C68",conc =="150")

# Adjusting the models

REF_C150_LL2 <- drm(abs~tempo, data = REF_C150 , fct =  LL.2())
plot(REF_C150_LL2, 
     broken = TRUE, gridsize = 100, xlab = "Tempo(dias)", 
     ylab = "Abs", type = "all")
summary(REF_C150_LL2)

C76_C150_LL2 <- drm(abs~ tempo, data = C76_C150 , fct =  LL.2())
plot(C76_C150_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C67_C150_LL2 <- drm(abs~ tempo, data = C67_C150 , fct =  LL.2())
plot(C67_C150_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

C68_C150_LL2 <- drm(abs~ tempo, data = C68_C150 , fct =  LL.2())
plot(C68_C150_LL2, broken = TRUE, gridsize = 100, xlab = "Tempo (dias)", 
     ylab = "Abs", type = "all")

# C150 analysis

newdata_REF_C150 <- expand.grid(tempo = seq(min(REF_C150$tempo), max(REF_C150$tempo), length.out = 200))
newdata_REF_C150$abs <- predict(REF_C150_LL2, newdata_REF_C150, type = 'response')
newdata_REF_C150$pop <- "REF"

newdata_C76_C150 <- expand.grid(tempo = seq(min(C76_C150$tempo), max(C76_C150$tempo), length.out = 200))
newdata_C76_C150$abs <- predict(C76_C150_LL2, newdata_C76_C150, type = 'response')
newdata_C76_C150$pop <- "C76"

newdata_C67_C150 <- expand.grid(tempo = seq(min(C67_C150$tempo), max(C67_C150$tempo), length.out = 200))
newdata_C67_C150$abs <- predict(C67_C150_LL2, newdata_C67_C150, type = 'response')
newdata_C67_C150$pop <- "C67"

newdata_C68_C150 <- expand.grid(tempo = seq(min(C68_C150$tempo), max(C68_C150$tempo), length.out = 200))
newdata_C68_C150$abs <- predict(C68_C150_LL2, newdata_C68_C150, type = 'response')
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
  scale_x_continuous(labels = c("24h", "48h", "72h", "96h"))+
  theme_bw() +
  theme( plot.title = element_text(size = 14, face = "bold"),
         axis.text.x = element_text(size = 10),
         axis.text.y = element_text(size = 10),
         axis.title.x = element_text(size = 15),
         axis.title.y = element_text(size = 15)) +
  facet_wrap(~factor(pop, levels = c("REF", "C76", "C67", "C68")))

DoseResponseCurves_01
ggsave("Figures/01_DoseResponseCurves_CONC_10.png")



summary(REF_C150_LL2)
# Extracting measures

COEFICIENTS <-  c("GrowthRate", "e")
modelREF <- summary(REF_C150_LL2)
confintREF <- confint(REF_C150_LL2)
modelREF_coef <- as.data.frame(modelREF$coefficients)
modelREF_conf <- as.data.frame(confintREF)
modelREF_SUM <- cbind(modelREF_coef, modelREF_conf)
modelREF_SUM$coef <- COEFICIENTS 
modelREF_SUM$pop <- rep("REF_C150", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC76 <- summary(C76_C150_LL2)
confintC76 <- confint(C76_C150_LL2)
modelC76_coef <- as.data.frame(modelC76$coefficients)
modelC76_conf <- as.data.frame(confintC76)
modelC76_SUM <- cbind(modelC76_coef, modelC76_conf)
modelC76_SUM$coef <- COEFICIENTS 
modelC76_SUM$pop <- rep("C76_C150", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC67 <- summary(C67_C150_LL2)
confintC67 <- confint(C67_C150_LL2)
modelC67_coef <- as.data.frame(modelC67$coefficients)
modelC67_conf <- as.data.frame(confintC67)
modelC67_SUM <- cbind(modelC67_coef, modelC67_conf)
modelC67_SUM$coef <- COEFICIENTS 
modelC67_SUM$pop <- rep("C67_C150", 4)

COEFICIENTS <-  c("GrowthRate", "e")
modelC7 <- summary(C68_C150_LL2)
confintC7 <- confint(C68_C150_LL2)
modelC68_coef <- as.data.frame(modelC7$coefficients)
modelC68_conf <- as.data.frame(confintC7)
modelC68_SUM <- cbind(modelC68_coef, modelC68_conf)
modelC68_SUM$coef <- COEFICIENTS 
modelC68_SUM$pop <- rep("C68_C150", 4)

summary_DR <- rbind(modelREF_SUM,modelC76_SUM,
                    modelC68_SUM, modelC67_SUM)

write.csv(summary_DR , file = "Docs/summary_GC_C150.csv", row.names = FALSE)
# Diagnóstico do ajuste C150------------------


plot1 <- ggplot(REF_C150, aes(x = fitted(REF_C150_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " REF - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(REF_C150, aes(x = fitted(REF_C150_LL2), y = residuals(REF_C150_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "REF - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(REF_C150, aes(sample = residuals(REF_C150_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

REF_PGPA_C150 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
REF_PGPA_C150
ggsave("Figuras/Fit_REF_C150.png", REF_PGPA_C150)

plot1 <- ggplot(C76_C150, aes(x = fitted(C76_C150_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C76 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C76_C150, aes(x = fitted(C76_C150_LL2), y = residuals(C76_C150_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C76 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C76_C150, aes(sample = residuals(C76_C150_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C76_PGPA_C150 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C76_PGPA_C150
ggsave("Figuras/Fit_C76_C150.png", C76_PGPA_C150)

plot1 <- ggplot(C67_C150, aes(x = fitted(C67_C150_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C67 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C67_C150, aes(x = fitted(C67_C150_LL2), y = residuals(C67_C150_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C67 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C67_C150, aes(sample = residuals(C67_C150_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C67_PGPA_C150 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C67_PGPA_C150
ggsave("Figuras/Fit_C67_C150.png", C67_PGPA_C150)

plot1 <- ggplot(C68_C150, aes(x = fitted(C68_C150_LL2), y = abs)) +
  geom_point(pch = 16, cex = 2) +
  geom_abline(intercept = 0, slope = 1) +
  labs(title = " C68 - LL2\nAjustados x Observados", x = "Valores ajustados", 
       y = "Valores observados") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot2 <- ggplot(C68_C150, aes(x = fitted(C68_C150_LL2), y = residuals(C68_C150_LL2, type = "studentised"))) +
  geom_point(pch = 16, cex = 2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "C68 - LL2\nResíduos x Ajustados", x = "Valores ajustados", 
       y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

plot3 <- ggplot(C68_C150, aes(sample = residuals(C68_C150_LL2, type = "studentised"))) +
  geom_qq() +
  geom_qq_line() +
  labs(title = "LL2", x = "Quantis teóricos", y = "Resíduos") +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(face = "bold"),
        axis.text.x = element_text(size = 12), 
        axis.text.y = element_text(size = 12),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))

C68_PGPA_C150 <- (plot1+plot2+plot3) + plot_annotation(tag_levels = "A")
C68_PGPA_C150
ggsave("Figuras/Fit_C68_C150.png", C68_PGPA_C150)



