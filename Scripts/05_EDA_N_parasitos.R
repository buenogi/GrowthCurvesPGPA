################################################################################
############## Data cleaning  - Growth Curves PGPA HKO clones  #################
################################################################################
# Utilizando o número de parasitos

library(dplyr)
library(ggplot2)


#Loading data
DataGC <- read.csv(file = "Data/Processed/DataGC_N.csv")

# Checking classes

sapply(DataGC, class)

DataGC$experiment <- as.factor(DataGC$experiment)
DataGC$conc <- as.factor(DataGC$conc)
# EDA

DataGC$tempo <- as.factor(DataGC$tempo)

DataGC_SUM_exp <- DataGC%>%
  group_by(pop, conc, tempo, experiment)%>%
  summarise(nparasitos_exp = mean(n_parasitos), sd_value_n_parasitos_exp = sd(n_parasitos))

# Box-plot

GrowthC_DotPlot <- ggplot(DataGC_SUM_exp, aes(tempo, nparasitos_exp, 
                                              shape = conc, 
                                              color = experiment)) +
  geom_point()+
  ggtitle("PGPA Hemi mutants promastigotes growth with and without 150μM of SbIII") +
  labs(x = " Time (h)  ", y = "Abs")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  facet_wrap(~pop, nrow = 2)+
  theme_bw()

GrowthC_DotPlot

ggsave("Figuras/01_GrowthC_DotPlot.png")
# Sumarizing

DataGC_sum <- DataGC%>%
  group_by(pop, conc, tempo)%>%
  summarise(mean_value = mean(n_parasitos), sd_value = sd(n_parasitos))

GrowthC_plot_Clone <- ggplot(DataGC_sum, aes(tempo, log(mean_value), group = conc)) +
  geom_line(aes(color = conc))+
  ggtitle("PGPA Mutants promastigotes growth with and without 150μM of SbIII") +
  labs(x = " Time (h)  ", y = "Abs")+
  theme(plot.title = element_text(size = 14,face="bold"),
        axis.text.x = element_text(size = 10), 
        axis.text.y = element_text(size = 10),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15))+
  scale_x_discrete(labels = c("0h", "24h", "48h", "72h", "96h", "120h")) +
  facet_wrap(~pop, nrow = 2)+
  theme_bw()

GrowthC_plot_Clone
ggsave("Figuras/02_GrowthC_plot_Clone.png")

# Todas juntas


GrowthC_plot <- ggplot(DataGC_sum, aes(tempo, mean_value, group = pop)) +
  geom_line(aes(color  = pop), linewidth = 0.8)+
  ggtitle("PGPA KO and HKO Promastigotes growth with and without SbIII") +
  labs(x = " Tempo (h)  ", y = "Abs (600nm)")+
  theme(base_size = 25)+
  scale_x_discrete(labels = c("0h", "24h", "48h", "72h", "96h","120h")) +
  facet_wrap(DataGC_sum$conc, ncol = 2)+
  theme_bw( base_size = 25)

GrowthC_plot + labs(color = "Populations")
ggsave("Figuras/03_GrowthC_plot.png")

# install.packages("lme4")
# library(lme4)
# modelo_anova <- lmer(abs ~ pop * tempo * conc + (1 | experiment),
#                      data = DataGC)
# summary(modelo_anova)

