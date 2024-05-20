#####Hemocyanin measurament##########
library(readxl)
library(ggplot2)

data <- read_excel("Hemocyanin_measurament.xlsx", sheet = 2) 

##Boxplot
ggplot(data, aes(x = Treatment, y = Hc_average, fill = Treatment)) + 
  geom_boxplot() +
  labs(title = "Boxplot of Hemocyanin by Treatment",
       x = "Treatment",
       y = "Hc (mmol/L)") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        panel.grid.major = element_blank(),  # Remueve las líneas de la cuadrícula mayor
        panel.grid.minor = element_blank(),  # Remueve las líneas de la cuadrícula menor
        panel.background = element_blank(),  # Remueve el fondo
        axis.line = element_line(color = "black")) +  # Asegura que las líneas de los ejes sean visibles
  scale_fill_manual(values = c("C" = "#FE9549", "C-E" = "#9FCAE6", "TL" = "#B57BA2", "TL-E" = "grey"))  # Colores personalizados para cada tratamiento

#Boxplot with individual points overlaid and changed colors
p <- ggplot(data, aes(x = Treatment, y = Hc_average, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.75) +  # Boxplots sin outliers y semi-transparentes
  geom_point(position = position_dodge(width = 0.7), aes(color = Treatment), size = 2) + # Puntos al costado
  scale_fill_manual(values = c("C" = "#FE9549", "C-E" = "#9FCAE6", "TL" = "#B57BA2", "TL-E" = "gray")) +
  scale_color_manual(values = c("C" = "#FE9549", "C-E" = "#9FCAE6", "TL" = "#B57BA2", "TL-E" = "gray")) +
  labs(title = "Boxplot of Hemocyanin by Treatment",
       x = "Treatment",
       y = "Hemocyanin Concentration (mmol/L)") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

print(p)

#Boxplot with individual points overlaid and individuals' labels
p <- ggplot(data, aes(x = Treatment, y = Hc_average, fill = Treatment)) +
  geom_boxplot(outlier.shape = NA) +  # Elimina outliers predeterminados
  geom_point(aes(color = Treatment), position = position_dodge(width = 0.75), alpha = 0.5) +
  geom_text(aes(label = Sample), position = position_dodge(width = 0.75), vjust = -0.5, size = 3, check_overlap = TRUE) +
  scale_fill_manual(values = c("C" = "#FE9549", "C-E" = "#9FCAE6", "TL" = "#B57BA2", "TL-E" = "gray")) +
  scale_color_manual(values = c("C" = "#FE9549", "C-E" = "#9FCAE6", "TL" = "#B57BA2", "TL-E" = "gray")) +
  labs(title = "Boxplot of Hemocyanin by Treatment",
       x = "Treatment",
       y = "Hemocyanin Concentration (mmol/L)") +
  theme_minimal() +
  theme(
    axis.line = element_line(colour = "black"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

print(p)

##############Statistics######################
library(tidyverse)
library(car)
library(stats)
library(dplyr)

####Comparison among 4 treatements with ANOVA
# Normality test, tested with Shapiro-Wilk
data %>%
  group_by(Treatment) %>%
  summarise(shapiro_test = shapiro.test(Hc_average)$p.value)

# Levene test
leveneTest(Hc_average ~ Treatment, data = data)

# ANOVA
anova_result <- aov(Hc_average ~ Treatment, data = data)
summary(anova_result)

####Comparison among C and C-E, TL and TL-E groups with a t.test
t.test(Hc_average ~ Treatment, data = data %>% filter(Treatment %in% c("C", "C-E")),
       paired = FALSE, var.equal = TRUE)


t.test(Hc_average ~ Treatment, data = data %>% filter(Treatment %in% c("TL", "TL-E")),
       paired = FALSE, var.equal = TRUE)

