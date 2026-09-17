rm(list = ls())

setwd("/Users/lorenzobenedetti/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Agroscena Next/DFC_SC_Lorenzo_Pablo/Static chambers")

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(performance)
library(FSA)

# Import
data <- read_excel("Total N2O-N Cattle.xlsx", sheet = 2) %>%
  mutate(TREATMENT = as.factor(TREATMENT))

# Descrittive
desc <- data %>%
  group_by(TREATMENT) %>%
  summarise(
    mean = mean(indirectnn2omg, na.rm = TRUE),
    sd = sd(indirectnn2omg, na.rm = TRUE),
    n = sum(!is.na(indirectnn2omg)),
    se = sd / sqrt(n)
  )

print(desc)

# Modello
model <- lm(indirectnn2omg ~ TREATMENT, data = data)

summary(model)
anova(model)

# Non parametrico
kruskal.test(indirectnn2omg ~ TREATMENT, data = data)
dunnTest(indirectnn2omg ~ TREATMENT, data = data, method = "bonferroni")

# Post-hoc
posthoc <- emmeans(model, pairwise ~ TREATMENT, adjust = "tukey")
summary(posthoc)

# Diagnostica
shapiro.test(residuals(model))
leveneTest(indirectnn2omg ~ TREATMENT, data = data)
check_model(model)

# Plot diagnostici
par(mfrow = c(2,2))
plot(model)

# Boxplot
ggplot(data, aes(x = TREATMENT, y = indirectnn2omg, fill = TREATMENT)) +
  geom_boxplot() +
  theme_bw() +
  labs(
    x = "Treatment",
    y = "indirectnn2omg",
    title = "indirectnn2omg"
  )
