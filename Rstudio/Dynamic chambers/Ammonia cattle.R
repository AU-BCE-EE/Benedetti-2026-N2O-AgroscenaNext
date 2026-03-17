rm(list = ls())   

setwd("/Users/lorenzobenedetti/Documents/GitHub/Benedetti-2026-N2O-AgroscenaNext/Rstudio/Dynamic chambers")

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(performance)
library(FSA)


data <- read_excel("Cattle Final NH3.xlsx")


data <- data %>%
  rename(NH3_TN = `N-NH3 perc TN`)


data$TREATMENT <- as.factor(data$TREATMENT)


desc <- data %>%
  group_by(TREATMENT) %>%
  summarise(
    mean = mean(NH3_TN, na.rm = TRUE),
    sd = sd(NH3_TN, na.rm = TRUE),
    se = sd / sqrt(n()),
    n = n()
  )

print(desc)


model <- lm(NH3_TN ~ TREATMENT, data = data)

summary(model)


anova(model)


kruskal.test(NH3_TN ~ TREATMENT, data = data)


dunnTest(NH3_TN ~ TREATMENT, data = data, method = "bonferroni")


posthoc <- emmeans(model, pairwise ~ TREATMENT, adjust = "tukey")

posthoc$emmeans
posthoc$contrasts


shapiro.test(residuals(model))
leveneTest(NH3_TN ~ TREATMENT, data = data)

check_model(model)


par(mfrow = c(2,2))
plot(model)


ggplot(data, aes(x = TREATMENT, y = NH3_TN, fill = TREATMENT)) +
  geom_boxplot() +
  theme_bw() +
  labs(
    x = "Treatment",
    y = "N-NH3 (% of TN)",
    title = "N-NH3 percentage of total N"
  )
