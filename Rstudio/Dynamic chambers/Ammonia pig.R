rm(list = ls())   

setwd("/Users/lorenzobenedetti/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Agroscena Next/DFC_SC_Lorenzo_Pablo/DFC NH3")


library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(performance)
library(FSA)

data <- read_excel("2026-03-16-field-pig-integrated-valve-lvl-v323.xlsx")


data_4nov <- data %>%
  filter(as.Date(Date) == as.Date("2025-11-12"))


data_4nov <- data_4nov %>%
  rename(NH3_TN = `N-NH3 perc TN`)


data_4nov$TREATMENT <- as.factor(data_4nov$TREATMENT)


desc <- data_4nov %>%
  group_by(TREATMENT) %>%
  summarise(
    mean = mean(NH3_TN, na.rm = TRUE),
    sd = sd(NH3_TN, na.rm = TRUE),
    se = sd/sqrt(n()),
    n = n()
  )

print(desc)


model <- lm(NH3_TN ~ TREATMENT, data = data_4nov)



summary(model)


anova(model)

kruskal.test(NH3_TN ~ TREATMENT, data = data_4nov)

dunnTest(NH3_TN ~ TREATMENT, data = data_4nov, method="bonferroni")


posthoc <- emmeans(model, pairwise ~ TREATMENT, adjust = "tukey")

posthoc$emmeans
posthoc$contrasts


shapiro.test(residuals(model))


leveneTest(NH3_TN ~ TREATMENT, data = data_4nov)


check_model(model)


par(mfrow = c(2,2))
plot(model)


ggplot(data_4nov, aes(x = TREATMENT, y = NH3_TN, fill = TREATMENT)) +
  geom_boxplot() +
  theme_bw() +
  labs(
    x = "Treatment",
    y = "N-NH3 (% of TN)",
    title = "N-NH3 percentage of total N - 11 November"
  )
