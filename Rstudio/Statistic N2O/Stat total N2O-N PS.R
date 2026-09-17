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
data <- read_excel("Total N2O-N Pig.xlsx", sheet = 1) %>%
  mutate(TREATMENT = as.factor(TREATMENT))

# Descrittive
desc <- data %>%
  group_by(TREATMENT) %>%
  summarise(
    mean = mean(netn2on_percTN, na.rm = TRUE),
    sd = sd(netn2on_percTN, na.rm = TRUE),
    n = sum(!is.na(netn2on_percTN)),
    se = sd / sqrt(n)
  )

print(desc)

# Modello
model <- lm(netn2on_percTN ~ TREATMENT, data = data)

summary(model)
anova(model)

# Non parametrico
kruskal.test(netn2on_percTN ~ TREATMENT, data = data)
dunnTest(netn2on_percTN ~ TREATMENT, data = data, method = "bonferroni")

# Post-hoc
posthoc <- emmeans(model, pairwise ~ TREATMENT, adjust = "tukey")

# Diagnostica
shapiro.test(residuals(model))
leveneTest(netn2on_percTN ~ TREATMENT, data = data)
check_model(model)

# Plot diagnostici
par(mfrow = c(2,2))
plot(model)

ggplot(desc, aes(x = TREATMENT, y = mean, fill = TREATMENT)) +
  geom_col(width = 0.7, color = "black") +
  scale_fill_manual(values = c("grey70", "tomato", "goldenrod")) +
  theme_classic() +
  labs(
    x = "Treatment",
    y = expression("Net N"[2]*"O-N (% of total N)")
  ) +
  theme(
    legend.position = "none",
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )