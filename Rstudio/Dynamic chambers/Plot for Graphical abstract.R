rm(list = ls())

setwd("/Users/lorenzobenedetti/Documents/GitHub/Benedetti-2026-N2O-AgroscenaNext/Rstudio/Dynamic chambers")

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(performance)
library(FSA)

# -----------------------
# IMPORT DATA
# -----------------------
data <- read_excel("Cattle Final NH3.xlsx")

data <- data %>%
  rename(NH3_TN = `N-NH3 perc TAN`)

# -----------------------
# FIX CRITICO: pulizia TREATMENT (evita NA)
# -----------------------
data$TREATMENT <- data$TREATMENT %>%
  trimws()

# controllo rapido (opzionale ma utile)
unique(data$TREATMENT)

# conversione factor corretta
data$TREATMENT <- factor(
  data$TREATMENT,
  levels = c("CSRAW", "CSH2SO4", "CSAA"),
  labels = c("RAW", "H2SO4", "CH3COOH")
)

# -----------------------
# DESCRIPTIVE STATISTICS
# -----------------------
desc <- data %>%
  group_by(TREATMENT) %>%
  summarise(
    mean = mean(NH3_TN, na.rm = TRUE),
    sd = sd(NH3_TN, na.rm = TRUE),
    n = sum(!is.na(NH3_TN)),
    se = sd / sqrt(n),
    .groups = "drop"
  )

print(desc)

# -----------------------
# MODEL
# -----------------------
model <- lm(NH3_TN ~ TREATMENT, data = data)

summary(model)
anova(model)

# -----------------------
# NON-PARAMETRIC TESTS
# -----------------------
kruskal.test(NH3_TN ~ TREATMENT, data = data)
dunnTest(NH3_TN ~ TREATMENT, data = data, method = "bonferroni")

# -----------------------
# POST-HOC
# -----------------------
posthoc <- emmeans(model, pairwise ~ TREATMENT, adjust = "tukey")

posthoc$emmeans
posthoc$contrasts

# -----------------------
# DIAGNOSTICS
# -----------------------
shapiro.test(residuals(model))
leveneTest(NH3_TN ~ TREATMENT, data = data)

check_model(model)

par(mfrow = c(2,2))
plot(model)

# -----------------------
# BAR PLOT (media ± SE)
# -----------------------
ggplot(desc, aes(x = TREATMENT, y = mean, fill = TREATMENT)) +
  
  geom_col(width = 0.7, color = "black") +
  

  
  geom_text(
    aes(label = TREATMENT, y = mean / 2),
    color = "white",
    fontface = "bold",
    size = 7.8
  ) +
  
  scale_fill_manual(values = c(
    "RAW" = "#00BFC4",
    "H2SO4" = "#7CAE00",
    "CH3COOH" = "#C77CFF"
  )) +
  
  labs(
    x = "Treatment",
    y = expression(NH[3])
    
  ) +
  
  theme_bw(base_size = 20) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_blank(),
    axis.text = element_text(color = "black")
  )