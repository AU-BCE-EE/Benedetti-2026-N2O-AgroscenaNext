rm(list = ls())

setwd("/Users/lorenzobenedetti/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Agroscena Next/DFC_SC_Lorenzo_Pablo/Static chambers")

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
data <- read_excel("Total N2O-N Cattle.xlsx") %>%
  filter(!is.na(TREATMENT)) %>%
  mutate(
    TREATMENT = factor(
      TREATMENT,
      levels = c("CSRAW", "CSSA", "CSAA"),
      labels = c("RAW", "H[2]*SO[4]", "CH[3]*COOH")
    )
  )

# -----------------------
# DESCRIPTIVE STATISTICS
# -----------------------
desc <- data %>%
  group_by(TREATMENT) %>%
  summarise(
    mean = mean(netn2on_percTN, na.rm = TRUE),
    sd = sd(netn2on_percTN, na.rm = TRUE),
    n = sum(!is.na(netn2on_percTN)),
    se = sd / sqrt(n),
    .groups = "drop"
  )

print(desc)

# -----------------------
# LINEAR MODEL
# -----------------------
model <- lm(netn2on_percTN ~ TREATMENT, data = data)

summary(model)
anova(model)

# -----------------------
# NON-PARAMETRIC TESTS
# -----------------------
kruskal.test(netn2on_percTN ~ TREATMENT, data = data)
dunnTest(netn2on_percTN ~ TREATMENT, data = data, method = "bonferroni")

# -----------------------
# POST-HOC
# -----------------------
emmeans(model, pairwise ~ TREATMENT, adjust = "tukey")

# -----------------------
# DIAGNOSTICS
# -----------------------
shapiro.test(residuals(model))
leveneTest(netn2on_percTN ~ TREATMENT, data = data)
check_model(model)

par(mfrow = c(2,2))
plot(model)

# -----------------------
# PLOT
# -----------------------
ggplot(desc, aes(x = TREATMENT, y = mean, fill = TREATMENT)) +
  
  geom_col(width = 0.7, color = "black") +
  
  
  
  
  geom_text(
    aes(label = TREATMENT, y = mean / 2),
    parse = TRUE,
    color = "white",
    fontface = "bold",
    size =5.9
  ) +
  
  scale_fill_manual(values = c(
    "RAW" = "#00BFC4",
    "H[2]*SO[4]" = "#7CAE00",
    "CH[3]*COOH" = "#C77CFF"
  )) +
  
  labs(
    x = "Treatment",
    y = expression(N[2]*O)
  ) +
  
  theme_bw(base_size = 22) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_text(face = "bold")
  )