rm(list = ls())

setwd(
  "/Users/lorenzobenedetti/Documents/GitHub/Benedetti-2026-N2O-AgroscenaNext/Rstudio/Statistic N2O"
)

library(readxl)
library(dplyr)
library(ggplot2)
library(car)
library(emmeans)
library(performance)


# -----------------------
# IMPORT DATA
# -----------------------

data <- read_excel("Total N2O-N Cattle.xlsx")

required_columns <- c(
  "TREATMENT",
  "BLOCK",
  "netn2on_percTN"
)

missing_columns <- setdiff(
  required_columns,
  names(data)
)

if (length(missing_columns) > 0) {
  stop(
    "The following columns are missing: ",
    paste(missing_columns, collapse = ", ")
  )
}

data <- data %>%
  filter(
    !is.na(TREATMENT),
    !is.na(BLOCK),
    !is.na(netn2on_percTN)
  ) %>%
  mutate(
    TREATMENT = factor(
      TREATMENT,
      levels = c(
        "CSRAW",
        "CSSA",
        "CSAA"
      ),
      labels = c(
        "RAW",
        "H[2]*SO[4]",
        "CH[3]*COOH"
      )
    ),
    BLOCK = factor(BLOCK)
  )


# -----------------------
# CHECK EXPERIMENTAL DESIGN
# -----------------------

design_table <- with(
  data,
  table(BLOCK, TREATMENT)
)

cat("\nOBSERVATIONS PER BLOCK AND TREATMENT\n")
print(design_table)

if (any(design_table != 1)) {
  warning(
    paste(
      "The dataset does not contain exactly one observation",
      "for each block-by-treatment combination."
    )
  )
}


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

cat("\nDESCRIPTIVE STATISTICS\n")
print(desc)


# -----------------------
# RANDOMIZED COMPLETE BLOCK MODEL
#
# BLOCK is entered before TREATMENT.
# Therefore, the sequential Type I test for treatment
# accounts for the block effect.
# -----------------------

model <- lm(
  netn2on_percTN ~ BLOCK + TREATMENT,
  data = data
)


# -----------------------
# COMPLETE MODEL INFORMATION
# -----------------------

cat("\nMODEL FORMULA\n")
print(formula(model))

cat("\nNUMBER OF OBSERVATIONS\n")
print(nobs(model))

cat("\nRESIDUAL DEGREES OF FREEDOM\n")
print(df.residual(model))


# -----------------------
# SEQUENTIAL TYPE I ANOVA
# -----------------------

anova_results <- anova(model)

cat("\nSEQUENTIAL TYPE I ANOVA\n")
print(anova_results)


# -----------------------
# MODEL SUMMARY
# -----------------------

cat("\nMODEL SUMMARY\n")
print(summary(model))


# -----------------------
# DIAGNOSTICS
# -----------------------

shapiro_results <- shapiro.test(
  residuals(model)
)

cat("\nSHAPIRO-WILK TEST ON MODEL RESIDUALS\n")
print(shapiro_results)


diagnostic_data <- data.frame(
  residual = residuals(model),
  treatment = model.frame(model)$TREATMENT
)

levene_results <- leveneTest(
  residual ~ treatment,
  data = diagnostic_data,
  center = median
)

cat("\nLEVENE TEST ON MODEL RESIDUALS\n")
print(levene_results)


cat("\nMODEL DIAGNOSTIC PLOTS\n")

par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

check_model(model)


# -----------------------
# BLOCK-ADJUSTED ESTIMATED MARGINAL MEANS
# -----------------------

emm_treatment <- emmeans(
  model,
  ~ TREATMENT
)

emm_results <- summary(
  emm_treatment,
  infer = c(TRUE, FALSE),
  level = 0.95
)

cat("\nBLOCK-ADJUSTED ESTIMATED MARGINAL MEANS\n")
print(emm_results)


# -----------------------
# TUKEY-ADJUSTED PAIRWISE COMPARISONS
#
# The output includes:
# - estimated differences;
# - standard errors;
# - degrees of freedom;
# - simultaneous 95% confidence intervals;
# - Tukey-adjusted P values.
# -----------------------

tukey_contrasts <- pairs(
  emm_treatment,
  adjust = "tukey"
)

contrast_results <- summary(
  tukey_contrasts,
  infer = c(TRUE, TRUE),
  level = 0.95,
  adjust = "tukey"
)

cat("\nTUKEY-ADJUSTED TREATMENT CONTRASTS\n")
print(contrast_results)


# -----------------------
# PLOT OF BLOCK-ADJUSTED MEANS
# -----------------------

emm_df <- as.data.frame(emm_results)

ggplot(
  emm_df,
  aes(
    x = TREATMENT,
    y = emmean,
    fill = TREATMENT
  )
) +
  geom_col(
    width = 0.7,
    color = "black"
  ) +
  geom_errorbar(
    aes(
      ymin = lower.CL,
      ymax = upper.CL
    ),
    width = 0.15,
    linewidth = 0.8
  ) +
  geom_text(
    aes(
      label = TREATMENT,
      y = emmean / 2
    ),
    parse = TRUE,
    color = "white",
    fontface = "bold",
    size = 5.9
  ) +
  scale_fill_manual(
    values = c(
      "RAW" = "#00BFC4",
      "H[2]*SO[4]" = "#7CAE00",
      "CH[3]*COOH" = "#C77CFF"
    )
  ) +
  labs(
    x = "Treatment",
    y = expression(N[2]*O-N~"(% of TN)")
  ) +
  theme_bw(base_size = 22) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title = element_text(face = "bold")
  )


# -----------------------
# OBSERVED VALUES BY BLOCK
# -----------------------

ggplot(
  data,
  aes(
    x = TREATMENT,
    y = netn2on_percTN,
    group = BLOCK,
    colour = BLOCK
  )
) +
  geom_line(
    linewidth = 0.7,
    alpha = 0.7
  ) +
  geom_point(size = 3) +
  theme_bw(base_size = 18) +
  labs(
    x = "Treatment",
    y = expression(N[2]*O-N~"(% of TN)"),
    colour = "Block"
  ) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    )
  )