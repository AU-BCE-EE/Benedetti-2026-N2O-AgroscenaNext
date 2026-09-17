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

data <- read_excel("Total N2O-N Pig.xlsx", sheet = 2)

required_columns <- c(
  "TREATMENT",
  "BLOCK",
  "indirectnn2omg"
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
    !is.na(indirectnn2omg)
  ) %>%
  mutate(
    TREATMENT = factor(TREATMENT),
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
    mean = mean(indirectnn2omg),
    sd = sd(indirectnn2omg),
    n = n(),
    se = sd / sqrt(n),
    .groups = "drop"
  )

cat("\nDESCRIPTIVE STATISTICS\n")
print(desc)

# -----------------------
# RANDOMIZED COMPLETE BLOCK MODEL
#
# BLOCK is treated as a fixed effect and entered before
# TREATMENT. Therefore, anova(model) provides a sequential
# Type I ANOVA in which treatment is tested after accounting
# for block.
#
# The BLOCK × TREATMENT interaction cannot be estimated
# because there is only one observation for each combination.
# -----------------------

model <- lm(
  indirectnn2omg ~ BLOCK + TREATMENT,
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

cat("\nRESIDUAL STANDARD DEVIATION\n")
print(sigma(model))

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
# MODEL DIAGNOSTICS
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
# These are Tukey-adjusted pairwise comparisons, not an
# LSD test. Estimates are treatment differences expressed
# in the original response units.
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
# PLOT OF BLOCK-ADJUSTED MEANS AND 95% CI
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
    linewidth = 0.7
  ) +
  scale_fill_manual(
    values = c("grey70", "tomato", "goldenrod")
  ) +
  theme_classic(base_size = 14) +
  labs(
    x = "Treatment",
    y = "Indirect N2O-N emission",
    title = "Block-adjusted treatment means with 95% confidence intervals"
  ) +
  theme(
    legend.position = "none"
  )

# -----------------------
# OBSERVED VALUES BY BLOCK
# -----------------------

ggplot(
  data,
  aes(
    x = TREATMENT,
    y = indirectnn2omg,
    group = BLOCK,
    color = BLOCK
  )
) +
  geom_line(
    linewidth = 0.6,
    alpha = 0.7
  ) +
  geom_point(
    size = 3
  ) +
  theme_classic(base_size = 14) +
  labs(
    x = "Treatment",
    y = "Indirect N2O-N emission",
    color = "Block",
    title = "Observed values within each experimental block"
  )