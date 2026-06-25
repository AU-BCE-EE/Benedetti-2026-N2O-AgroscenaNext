
rm(list = ls())   

setwd("/Users/lorenzobenedetti/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Agroscena Next/DFC_SC_Lorenzo_Pablo/Static chambers")


library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(scales)
library(emmeans)
library(car)


tn_raw    <- read_excel("Total Nitrogen.xlsx") |> clean_names()
flux_raw  <- read_excel("Flux.xlsx")           |> clean_names()
frames    <- read_excel("Frames Volume.xlsx")  |> clean_names()


tn_mean_by_type <- tn_raw |>
  group_by(type) |>
  summarise(tn_mean_gN = mean(tn, na.rm = TRUE), .groups = "drop")


frame_area_col <- grep("frame.*area", names(frames), ignore.case = TRUE, value = TRUE)[1]
frame_area_m2  <- frames |> pull(all_of(frame_area_col))
frame_area_m2  <- frame_area_m2[!is.na(frame_area_m2)][1]

if (is.na(frame_area_m2)) stop("Area del frame non trovata o NA.")


frac_N_in_N2O <- 28 / 44



target_date <- as.Date("2026-01-20")

treatments_to_keep <- c("PSAA", "PSH2SO4", "PSRAW")


n2o_df <- flux_raw |>
  filter(compound == "N2O") |>
  mutate(date = suppressWarnings(as.Date(date))) |>
  select(id, type, chamber, date, cum) |>
  filter(type %in% treatments_to_keep) |>
  #filter(!date %in% dates_to_remove) |>
  filter(date == target_date) |>
  left_join(tn_mean_by_type, by = "type") |>
  mutate(
    n2o_g_per_frame   = cum * frame_area_m2,
    n2oN_g_per_frame  = n2o_g_per_frame * frac_N_in_N2O,
    n2oN_pct_of_tn    = 100 * n2oN_g_per_frame / tn_mean_gN
  ) |>
  filter(!is.na(n2oN_pct_of_tn)) |>
  mutate(
    type = factor(type),
    chamber = factor(chamber)
  )

mod <- lm(n2oN_pct_of_tn ~ type, data = n2o_df)
#mod <- lm(log(n2oN_pct_of_tn + 0.001) ~ type, data = n2o_df)


anova(mod)


summary(mod)


par(mfrow = c(2,2))
plot(mod)
qqnorm(resid(mod)); qqline(resid(mod))
hist(resid(mod), main = "Residui", xlab = "Residui")


emm_type <- emmeans(mod, ~ type)
pairs(emm_type, adjust = "tukey")

emm_df <- as.data.frame(emm_type)

write.csv(
  emm_df,
  "LSM_PS_cumulate_2026-01-20.csv",
  row.names = FALSE
)


p <- ggplot(emm_df, aes(x = type, y = emmean)) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.2
  ) +
  theme_minimal(base_size = 12) +
  labs(
    x = "Treatment",
    y = "% TN",
    #title = "LS-means N2O-N (%TN) – 20/01/2026"
  )

ggsave(
  "LSM_PS_cumulate_2026-01-20.png",
  p,
  width = 7,
  height = 5,
  dpi = 300
)
