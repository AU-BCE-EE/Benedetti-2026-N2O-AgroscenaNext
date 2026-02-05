rm(list = ls())   

setwd("/Users/lorenzobenedetti/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Agroscena Next/DFC_SC_Lorenzo_Pablo/Static chambers")


library(readxl)
library(dplyr)
library(janitor)
library(ggplot2)
library(scales)
library(lme4)
library(lmerTest)
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


frac_N_in_N2O <- 28/44


#dates_to_remove <- as.Date(c("2025-11-07", "2026-01-20"))
dates_to_remove <- as.Date(c("2025-11-07", "2026-01-19", "2026-01-20"))
treatments_to_keep <- c("CSAA", "CSH2SO4", "CSRAW", "CTR")

n2o_df <- flux_raw |>
  filter(compound == "N2O") |>
  mutate(date = suppressWarnings(as.Date(date))) |>
  select(id, type, chamber, date, cum) |>
  filter(type %in% treatments_to_keep) |>
  filter(!date %in% dates_to_remove) |>
  left_join(tn_mean_by_type, by = "type") |>
  mutate(
    frame_area_m2     = frame_area_m2,
    n2o_g_per_frame   = cum * frame_area_m2,
    n2oN_g_per_frame  = n2o_g_per_frame * frac_N_in_N2O,
    n2oN_pct_of_tn    = 100 * n2oN_g_per_frame / tn_mean_gN
  ) |>
  filter(!is.na(date), !is.na(n2oN_pct_of_tn))


agg_df <- n2o_df |>
  group_by(type, date) |>
  summarise(
    n = n(),
    mean_pct = mean(n2oN_pct_of_tn, na.rm = TRUE),
    sd_pct   = sd(n2oN_pct_of_tn, na.rm = TRUE),
    se_pct   = sd_pct / sqrt(n),
    .groups  = "drop"
  )


p <- ggplot(agg_df, aes(x = date, y = mean_pct, color = type)) +
  geom_line(linewidth = 0.9, alpha = 0.95) +
  geom_point(size = 1.8, alpha = 0.95) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d-%b") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(x = "Data",
       y = "% TN",
       color = "Type") +
  theme_minimal(base_size = 11) +
  theme(panel.grid.minor = element_blank(), legend.position = "bottom")

ggsave("grafico_medie_per_type.png", p, width = 9, height = 5, dpi = 300)
#write.csv(agg_df, "medie_per_type.csv", row.names = FALSE)


n2o_df <- n2o_df |>
  mutate(
    type = factor(type),
    chamber = factor(chamber),
    date = factor(date)   
  )


#mod <- lmer(n2oN_pct_of_tn ~ type * date + (1 | chamber), data = n2o_df)

mod <- lmer(log(n2oN_pct_of_tn + 0.001) ~ type * date + (1 | chamber), data = n2o_df)

anova(mod)
#anova(mod, ddf = "Kenward-Roger")


summary(mod)


par(mfrow = c(2,2))
plot(mod)                  
qqnorm(resid(mod)); qqline(resid(mod))  
hist(resid(mod), main="Residues Distribution", xlab="Residues")  
influenceIndexPlot(mod, vars=c("Cook"))  

emm_type <- emmeans(mod, ~ type)
pairs(emm_type, adjust="tukey")


emm_td <- emmeans(mod, ~ type | date)
pairs(emm_td, adjust="tukey")


emm_dt <- emmeans(mod, ~ date | type)
pairs(emm_dt, adjust="tukey")


emm_df <- as.data.frame(emm_td)

ggplot(emm_df, aes(x = date, y = emmean, color = type, group = type)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.2) +
  theme_minimal() +
  labs(
    y = "% TN (N2O-N)",
    x = "Date",
    color = "Treatment"
  )

ggsave("LMM_emission_plot.png", width = 9, height = 5, dpi = 300)

write.csv(emm_df, "LSM_CS_cumulate.csv", row.names = FALSE)
