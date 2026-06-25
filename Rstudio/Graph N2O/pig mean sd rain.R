rm(list = ls())

setwd("/Users/lorenzobenedetti/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Agroscena Next/DFC_SC_Lorenzo_Pablo/Static chambers")

library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(readxl)
library(ggplot2)
library(dplyr)
library(scales)
library(grid)

# ------------------------
# DATA IMPORT
# ------------------------
df <- read_excel("Flux.xlsx") %>%
  mutate(Date = as.Date(Date))

# ------------------------
# RAIN DATA
# ------------------------
rain_df <- data.frame(
  Date = as.Date(c(
    "2025-11-07","2025-11-08","2025-11-09","2025-11-10","2025-11-11",
    "2025-11-12","2025-11-13","2025-11-14","2025-11-15","2025-11-16",
    "2025-11-17","2025-11-18","2025-11-19","2025-11-20","2025-11-21",
    "2025-11-22","2025-11-23","2025-11-24","2025-11-25","2025-11-26",
    "2025-11-27","2025-11-28","2025-11-29","2025-11-30",
    "2025-12-01","2025-12-02","2025-12-03","2025-12-04","2025-12-05",
    "2025-12-06","2025-12-07","2025-12-08","2025-12-09","2025-12-10",
    "2025-12-11","2025-12-12","2025-12-13","2025-12-14","2025-12-15",
    "2025-12-16","2025-12-17","2025-12-18"
  )),
  Rain = c(
    0,0.1,0.2,0,0,
    2.9,4.8,0,0,0,
    0,0,0,0,0,
    0.1,0,0.8,1,0,
    6.4,1.5,1.4,2.6,
    0.4,0.2,0.4,4.2,1.4,
    2.7,6.5,2,12.3,2.2,
    0.6,0,2.7,0.4,0,
    0.6,2,0.2
  )
)

# ------------------------
# FILTER DATA
# ------------------------
df_filt <- df %>%
  filter(
    Compound == "N2O",
    !Date %in% as.Date(c(
      "2025-10-30",
      "2025-10-31",
      "2025-11-03",
      "2025-11-06",
      "2026-01-19",
      "2026-01-20"
    )),
    Type %in% c("PSSA", "PSRAW", "PSAA", "CTR")
  )

# ------------------------
# CTR CALCULATION
# ------------------------
CTR_by_date <- df_filt %>%
  filter(Type == "CTR") %>%
  group_by(Date) %>%
  summarise(
    CTR_Date = mean(flux.final, na.rm = TRUE),
    .groups = "drop"
  )

conv_factor <- 28 / 44

ctr_mg <- CTR_by_date %>%
  mutate(
    CTR_mg = CTR_Date * 1000 * conv_factor
  )

# ------------------------
# NORMALIZATION
# ------------------------
df_subA <- df_filt %>%
  left_join(CTR_by_date, by = "Date") %>%
  mutate(
    flux_minus_CTR = flux.final - CTR_Date
  )

df_subA_noCTR <- df_subA %>%
  filter(Type != "CTR") %>%
  mutate(
    Type = factor(Type),
    Chamber = factor(Chamber),
    Date = as.Date(Date)
  )

# ------------------------
# SUMMARY
# ------------------------
summary_df <- df_subA_noCTR %>%
  group_by(Date, Type) %>%
  summarise(
    mean = mean(flux_minus_CTR, na.rm = TRUE),
    sd   = sd(flux_minus_CTR, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    sd = ifelse(is.na(sd), 0, sd)
  )

summary_df_mg <- summary_df %>%
  mutate(
    mean  = mean * 1000 * conv_factor,
    sd    = sd * 1000 * conv_factor,
    lower = mean - sd,
    upper = mean + sd
  )

# ------------------------
# CTR FOR PLOT
# ------------------------
ctr_for_plot <- ctr_mg %>%
  transmute(
    Date  = Date,
    mean  = CTR_mg,
    lower = NA,
    upper = NA,
    Type  = "CTR"
  )

# ------------------------
# COMBINE DATA
# ------------------------
plot_data <- bind_rows(
  summary_df_mg,
  ctr_for_plot
)

plot_data$Type <- factor(
  plot_data$Type,
  levels = c("CTR", "PSSA", "PSRAW", "PSAA")
)

# ------------------------
# SCALE FACTOR
# ------------------------
scale_factor <- max(plot_data$mean, na.rm = TRUE) /
  max(rain_df$Rain, na.rm = TRUE)

y_top <- max(plot_data$upper, na.rm = TRUE) * 1.2

# ------------------------
# COLORS
# ------------------------
cols <- c(
  "CTR"   = "black",
  "PSSA"  = "#7CAE00",
  "PSRAW" = "#00BFC4",
  "PSAA"  = "#C77CFF"
)

# ------------------------
# PLOT
# ------------------------
p_sd <- ggplot(
  plot_data,
  aes(
    x = Date,
    y = mean,
    color = Type,
    fill = Type,
    group = Type
  )
) +
  
  geom_rect(
    data = rain_df,
    aes(
      xmin = Date - 0.4,
      xmax = Date + 0.4,
      ymin = y_top - Rain * scale_factor,
      ymax = y_top
    ),
    fill = "#4FA3D9",
    alpha = 0.45,
    inherit.aes = FALSE
  ) +
  
  geom_ribbon(
    data = subset(plot_data, Type != "CTR"),
    aes(
      ymin = lower,
      ymax = upper
    ),
    alpha = 0.2,
    color = NA
  ) +
  
  geom_line(linewidth = 1.2) +
  geom_point(size = 1.5) +
  
  scale_color_manual(
    values = cols,
    name = NULL,
    labels = c(
      "CTR"   = "CTR",
      "PSSA"  = "PS-SA",
      "PSRAW" = "PS-RAW",
      "PSAA"  = "PS-AA"
    )
  ) +
  
  scale_fill_manual(
    values = cols,
    guide = "none"
  ) +
  
  scale_y_continuous(
    name = expression(
      N[2] * O * "-N (mg m"^{-2} ~ h^{-1} * ")"
    ),
    sec.axis = sec_axis(
      ~ (y_top - .) / scale_factor,
      name = "Rain (mm)"
    )
  ) +
  
  labs(
    title = "Pig",
    x = "Days after application"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    
    legend.position = "top",
    legend.direction = "horizontal",
    legend.box = "horizontal",
    
    legend.background = element_rect(
      fill = "white",
      color = "grey80",
      linewidth = 1
    ),
    
    legend.key.size = unit(0.2, "cm"),
    
    legend.text = element_text(
      size = 14
    ),
    axis.title = element_text(size = 14),
    axis.text  = element_text(size = 14),
    
    plot.title = element_text(
      size = 16,
      face = "bold",
      hjust = 0.5
    ),
    
    plot.margin = margin(
      20, 5.5, 5.5, 5.5
    )
  ) +
  
  guides(
    color = guide_legend(
      override.aes = list(
        shape = 16,
        size = 3,
        linewidth = 1
      ),
      nrow = 1
    ),
    fill = "none"
  )

# ------------------------
# SAVE
# ------------------------
#ggsave(
 # "grafico_media_SD_Pig_rain.png",
  #p_sd,
  #width = 14,
  #height = 10,
  #units = "cm",
 # dpi = 300
#)

print(p_sd)