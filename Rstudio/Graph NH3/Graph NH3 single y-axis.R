rm(list = ls())

library(readxl)
library(dplyr)
library(ggplot2)
library(patchwork)

setwd("/Users/lorenzobenedetti/Documents/GitHub/Benedetti-2026-N2O-AgroscenaNext/Rstudio/Graph NH3")

load_flux_data <- function(file_xlsx, levels_treat){
  
  raw <- read_excel(file_xlsx, sheet = "Foglio1")
  
  dat_flux <- raw %>%
    select(
      TREATMENT = `TREATMENT`,
      time_h = `time_since_slurry_aplication[h]`,
      flux = `flux [mg/m2 h]`
    ) %>%
    filter(!is.na(flux))
  
  dat_flux$TREATMENT <- toupper(trimws(dat_flux$TREATMENT))
  
  dat_flux$TREATMENT <- factor(
    dat_flux$TREATMENT,
    levels = levels_treat
  )
  
  dat_flux %>%
    group_by(TREATMENT,time_h) %>%
    summarise(
      n = n(),
      mean_flux = mean(flux, na.rm = TRUE),
      sd_flux = ifelse(n > 1, sd(flux, na.rm = TRUE), 0),
      .groups = "drop"
    )
}

cattle_summary <- load_flux_data(
  "2026-03-16-field-cattle-integrated-valve-lvl-v323.xlsx",
  c("CSSA","CSRAW","CSAA")
)

pig_summary <- load_flux_data(
  "2026-03-16-field-pig-integrated-valve-lvl-v323.xlsx",
  c("PSSA","PSRAW","PSAA")
)


temp_cattle <- data.frame(
  time_h = 1:172,
  Temp = c(
    6.5,6.3,6.5,6.7,6.7,6.7,6.8,7.0,6.6,6.5,6.4,5.6,6.2,7.3,
    7.8,8.1,8.5,9.2,10.2,9.8,9.4,9.4,9.5,9.4,
    9.6,9.3,9.3,9.2,9.2,9.1,9.0,8.9,8.8,8.8,
    8.9,8.9,8.7,8.7,8.2,8.3,8.0,7.3,8.0,9.4,
    9.9,10.0,8.9,7.6,6.7,5.9,5.9,5.4,5.3,5.1,
    4.6,4.2,4.7,5.2,4.9,4.7,4.1,4.3,4.3,5.0,
    6.1,7.3,9.0,10.1,10.3,10.3,10.1,9.3,8.5,8.1,
    8.2,8.3,8.4,8.3,8.3,8.4,8.5,8.5,8.4,8.3,
    8.3,8.0,7.9,7.9,8.4,9.6,10.4,10.9,11.0,11.2,
    11.1,10.8,10.8,10.8,10.9,10.9,11.1,10.8,10.2,9.9,
    9.6,9.2,9.0,9.1,8.8,8.8,9.1,9.2,9.1,10.0,
    10.7,11.5,11.5,11.4,11.0,10.3,8.9,7.9,7.4,7.4,
    7.6,7.4,7.2,7.1,7.3,7.3,7.3,6.9,7.0,7.2,
    6.5,6.5,7.5,8.7,9.8,10.2,10.6,10.4,9.9,9.5,
    9.5,10.0,10.5,11.0,11.5,12.2,12.5,12.7,12.8,13.0,
    13.1,13.0,12.9,12.9,12.8,12.9,12.9,13.0,13.0, 13, 13.2, 13.2,
    13.0,13.0,12.9,12.5,12.4,12.2
  )
)

temp_pig <- data.frame(
  time_h = 1:163,
  Temp = c(
    
    12.9,13.1,13.0,12.9,12.5,11.8,11.2,11.1,11.0,11.0,
    11.0,11.0,10.9,10.8,10.8,11.0,11.0,10.8,10.6,10.3,
    10.3,11.1,12.7,13.1,13.7,15.0,15.3,14.0,12.6,11.4,
    10.0,8.9,8.3,8.3,8.3,8.1,7.8,7.4,7.0,6.5,
    5.8,5.7,6.1,6.3,7.2,7.5,7.8,8.6,8.9,8.9,
    8.3,7.9,7.9,7.9,7.9,7.9,7.8,7.8,7.8,7.9,
    8.1,8.1,8.1,8.2,8.1,8.0,8.0,7.9,7.9,8.0,
    8.1,8.2,8.3,8.3,8.3,8.2,8.0,8.0,7.9,7.8,
    7.7,7.8,7.8,7.7,7.5,7.4,7.2,7.0,6.9,6.8,
    6.8,6.8,6.8,6.8,6.8,7.0,7.2,7.1,6.9,6.9,
    6.8,6.6,6.2,6.0,6.1,6.2,6.2,5.7,5.8,6.1,
    6.3,6.2,5.8,5.5,5.6,6.0,5.7,5.8,6.6,7.5,
    8.7,8.9,8.8,8.7,8.3,8.3,8.1,8.1,7.9,8.0,
    8.2,7.8,7.3,6.9,6.7,6.4,6.2,6.6,6.9,7.2,
    7.4,7.6,7.7,7.9,8.3,8.9,9.1,9.3,9.3,8.7,
    8.8,8.1,7.7,7.4,7.8,8.7,9.5,9.8,9.7,9.6,
    9.5,9.4,9.2
    
  )
)

temp_cattle <- temp_cattle %>%
  filter(time_h <= max(cattle_summary$time_h))

temp_pig <- temp_pig %>%
  filter(time_h <= max(pig_summary$time_h))

global_flux_max <- max(
  cattle_summary$mean_flux + cattle_summary$sd_flux,
  pig_summary$mean_flux + pig_summary$sd_flux,
  na.rm = TRUE
)

library(dplyr)
library(ggplot2)

###########################################################
## Prepare data
###########################################################

cattle_summary <- cattle_summary %>%
  mutate(
    Animal = "Cattle",
    Treatment = recode(as.character(TREATMENT),
                       CSSA = "SA",
                       CSRAW = "RAW",
                       CSAA = "AA")
  )

pig_summary <- pig_summary %>%
  mutate(
    Animal = "Pig",
    Treatment = recode(as.character(TREATMENT),
                       PSSA = "SA",
                       PSRAW = "RAW",
                       PSAA = "AA")
  )

flux_summary <- bind_rows(cattle_summary, pig_summary)

###########################################################
## Temperature
###########################################################

temp_cattle <- temp_cattle %>%
  mutate(Animal = "Cattle")

temp_pig <- temp_pig %>%
  mutate(Animal = "Pig")

temp_all <- bind_rows(temp_cattle, temp_pig)

###########################################################
## Common scale
###########################################################

global_flux_max <- max(
  flux_summary$mean_flux + flux_summary$sd_flux,
  na.rm = TRUE
)

temp_max <- 20

scale_factor <- global_flux_max / temp_max

###########################################################
## Plot
###########################################################

palette_auto <- c(
  SA = "#7CAE00",
  RAW = "#00BFC4",
  AA = "#C77CFF"
)

final_plot <- ggplot(
  flux_summary,
  aes(time_h,
      mean_flux,
      colour = Treatment,
      fill = Treatment)
) +
  
  geom_ribbon(
    aes(
      ymin = mean_flux - sd_flux,
      ymax = mean_flux + sd_flux
    ),
    alpha = 0.20,
    colour = NA
  ) +
  
  geom_line(linewidth = 1.2) +
  
  geom_point(size = 1.5) +
  
  geom_line(
    data = temp_all,
    aes(
      x = time_h,
      y = Temp * scale_factor
    ),
    inherit.aes = FALSE,
    colour = "black",
    linetype = "dashed",
    linewidth = 1.1
  ) +
  
  facet_wrap(~Animal, nrow = 1) +
  
  scale_colour_manual(
    values = palette_auto,
    name = NULL
  ) +
  
  scale_fill_manual(
    values = palette_auto,
    guide = "none"
  ) +
  
  scale_y_continuous(
    limits = c(0, global_flux_max),
    
    name = expression(
      paste("NH"[3], " (mg ", m^{-2}, " h"^{-1}, ")")
    ),
    
    sec.axis = sec_axis(
      ~./scale_factor,
      name = expression(Temperature~(degree*C)),
      breaks = seq(0,20,5)
    )
  ) +
  
  labs(
    x = "Hours after application"
  ) +
  
  theme_minimal(base_size = 15) +
  
  theme(
    
    strip.text = element_text(
      size = 20,
      face = "bold"
    ),
    
    axis.title = element_text(size = 20),
    
    axis.text = element_text(size = 18),
    
    legend.position = "top",
    
    legend.text = element_text(size = 18),
    
    panel.grid.minor = element_blank()
    
  )

final_plot

ggsave(
  "NH3_Cattle_Pig.png",
  final_plot,
  width = 12,
  height = 6,
  dpi = 600
)
