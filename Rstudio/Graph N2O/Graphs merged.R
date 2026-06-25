rm(list = ls())   

setwd("/Users/lorenzobenedetti/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Agroscena Next/DFC_SC_Lorenzo_Pablo/Static chambers")

library(png)
library(grid)
library(ggplot2)
library(patchwork)

# Carica le immagini dai file
img1 <- png::readPNG("grafico_media_SD_Cattle_rain.png")
img2 <- png::readPNG("grafico_media_SD_Pig_rain.png")

# Converti in grob
g1 <- grid::rasterGrob(img1, interpolate = TRUE)
g2 <- grid::rasterGrob(img2, interpolate = TRUE)

# Crea due "finti" plot che contengono le immagini
p1 <- ggplot() + 
  annotation_custom(g1, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void()

p2 <- ggplot() +
  annotation_custom(g2, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
  theme_void()

# Unione orizzontale
combined <- p1 | p2

# Mostra il risultato
combined

# Salva
ggsave("immagini_unite_orizzontale.png",
       combined, width = 12, height = 6, dpi = 300)
