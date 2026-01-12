### Main R file for data treatment on static chambers ###
rm(list = ls())   

# ---- Load packages ----
library(anytime)
library(dplyr)
library(ggplot2)
library(anytime)
library(lubridate)
library(data.table)
library(readxl)
library(changepoint)
library(gasfluxes)
library(myClim)
# ---- Set working directory ----
setwd("/Users/lorenzobenedetti/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Agroscena Next/DFC_SC_Lorenzo_Pablo")

# ---- Days to plot raw data ----
Day<-Day_o<-as.Date("2025-12-15")
#Day<-Day_o<-"all"
#write a date as as.Date("2025-08-28") checking potential errors on specific dates (saves time)
#write "all" for all data 
# ---- Load data ----
Load<-0 #0 do not load #1 load (first time or if changes are made in raw data)
# ---- Select control plot ----
control_plot<-TRUE
#TRUE: Gives a plot of the specified Day to check how polishing worked
ID_Sample<-"CSAA_5" 
#Write name of Sample that you want to do the control plot
#If control_plot == TRUE; gives the plot of the specific sample to plot

# ---- Run main ----
#Load functions
source("Rfunctions/Read_Aeris.R")
source("Rfunctions/plot_raw.R")
source("Rfunctions/Refine_StartEnd_Points.R")
source("Rfunctions/plot_polish.R")
source("Rfunctions/dt_for_gasfluxes.R")
source("Rfunctions/simulation_for_selectfluxes.R")
source("Rfunctions/mintegrate.R")
source("Rfunctions/Selectflux.R") #Compares flux selected with "kappa.max" and AICc (see selectfluxes() function help)

#Load sripts
source("1_Read.R")
source("2_Calculations.R")










