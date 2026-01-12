
## Script to calculate daily emissions

# ---- Data for calculations ----

## ---- Chamber volume and area ----

Frame_Vol<-read_excel("Static chambers/Frames Volume.xlsx")
Frame_Vol<-data.table(Frame_Vol)
Frame_Vol$`Volume with chamber (m3)`
Frame_Vol$`Frame number`

dt_Volume<-data.table(Chamber = Frame_Vol$`Frame number`,
                      V = Frame_Vol$`Volume with chamber (m3)`,
                      A = Frame_Vol$`Area chamber (cm2)`/10000)
dt_Volume<-dt_Volume[1:28]



# ---- Merge concentration, volume and area, and calculate concentration in g/m3 ----
dt_CH4<-dt_fluxes(dt_Aeris_CH4,"CH4")
dt_N2O<-dt_fluxes(dt_Aeris_CO2,"N2O")
dt_CO2<-dt_fluxes(dt_Aeris_CO2,"CO2")


# ---- Calculate the emission in g/m2/h ----

# Temporarily change wd 
setwd("/Users/lorenzobenedetti/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Agroscena Next/DFC_SC_Lorenzo_Pablo/Static chambers/Pictures")

#Control plot (with TRUE it creates all PDF plots)
if (as.character(Day_o) == "all"){
  plot <- TRUE
}else{
  plot <- TRUE
}


if (mean(dt_CH4$CH4,na.rm = TRUE) != 0){
CH4_Emis_multiple <- gasfluxes(
  dt_CH4,
  .id    = "Sample",
  .V     = "V",
  .A     = "A",
  .times = "time_hours",
  .C     = "CH4",
  plot = plot)
}
N2O_Emis_multiple <- gasfluxes(
  dt_N2O,
  .id    = "Sample",
  .V     = "V",
  .A     = "A",
  .times = "time_hours",
  .C     = "N2O",
  plot = plot)



CO2_Emis_multiple <- gasfluxes(
  dt_CO2,
  .id    = "Sample",
  .V     = "V",
  .A     = "A",
  .times = "time_hours",
  .C     = "CO2",
  plot = plot)

#Back to original wd
setwd("/Users/lorenzobenedetti/Desktop/Università/DiANA/Dottorato/Danimarca/AU/Agroscena Next/DFC_SC_Lorenzo_Pablo")

# ---- Select best method to solve gasfluxes() ----

#Select proper flux
f.detect_CH4<-Simzeroflux("CH4") #ppm
f.detect_N2O<-Simzeroflux("N2O") #ppm
f.detect_CO2<-Simzeroflux("CO2") #ppm

if (any(as.Date(Data_CH4_merged$time) == Day)) {
CH4_Emis_Best<-selectfluxes(CH4_Emis_multiple, select = "kappa.max", 
                            f.detect = f.detect_CH4/1e6 * unique(dt_CH4$Factor) , 
                            t.meas = max(dt_CH4$time_hours))
}

N2O_Emis_Best<-selectfluxes(N2O_Emis_multiple, select = "kappa.max", 
                            f.detect = f.detect_N2O/1e6 * unique(dt_N2O$Factor) , 
                            t.meas = max(dt_N2O$time_hours))

CO2_Emis_Best<-selectfluxes(CO2_Emis_multiple, select = "kappa.max", 
                            f.detect = f.detect_CO2/1e6 * unique(dt_CO2$Factor) , 
                            t.meas = max(dt_CO2$time_hours))

#Clearly best but not selected
#CH4_Emis_Best[Sample == "F+U_13_2025-08-28_morning"]$method<-"robust linear"
#CH4_Emis_Best[Sample == "F+U_13_2025-08-28_morning"]$flux<-CH4_Emis_Best[Sample == "F+U_13_2025-08-28_morning"]$robust.linear.f0
#CH4_Emis_Best[Sample == "F+U_13_2025-08-28_morning"]$flux.se<-CH4_Emis_Best[Sample == "F+U_13_2025-08-28_morning"]$robust.linear.f0.se
#CH4_Emis_Best[Sample == "F+U_13_2025-08-28_morning"]$flux.p<-CH4_Emis_Best[Sample == "F+U_13_2025-08-28_morning"]$robust.linear.f0.p

# 
# ---- Simple calculation of flux with manual notes ----
Data_w[, CH4_gm3 := (`End CH4 (ppm)`- `Start CH4 (ppm)`)* unique(dt_CH4$Factor) / 1e6]
Data_w[, N2O_gm3 := (`End N2O (ppm)`- `Start N2O (ppm)`)* unique(dt_N2O$Factor) / 1e6]

dt_Simple<-merge(dt_Volume, Data_w, by = "Chamber", all = TRUE)
dt_Simple[, diff_time_h := as.numeric(difftime(End_datetime_CH4, Start_datetime_CH4, units = "hours"))]
dt_Simple[is.na(diff_time_h), diff_time_h := as.numeric(difftime(End_datetime_N2O, Start_datetime_N2O, units = "hours"))]
dt_Simple[, CH4_gm2h := CH4_gm3*(V/A)/diff_time_h]
dt_Simple[, N2O_gm2h := N2O_gm3*(V/A)/diff_time_h]

#dt_Simple$period <- ifelse(format(dt_Simple$Start_datetime_CH4, 
 #                                 "%H:%M:%S") < "13:00:00",
  #                         "morning", "afternoon")
#dt_Simple[is.na(period), period := ifelse(format(Start_datetime_N2O, 
 #                                                "%H:%M:%S") < "13:00:00",
  #                                        "morning", "afternoon")]

dt_Simple[, diff_time_CH4 := as.numeric(difftime(Start_datetime_CH4, min(Start_datetime_CH4,na.rm=TRUE), units = "hours")),
          by = .(Replicate)]
dt_Simple[, diff_time_N2O := as.numeric(difftime(Start_datetime_N2O, min(Start_datetime_N2O,na.rm=TRUE), units = "hours")),
          by = .(Replicate)]

dt_Simple[, cum_CH4 := mintegrate(diff_time_CH4, CH4_gm2h,lwr=0), by = .(Replicate)]
dt_Simple[, cum_N2O := mintegrate(diff_time_N2O, N2O_gm2h,lwr=0), by = .(Replicate)]




# ---- Write txt file for plotting ----

N2O_flux<-dt_fluxFinal(N2O_Emis_Best,"N2O")
CO2_flux<-dt_fluxFinal(CO2_Emis_Best,"CO2")
if (any(as.Date(Data_CH4_merged$time) == Day)) {
CH4_flux<-dt_fluxFinal(CH4_Emis_Best,"CH4")
}

if (any(as.Date(Data_CH4_merged$time) == Day)) {
data<-rbind(CH4_flux,N2O_flux,CO2_flux)
}else{
  data<-rbind(N2O_flux,CO2_flux)
}
data[, diff_time := as.numeric(difftime(Date, min(Date,na.rm=TRUE), units = "hours")),
          by = .(Type,Chamber,Compound)]

data[, cum := mintegrate(diff_time, flux,lwr=0), by = .(Type,Chamber,Compound)]

if (length(unique(data$Date)) != 1){
  write.table(data,"Static chambers/Rfiles/data.txt",sep = "\t")
} else{
  write.table(data,"Static chambers/Rfiles/dataDay.txt",sep = "\t")
    }

write.table(data,"Static chambers/Rfiles/data.txt",sep = "\t")
write.table(dt_Simple,"Static chambers/Rfiles/Simple_data.txt",sep = "\t")


