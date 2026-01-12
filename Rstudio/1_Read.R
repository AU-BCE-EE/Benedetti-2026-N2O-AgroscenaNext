
## Script to read all data: Aeris, CRDS, Temperature, and manual notes

# ---- Read data ----
if (Load == 1){
#Path for Aeris Data
path_CH4<-"Static chambers/Data/AerisCH4"
path_N2O<-"Static chambers/Data/AerisN2O"
#Path for Picarro Data
path_NH3<-"DFC NH3/data"



## ---- Aeris data ----

Data_CH4<-data.table(Read_Data(path_CH4))
Data_N2O<-data.table(Read_Data(path_N2O))
# Syncronize with DK time
Data_N2O$time<-anytime(Data_N2O$Time.Stamp)
Data_CH4$time<-anytime(Data_CH4$Time.Stamp)+hours(9)

## ---- CRDS data ----

Data_NH3<-data.table(Read_Data(path_NH3))



save(Data_NH3,Data_CH4,Data_N2O,
     file = paste0("Rfiles/DataFilter.RData"))}else{
       load(paste0("Rfiles/DataFilter.RData"))
       
     }



## ---- Written data ----
Data_w<-data.table(read_excel("Static chambers/Data/chamber_data.xlsx"))
#Data_w[, ID := gsub("^F$", "Feces", ID)]
#Data_w[, ID := gsub("^U$", "Urine", ID)]



Data_w[, `:=`(
  Start_time = format(as.POSIXct(Start_time), "%H:%M:%S"),
  End_time   = format(as.POSIXct(End_time),   "%H:%M:%S")
)]
Data_w$Date<-as.character(Data_w$Date)
#Data_w<-Data_w[Date == Day]

Data_w[!is.na(`Start CH4 (ppm)`) & !is.na(`End CH4 (ppm)`),
       `:=`(
         Start_datetime_CH4 = as.POSIXct(paste(Date, Start_time),
                                         format = "%Y-%m-%d %H:%M:%S") + hours(9),
         End_datetime_CH4   = as.POSIXct(paste(Date, End_time),
                                         format = "%Y-%m-%d %H:%M:%S") + hours(9)
       )]
Data_w[!is.na(`Start N2O (ppm)`) & !is.na(`End N2O (ppm)`),
       `:=`(
         Start_datetime_N2O = as.POSIXct(paste(Date, Start_time),
                                         format = "%Y-%m-%d %H:%M:%S"),
         End_datetime_N2O   = as.POSIXct(paste(Date, End_time),
                                         format = "%Y-%m-%d %H:%M:%S") 
       )]

# Data_w[, Start_datetime := as.POSIXct(paste(Date, Start_time))+hours(9)]
# Data_w[, End_datetime := as.POSIXct(paste(Date, End_time))+hours(9)]
 Data_w[,Replicate :=paste0(ID,"_",Chamber)]
# Data_w$Start_limit<-Data_w$Start_datetime-seconds(10)
# Data_w$End_limit<-Data_w$End_datetime+seconds(10)

# ---- Merge written data with Aeris data ----
 Data_CH4_merged <- Data_w[Data_CH4, 
                             on = .(Start_datetime_CH4 <= time, End_datetime_CH4 >= time), 
                             nomatch = NA,
                             .(time = i.time, ID,Replicate,Chamber,CH4..ppm.)]
 # plot_CH4 <- Data_CH4[, .(time,CH4..ppm.)]
 # plot_CH4$ID<-Data_CH4_merged$ID
 # plot_CH4$Replicate<-Data_CH4_merged$Replicate
 # plot_CH4$Chamber<-Data_CH4_merged$Chamber 
 
 Data_N2O_merged <- Data_w[Data_N2O, 
                           on = .(Start_datetime_N2O <= time, End_datetime_N2O >= time), 
                           nomatch = NA,
                           .(time = i.time,N2O..ppm.,CO2..ppm.,ID,Replicate,Chamber)]
 # plot_N2O <- Data_N2O[, .(time,N2O..ppm.)]
 # plot_N2O$ID<-Data_N2O_merged$ID
 # plot_N2O$Replicate<-Data_N2O_merged$Replicate
 # plot_N2O$Chamber<-Data_N2O_merged$Chamber 
 
 # ---- Refine starting point and ending point ----
 if (is.Date(Day)) {
   Data_w<-Data_w[Date == Day]
   plot_raw(Day)
   
 } else {
     Day <- as.Date("2025-11-10")
     Data_w<-Data_w
     plot_raw()
      }  
 
 dt_Aeris_N2O<-changepoints(Data_w[Date == Day],Data_N2O_merged,"N2O..ppm.") #Not necessary as points are the same as CO2
 if (any(as.Date(Data_CH4_merged$time) == Day)) {
 dt_Aeris_CH4<-changepoints(Data_w,Data_CH4_merged,"CH4..ppm.",plot = control_plot,ID_Sample,Day = Day)
 }
 if (any(as.Date(Data_N2O_merged$time) == Day)) {
 dt_Aeris_CO2<-changepoints(Data_w,Data_N2O_merged,"CO2..ppm.",plot = control_plot,ID_Sample,Day = Day)
}
 ## ---- Control plot to check that changepoints function works ----
 if (!any(as.Date(Data_CH4_merged$time) == Day)) {
   dt_Aeris_CH4<-dt_Aeris_CO2
   dt_Aeris_CH4$CH4..ppm.<-0
 }
 
 if (!any(as.Date(Data_N2O_merged$time) == Day)) {
   dt_Aeris_CO2<-dt_Aeris_CH4
   dt_Aeris_CO2$N2O..ppm.<-0
   dt_Aeris_CO2$CO2..ppm.<-0
   
 }

  plot_polish(Day)
 

