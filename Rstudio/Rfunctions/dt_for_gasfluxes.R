### Function that provides the data table for the gasfluxes function

dt_fluxes<-function(data,compound){
  
  dt_Volume<-dt_Volume
  #Temp_Data<-Temp_Data
  data$Chamber<-as.numeric(data$Chamber)

  dt_merged1<-merge(dt_Volume, data, by = "Chamber", all = TRUE)
  dt_merged2 <- dt_merged1[!is.na(Chamber), ]
  dt_merged2$Date<-as.Date(dt_merged2$time)
  #dt_merged2$period <- ifelse(format(dt_merged2$time, "%H:%M:%S") < "13:00:00", "morning", "afternoon")
  

#Temperature for each sensor per date and period(removed)
dt_times <- dt_merged2[,.(min_time = min(time),max_time = max(time)), 
                       by = .( Date )]
#Temp_Data$Date <- as.Date(NA)
#Temp_Data$period <- "NaN"
list_pos <- list()
#for (i in 1:nrow(dt_times)){
 # Pos <- which(Temp_Data$time >= dt_times$min_time[i] & Temp_Data$time <= dt_times$max_time[i]) 
  #list_pos[[i]]<-Pos
  #Temp_Data$Date[list_pos[[i]]]<-dt_times$Date
  #Temp_Data$period[list_pos[[i]]]<-dt_times$period
#}


P<-1 #atm air pressure inside chamber
R<-0.082/1000 #(atm m3)/(K mol) ideal gas constant

#Temp <- Temp_Data[,.(mean_value = mean(value),sd_value = sd(value),
 #                    N_value = length(value)), 
  #by = .(Date, period, sensor_name,height,Position,Date,period)]
mean_SC <- 8

#Temp<- na.omit(Temp)
#mean_SC<-mean(Temp[(sensor_name == "TMS_T3" | sensor_name == "TMS_T2") &
                    # Position != "Dynamic chamber"]$mean_value)
if (compound == "N2O"){
  M<-44.013
} else if (compound == "CH4"){
  M<-16.04
} else if (compound == "CO2"){
    M<-44
  }

Factor<-(P/(R*(mean_SC+273)))*M

dt_merged2$Factor<-Factor
dt_merged2[, paste0(compound, "..gm3.") := Factor/1e6 * get(paste0(compound, "..ppm."))]

varname<-as.name(paste0(compound))
dt_fluxes <- dt_merged2[, .(V, A,Factor, 
                            time_hours = as.numeric(difftime(time, min(time), units = "hours")),
                            varname = get(paste0(compound, "..gm3."))),
                        by = .( Replicate,Date)]
setnames(dt_fluxes, "varname", paste0(compound))
dt_fluxes[, Sample := paste0(Replicate, "_", Date)]
dt_fluxes<-dt_fluxes[complete.cases(dt_fluxes)]
return(dt_fluxes)
}

