library(scam)


changepoints <- function(dt,dt_Aeris,compound,plot = FALSE,repl = ID_Sample,Day = "2025-11-10"){
                                      
  # Ensure data.table format
  dt <- as.data.table(dt)
  dt_Aeris <- as.data.table(dt_Aeris)
  
  # Prepare results container
  list_subdat <- list()
  
  #Print progress bar
  pb <- txtProgressBar(min = 0, max = nrow(dt)/2, style = 3)
  
  # Make sure not NA
  if (compound == "CH4..ppm.") {
    dt <- dt[!is.na(Start_datetime_CH4)]
  } else {
    dt <- dt[!is.na(Start_datetime_N2O)]
  }
  
  
  
  # Loop over each interval
  for (i in seq_len(nrow(dt))) {
    #Generate each start and end time
    if (compound == "CH4..ppm."){
      factor<-(dt$End_datetime_CH4[i]-dt$Start_datetime_CH4[i])/6.5
      start_time <- dt$Start_datetime_CH4[i]-factor
      end_time   <- dt$End_datetime_CH4[i]+factor
    }else{
      factor<-(dt$End_datetime_N2O[i]-dt$Start_datetime_N2O[i])/6.5
      start_time <- dt$Start_datetime_N2O[i]-factor
      end_time   <- dt$End_datetime_N2O[i]+factor
    }
    
#subdata inside time interval
    sub_dt <- dt_Aeris[time >= start_time & time <= end_time]

    while (length(unique(sub_dt$Replicate)) < 3){
      sub_dt <- dt_Aeris[time >= start_time-factor & time <= end_time+factor]
      factor<-factor+as.duration(seconds(10))
      print(unique(sub_dt$Replicate))
    }
    
    #Extra refinements to improve spline fit
    if (!is.na(unique(sub_dt$Replicate)[1]) & 
        isTRUE(names(which.max(table(sub_dt$Replicate))) != unique(sub_dt$Replicate)[1])){
    last <- tail(sub_dt[sub_dt$Replicate == unique(sub_dt$Replicate)[1], ], 1)
    sub_dt <- sub_dt[-(1:(as.numeric(table(sub_dt$Replicate)[unique(sub_dt$Replicate)[1]])-1)),] 
    }
    
    print(paste0(start_time," ",end_time," ",names(which.max(table(sub_dt$Replicate)))))
    
    #Fit model and calculate slopes and intercepts of segments
    y<-sub_dt[time<=end_time-factor][[compound]]
    x<-as.numeric(sub_dt[time<=end_time-factor][["time"]])
    data<-data.frame(x=x)
    
    model<-scam( y~ s(x, k=6, bs="ps", m=0)) #knots = 5 to have 4 segments

    predict<-predict(model,newdata = data)
    
    Fitted<-fitted(model)
    Residuals<-residuals(model)
    
    internal_knots <- model$smooth[[1]]$knots[2:(length(model$smooth[[1]]$knots) - 1)] #Exclude external knots
    predicted_values <- predict(model, newdata = data.frame(x = internal_knots))
    
    slopes <- diff(predicted_values) / diff(internal_knots)
    intercepts <- predicted_values[-length(predicted_values)] - slopes * internal_knots[-length(internal_knots)]
   
    #Take second and last segments (might need to be refined Indeed!! it needed to be refined)
    idx <- which(slopes[2:length(slopes)] >= 0)[1] + 1  # +1 because we started at 2 Make sure takes first positive slope
    if (is.na(idx) | idx == length(slopes)){
      mean_a<-(slopes[c(2,length(slopes))]) 
      mean_b<-(intercepts[c(2,length(slopes))])
    }else{
      mean_a<-(slopes[c(idx,length(slopes))]) 
      mean_b<-(intercepts[c(idx,length(slopes))])
    }


    #Calculate point where both segments cut
    x_int <- (mean_b[2] - mean_b[1]) / (mean_a[1] - mean_a[2]) 
    timeA<-sub_dt$time[as.numeric(sub_dt$time)<=x_int]
    timeB<-sub_dt$time[as.numeric(sub_dt$time)>=x_int]
    
    #Function to extrapolate two middle segments (they cover correct data)
    EqA<-mean_a[1]*as.numeric(timeA)+mean_b[1]
    EqB<-mean_a[2]*as.numeric(timeB)+mean_b[2]
    Eq<-c(EqA,EqB)
    
    if (length(EqB) == 0 ){
      Eq<-mean_a[2]*as.numeric(timeA)+mean_b[2]
    }
    
    detrend_out<-sub_dt[[compound]]-Eq
    
    q <- 0.1*(max(detrend_out))
    outliers <- which( abs(detrend_out) > q)
    detrend<-detrend_out[-outliers]
  
    
    n <- round(0.005*length(sub_dt[[compound]]))
    
    time_polish <- c(sub_dt[["time"]][-outliers][(n)],sub_dt[["time"]][-outliers][(length(sub_dt[["time"]][-outliers])-n)])
    
    
    
    sub_dt$Replicate <- ifelse(
      sub_dt[["time"]] > time_polish[1] & sub_dt[["time"]] < time_polish[2],
      names(which.max(table(sub_dt$Replicate))), NA
    )
    sub_dt$Chamber <- ifelse(
      sub_dt[["time"]] > time_polish[1] & sub_dt[["time"]] < time_polish[2],
      names(which.max(table(sub_dt$Chamber))), NA
    )
    sub_dt$ID <- ifelse(
      sub_dt[["time"]] > time_polish[1] & sub_dt[["time"]] < time_polish[2],
      names(which.max(table(sub_dt$ID))), NA
    )

    #Further refinement of the initial part
    result <- cpt.meanvar(sub_dt[[compound]],method = "PELT",
                          penalty = "Manual",
                          pen.value = length(sub_dt[[compound]])) 
    Pos_change<-cpts(result)
    
    if (length(Pos_change) > 0){
      
    
    if (Pos_change[1] <= 0.35*length(sub_dt[[compound]])){
      
      sub_dt$Replicate[Pos_change[1]:round(0.35*length(sub_dt[[compound]]))]<-unique(sub_dt$Replicate)[2]
      sub_dt$Chamber[Pos_change[1]:round(0.35*length(sub_dt[[compound]]))]<-unique(sub_dt$Chamber)[2]
      sub_dt$ID[Pos_change[1]:round(0.35*length(sub_dt[[compound]]))]<-unique(sub_dt$ID)[2]
      
      
      while (length(Pos_change[1]) == 1  && sub_dt[[compound]][Pos_change[1] + 1] <= sub_dt[[compound]][Pos_change[1]]) {
        Pos_change[1] <- Pos_change[1] + 1
      }
      while (length(Pos_change[1]) == 1 && mean(mean_a) >= 1e-3 && round(sub_dt[[compound]][Pos_change[1]],1) >= round(sub_dt[[compound]][Pos_change[1]-5],1)) {
        Pos_change[1] <- Pos_change[1] - 5
      }
      
    sub_dt$Replicate[1:Pos_change[1]]<-NA
    sub_dt$Chamber[1:Pos_change[1]]<-NA
    sub_dt$ID[1:Pos_change[1]]<-NA
    
    
    
    
    } else if(Pos_change[1] > 0.5*length(sub_dt[[compound]])){
      while (length(Pos_change[1]) == 1 && Pos_change[1] + 5 <= nrow(sub_dt) && 
             mean(sub_dt[[compound]][Pos_change[1]:(Pos_change[1]+5)]) >= round(sub_dt[[compound]][Pos_change[1]],2)
              ) {
        Pos_change[1] <- Pos_change[1] + 5
      }
      sub_dt$Replicate[Pos_change[1]:nrow(sub_dt)]<-NA
      sub_dt$Chamber[Pos_change[1]:nrow(sub_dt)]<-NA
      sub_dt$ID[Pos_change[1]:nrow(sub_dt)]<-NA
    }
    }
     if (length(Pos_change) >= 2){
       Check2<-mean(sub_dt[[compound]][Pos_change[1]:Pos_change[2]])-sub_dt[[compound]][Pos_change[1]:Pos_change[2]]
       if (all(round(Check2,1) == 0) & Pos_change[2] > 0.75*length(sub_dt[[compound]])){
      sub_dt$Replicate[Pos_change[2]:nrow(sub_dt)]<-NA
      sub_dt$Chamber[Pos_change[2]:nrow(sub_dt)]<-NA
      sub_dt$ID[Pos_change[2]:nrow(sub_dt)]<-NA
       }else if(Pos_change[2] < 0.5*length(sub_dt[[compound]])){
         while (length(Pos_change[2]) == 1 && sub_dt[[compound]][Pos_change[2] + 1] <= sub_dt[[compound]][Pos_change[2]]) {
           Pos_change[2] <- Pos_change[2] + 1
         }
         while (length(Pos_change[2]) == 1 && sub_dt[[compound]][Pos_change[2]] > sub_dt[[compound]][Pos_change[2]-5] && mean_a[2] > 1e-5) {
           Pos_change[2] <- Pos_change[2] - 5
         }
         
         sub_dt$Replicate[1:Pos_change[2]]<-NA
         sub_dt$Chamber[1:Pos_change[2]]<-NA
         sub_dt$ID[1:Pos_change[2]]<-NA
       }
     }
  ## Final refinement

   Pos<-which(sub_dt$Replicate == unique(sub_dt$Replicate)[2])
   Pos<-Pos[1]
   Pos_o <- Pos  
   
   Pos2<-which(sub_dt$Replicate == unique(sub_dt$Replicate)[2])
   Pos2<-Pos2[length(Pos2)]
   Pos_o2 <- Pos2  

         #Keep previously deleted positions with increasing points
   if ((Pos-5) > 1){
   while (sub_dt[[compound]][Pos] >= round(sub_dt[[compound]][Pos-5],1) & mean_a[2] > 1e-5) {
     Pos <- Pos - 5
     if ((Pos-5) <= (1)) {
       break
     }       
   }
}
   sub_dt$Replicate[Pos:Pos_o]<-unique(sub_dt$Replicate)[2]
   sub_dt$Chamber[Pos:Pos_o]<-unique(sub_dt$Chamber)[2]
   sub_dt$ID[Pos:Pos_o]<-unique(sub_dt$ID)[2] 

   
   #Remove positions in the first end with decreasing points 
   while (sub_dt[[compound]][Pos] >= sub_dt[[compound]][Pos+5]) { 
     Pos <- Pos + 5
   }
   
   while (sub_dt[[compound]][Pos] <= sub_dt[[compound]][Pos+5] & mean_a[2] < 1e-4) { 
     Pos <- Pos + 5
   }
   sub_dt$Replicate[1:Pos]<-NA
   sub_dt$Chamber[1:Pos]<-NA
   sub_dt$ID[1:Pos]<-NA 
   
   #Keep previously deleted positions with increasing points
   if ((Pos2) <= (nrow(sub_dt)-6) & mean_a[2] > 9e-5) {
     while (round(sub_dt[[compound]][Pos2],2) <= round(sub_dt[[compound]][Pos2+5],2)){
       Pos2 <- Pos2 + 5
       if ((Pos2+5) >= (nrow(sub_dt))) {
         break
       }       
     }
   }
   
   sub_dt$Replicate[Pos_o2:Pos2]<-unique(sub_dt$Replicate)[2]
   sub_dt$Chamber[Pos_o2:Pos2]<-unique(sub_dt$Chamber)[2]
   sub_dt$ID[Pos_o2:Pos2]<-unique(sub_dt$ID)[2]
   
   #Remove positions in the last end with decreasing points 
   while (sub_dt[[compound]][Pos2] <= sub_dt[[compound]][Pos2-5]) {
     Pos2 <- Pos2 - 5
   }
   sub_dt$Replicate[Pos2:nrow(sub_dt)]<-NA
   sub_dt$Chamber[Pos2:nrow(sub_dt)]<-NA
   sub_dt$ID[Pos2:nrow(sub_dt)]<-NA 
   
   sub_dt$Replicate[Pos:Pos2]<-unique(sub_dt$Replicate)[2]
   sub_dt$Chamber[Pos:Pos2]<-unique(sub_dt$Chamber)[2]
   sub_dt$ID[Pos:Pos2]<-unique(sub_dt$ID)[2]
   
   
     
    if (plot == TRUE & names(which.max(table(sub_dt$Replicate))) == repl & 
        unique(as.Date(sub_dt$time)) == as.Date(Day)){
      #If some point wrong check why

      plot(as.numeric(sub_dt[["time"]]),sub_dt[[compound]])
      lines(x,predict,col="red")
      lines(as.numeric(sub_dt[["time"]]),Eq,col="blue",type = "l",lty = 2)
      
      plot(as.numeric(sub_dt[["time"]]),(detrend_out))
      points(as.numeric(sub_dt[["time"]])[-outliers],detrend,col="red")
      
      plot(result)
      
      plot((sub_dt[["time"]]),sub_dt[[compound]])
      points(sub_dt[["time"]][sub_dt[["Replicate"]] == repl],
             sub_dt[[compound]][sub_dt[["Replicate"]] == repl],col="red")
    }
    list_subdat[[i]] <- sub_dt
print(paste0("finish row",dt$Row_N[i]," ",names(which.max(table(sub_dt$Replicate))),"_",compound))
setTxtProgressBar(pb, i) 
 }
  Data_duplicated<-data.table(bind_rows(list_subdat, .id = "column_label")) ## Binds lists into a single dataframe but have duplicated times
  Data <- Data_duplicated[order(time), 
                           .SD[if (any(!is.na(Replicate))) which(!is.na(Replicate))[1] else 1], 
                           by = time]
  close(pb)
  cat("Done!\n")
  return(Data)
}
























