### Plot raw data at chosen day###


plot_raw<-function(Day = as.Date("2025-11-10"),Data_all1 = Data_N2O_merged,Data_all2 = Data_CH4_merged,Notes_all1 = Data_w){
  setDT(Data_all1)
  setDT(Data_all2)
  setDT(Notes_all1)
  
  Data_1 <- Data_all1[as.Date(anytime(time)) == as.Date(Day)]
  Data_2 <- Data_all2[as.Date(anytime(time)) == as.Date(Day)]
  Notes1 <- Notes_all1[as.Date(anytime(Start_datetime_N2O)) == as.Date(Day)]
  Notes2 <- Notes_all1[as.Date(anytime(Start_datetime_CH4)) == as.Date(Day)]
  

  
  plot_N2O<-ggplot(data=Data_1,aes(time,N2O..ppm.,color=ID))+
    geom_point()+theme_bw()+
    xlab(paste0(Day))+ylab(expression("N"[2]* "O, ppm"))+
    geom_point(data=Notes1,aes(Notes1$Start_datetime_N2O,Notes1$`Start N2O (ppm)`,color="Start"),shape=4,stroke=2)+
    geom_point(data=Notes1,aes(Notes1$End_datetime_N2O,Notes1$`End N2O (ppm)`,color="End"),shape=4,stroke=2)
  


  plot_CO2<-ggplot(data=Data_1,aes(time,CO2..ppm.,color=ID))+
    geom_point()+theme_bw()+
    xlab(paste0(Day))+ylab(expression("CO"[2]* " , ppm"))

    
  plot_CH4<-ggplot(data=Data_2,aes(time,CH4..ppm.,color=ID))+
    geom_point()+theme_bw()+
    xlab(paste0(Day))+ylab(expression("CH"[4]* " , ppm"))+
    geom_point(data=Notes2,aes(Notes2$Start_datetime_CH4,Notes2$`Start CH4 (ppm)`,color="Start"),shape=4,stroke=2)+
    geom_point(data=Notes2,aes(Notes2$End_datetime_CH4,Notes2$`End CH4 (ppm)`,color="End"),shape=4,stroke=2)


  ggsave(paste0("Static chambers/Pictures/1_", "_rawN2O.png"),
         plot = plot_N2O,width = 8, height = 5)
  ggsave(paste0("Static chambers/Pictures/1_", "rawCH4_.png"),
         plot = plot_CH4,width = 8, height = 5)
  ggsave(paste0("Static chambers/Pictures/1_", "_rawCO2_.png"),
         plot = plot_CO2,width = 8, height = 5)

  
  return(plot_N2O)
  return(plot_CO2)
  return(plot_CH4)
  
  
}
  
