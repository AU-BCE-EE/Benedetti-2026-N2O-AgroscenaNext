### Plot polished data at chosen day###


plot_polish<-function(Day = as.Date("2025-11-10"),Data_all2 = dt_Aeris_CH4,
                      Data_all3 = dt_Aeris_CO2){
  setDT(Data_all2)
  setDT(Data_all3)
  
  Data_2 <- Data_all2[as.Date(anytime(time)) == as.Date(Day)]
  Data_3 <- Data_all3[as.Date(anytime(time)) == as.Date(Day)]
  
  
  
  plot_N2O<-ggplot(data=Data_3,aes(time,N2O..ppm.,color=ID))+
    geom_point()+theme_bw()+
    xlab(paste0(Day))+ylab(expression("N"[2]* "O, ppm"))

  
  
  plot_CO2<-ggplot(data=Data_3,aes(time,CO2..ppm.,color=ID))+
    geom_point()+theme_bw()+
    xlab(paste0(Day))+ylab(expression("CO"[2]* " , ppm"))
  
  
  if (exists("Data_2")) {
    plot_CH4<-ggplot(data=Data_2,aes(time,CH4..ppm.,color=ID))+
    geom_point()+theme_bw()+
    xlab(paste0(Day))+ylab(expression("CH"[4]* " , ppm"))
}
  
  ggsave(paste0("Static chambers/Pictures/2_", "refinedN2O.png"),
         plot = plot_N2O,width = 8, height = 5)
  ggsave(paste0("Static chambers/Pictures/2_", "refinedCH4.png"),
         plot = plot_CH4,width = 8, height = 5)
  ggsave(paste0("Static chambers/Pictures/2_", "refinedCO2.png"),
         plot = plot_CO2,width = 8, height = 5)
  
  
  return(plot_N2O)
  return(plot_CO2)
  return(plot_CH4)
  
  
}

