
#### Plot the results ####
source("Rfunctions/mintegrate.R")
# ---- Load data ----


dt_Simple <- as.data.table(read.table("Static chambers/Rfiles/Simple_data.txt", 
                             header = TRUE,sep = "\t"))

dt_Aeris <- as.data.table(read.table("Static chambers/Rfiles/data.txt", 
                                      header = TRUE,sep = "\t"))

dt_Aeris$ID <- paste0(dt_Aeris$Type,"_",dt_Aeris$Chamber)

dt_cumEmis<-dt_Simple[, .(total_CH4 = mintegrate(diff_time_CH4, CH4_gm2h,lwr=0,
                                                 value="total"),
                          total_N2O = mintegrate(diff_time_N2O, N2O_gm2h,lwr=0,
                                                 value="total")), by = Replicate]

dt_cumEmis_Aeris<-dt_Aeris[, .(total_Emis = mintegrate(diff_time, flux.final,lwr=0,
                                                 value="total")),
                          by = .(Type,Chamber,Compound,ID)]

dt_cumEmis[, c("ID","Chamber") := tstrsplit(Replicate, "_", fixed = TRUE, keep = c(1,2))]

# ---- Plot manual emissions ----


plot_CH4<-ggplot(data = dt_Simple[!is.na(CH4_gm2h)], aes(x = Date, y = CH4_gm2h, 
                                                            color = ID,shape = period,
                                                            group = Replicate)) +
geom_line()+geom_point()+
  ylab(expression("CH"[4]* " emisisons, g m"^-2* "h"^-1*""))+xlab("")+
  theme_bw()
ggsave(plot=plot_CH4,"Figures/Figure1_Emis_CH4_Manual.png",width = 10, height = 4)

plot_N2O<-ggplot(data = dt_Simple[!is.na(N2O_gm2h)], aes(x = Date, y = N2O_gm2h, 
                                                         color = ID,shape = period,
                                                         group = Replicate)) +
  geom_line()+geom_point()+
  ylab(expression("N"[2]* "O emisisons, g m"^-2* "h"^-1*""))+xlab("")+
  theme_bw()
ggsave(plot=plot_N2O,"Figures/Figure1_Emis_N2O_Manual.png",width = 10, height = 4)

plot_CH4_cum<-ggplot(data = dt_Simple[!is.na(CH4_gm2h)], aes(x = Date, y = cum_CH4, 
                                                         color = ID,shape = period,
                                                         group = Replicate)) +
  geom_line()+geom_point()+
  ylab(expression("CH"[4]* " emisisons, g m"^-2* "h"^-1*""))+xlab("")+
  theme_bw()
ggsave(plot=plot_CH4_cum,"Figures/Figure2_cumEmis_CH4_Manual.png",width = 10, height = 4)

plot_N2O_cum<-ggplot(data = dt_Simple[!is.na(N2O_gm2h)], aes(x = Date, y = cum_N2O, 
                                                         color = ID,shape = period,
                                                         group = Replicate)) +
  geom_line()+geom_point()+
  ylab(expression("N"[2]* "O emisisons, g m"^-2* "h"^-1*""))+xlab("")+
  theme_bw()
ggsave(plot=plot_N2O_cum,"Figures/Figure2_cumEmis_N2O_Manual.png",width = 10, height = 4)

bar_tCH4<-ggplot(data = dt_cumEmis, aes(Replicate, total_CH4, fill = ID)) +
  geom_bar(stat = "identity") +
  labs(x = "Replicate", y = expression("Total CH"[4]* ", g m"^-2*"")) +
  theme_bw()
ggsave(plot=bar_tCH4,"Figures/Figure3_Total_CH4_Manual.png",width = 10, height = 4)

bar_tN2O<-ggplot(data = dt_cumEmis, aes(Replicate, total_N2O, fill = ID)) +
  geom_bar(stat = "identity") +
  labs(x = "Replicate", y = expression("Total N"[2]* "O , g m"^-2*"")) +
  theme_bw()
ggsave(plot=bar_tN2O,"Figures/Figure3_Total_N2O_Manual.png",width = 10, height = 4)

# ---- Plot Aeris emissions ----

plot_CH4<-ggplot(data = dt_Aeris[Compound == "CH4"], aes(x = Date, y = flux.final, 
                                                         color = Type,shape = period,
                                                         group = ID)) +
  geom_line()+geom_point()+
  ylab(expression("CH"[4]* " emisisons, g m"^-2* "h"^-1*""))+xlab("")+
  theme_bw()
ggsave(plot=plot_CH4,"Figures/Figure1_Emis_CH4_Aeris.png",width = 10, height = 4)

plot_N2O<-ggplot(data = dt_Aeris[Compound == "N2O"], aes(x = Date, y = flux.final, 
                                                         color = Type,shape = period,
                                                         group = ID)) +
  geom_line()+geom_point()+
  ylab(expression("N"[2]* "O emisisons, g m"^-2* "h"^-1*""))+xlab("")+
  theme_bw()
ggsave(plot=plot_N2O,"Figures/Figure1_Emis_N2O_Aeris.png",width = 10, height = 4)

plot_CO2<-ggplot(data = dt_Aeris[Compound == "CO2"], aes(x = Date, y = flux.final, 
                                                         color = Type,shape = period,
                                                         group = ID)) +
  geom_line()+geom_point()+
  ylab(expression("CO"[2]* " emisisons, g m"^-2* "h"^-1*""))+xlab("")+
  theme_bw()
ggsave(plot=plot_CO2,"Figures/Figure1_Emis_CO2_Aeris.png",width = 10, height = 4)

plot_CH4_cum<-ggplot(data = dt_Aeris[Compound == "CH4"], aes(x = Date, y = cum, 
                                                             color = Type,shape = period,
                                                             group = ID)) +
  geom_line()+geom_point()+
  ylab(expression("CH"[4]* " emisisons, g m"^-2* "h"^-1*""))+xlab("")+
  theme_bw()
ggsave(plot=plot_CH4_cum,"Figures/Figure2_cumEmis_CH4_Aeris.png",width = 10, height = 4)

plot_N2O_cum<-ggplot(data = dt_Aeris[Compound == "N2O"], aes(x = Date, y = cum, 
                                                             color = Type,shape = period,
                                                             group = ID)) +
  geom_line()+geom_point()+
  ylab(expression("N"[2]* "O emisisons, g m"^-2* "h"^-1*""))+xlab("")+
  theme_bw()
ggsave(plot=plot_N2O_cum,"Figures/Figure2_cumEmis_N2O_Aeris.png",width = 10, height = 4)

plot_CO2_cum<-ggplot(data = dt_Aeris[Compound == "CO2"], aes(x = Date, y = cum, 
                                                             color = Type,shape = period,
                                                             group = ID)) +
  geom_line()+geom_point()+
  ylab(expression("CO"[2]* " emisisons, g m"^-2* "h"^-1*""))+xlab("")+
  theme_bw()
ggsave(plot=plot_CO2_cum,"Figures/Figure2_cumEmis_CO2_Aeris.png",width = 10, height = 4)


bar_tCH4<-ggplot(data = dt_cumEmis_Aeris[Compound == "CH4"], aes(ID, total_Emis, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(x = "Replicate", y = expression("Total CH"[4]* ", g m"^-2*"")) +
  theme_bw()
ggsave(plot=bar_tCH4,"Figures/Figure3_Total_CH4_Aeris.png",width = 10, height = 4)

bar_tN2O<-ggplot(data = dt_cumEmis_Aeris[Compound == "N2O"], aes(ID, total_Emis, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(x = "Replicate", y = expression("Total N"[2]* "O , g m"^-2*"")) +
  theme_bw()
ggsave(plot=bar_tN2O,"Figures/Figure3_Total_N2O_Aeris.png",width = 10, height = 4)

bar_tCO2<-ggplot(data = dt_cumEmis_Aeris[Compound == "CO2"], aes(ID, total_Emis, fill = Type)) +
  geom_bar(stat = "identity") +
  labs(x = "Replicate", y = expression("Total CO"[2]* ", g m"^-2*"")) +
  theme_bw()
ggsave(plot=bar_tCO2,"Figures/Figure3_Total_CO2_Aeris.png",width = 10, height = 4)

