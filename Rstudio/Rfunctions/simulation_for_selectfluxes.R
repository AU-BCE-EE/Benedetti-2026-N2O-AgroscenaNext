### Script to select the best fit in the gasfluxes function according to recomendations by the function developer

Simzeroflux<-function(compound){
  
if (compound == "CH4"){
  C0    <- 2   #ambient concentration, here in [ppm]
  GC.sd <- 2/1000 #uncertainty of Aeris measurement, here in [ppm] (< 2 ppb as reported by the manufacturer)
  dt_fluxes<-dt_CH4
} else if (compound == "N2O"){
  C0    <- 0.35   #ambient concentration, here in [ppm]
  GC.sd <- 500/1e6 #uncertainty of Aeris measurement, here in [ppm] (< 500 ppt as reported by the manufacturer)
  dt_fluxes<-dt_N2O
  
} else if (compound == "CO2"){
  C0    <- 480   #ambient concentration, here in [ppm]
  GC.sd <- 500/1e6 #uncertainty of Aeris measurement, here in [ppm] (< 500 ppt as reported by the manufacturer)
  dt_fluxes<-dt_CO2
  
}


### estimate f.detect by simulation 
#create simulated concentrations corresponding to flux measurements with zero fluxes:
set.seed(42)
sim <- data.frame(t = seq(min(dt_fluxes$time_hours), max(dt_fluxes$time_hours), length.out = 4), 
                  C = rnorm(4e3, mean = C0, sd = GC.sd),
                  id = rep(1:1e3, each = 4), 
                  A = mean(dt_fluxes$A), 
                  V = mean(dt_fluxes$V))   # specify your sampling scheme t (here in [h]) and chamber volume (V) and area (A)
#fit HMR model:                  
simflux <- gasfluxes(sim, .id = "id", .times = "t", methods = c("HMR", "linear"), plot = FALSE, verbose = F) 
simflux[, f0 := HMR.f0]
simflux[is.na(f0), f0 := linear.f0] # use linear estimates where HMR could not be fitted
#dection limit as 97.5 % quantile (95 % confidence):
f.detect <- simflux[, quantile(f0, 0.975)]
f.detect # here in [ppm/h/m^2], use same unit as your flux estimates, 

#e.g., convert to mass flux 
return(f.detect)
}