### Function to select flux based on kappa.max and AICc value
## Between the 2, the one with lowest p-value is selected

dt_fluxFinal<-function(data,compound){
  
cols <- grep("\\.AICc", names(data), value = TRUE)
cols_f0 <- grep("\\.f0$", names(data), value = TRUE)

method.AICc<-colnames(data[, ..cols])[apply(data[, ..cols], 1, which.min)] #Get method with minimum AICc
method.AICc<-sub("\\.AICc$", "", method.AICc)
flux.AICc<-flux.p.AICc<-rep(NaN,length(method.AICc))
for (i in 1:length(method.AICc)){
  flux.AICc[i]<-data[[paste0(method.AICc[i],".f0")]][i]
  flux.p.AICc[i]<-data[[paste0(method.AICc[i],".f0.p")]][i]
}

flux<-data[, .(flux, flux.p,flux.se,Sample,method)]
flux<-cbind(flux,method.AICc,flux.AICc,flux.p.AICc)
flux$flux.diff.pct<-(1-(flux$flux/flux$flux.AICc))*100
flux$flux.final <- ifelse(
  flux[["flux.p"]] <= flux[["flux.p.AICc"]],
  flux[["flux"]], flux[["flux.AICc"]]
)
flux$method.final <- ifelse(
  flux[["flux.p"]] <= flux[["flux.p.AICc"]],
  flux[["method"]], flux[["method.AICc"]]
)



flux[, c("Type","Chamber", "Date") := tstrsplit(Sample, "_", fixed = TRUE, keep = c(1,2,3))]
flux$Compound <- compound
return(flux)
}