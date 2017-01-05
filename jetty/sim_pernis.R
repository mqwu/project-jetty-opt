#############################
# Setting
#############################
library(dplyr)
library(plotly)

#setwd("C:/Apps/projects/JetOpt/code")


#############################
# Add Parcels
#############################
## load parcel default values
parcel.dat <- read.csv("./data/common_data.csv", header=T, as.is=T)
d <- parcel.dat %>% distinct(ProductName)  # unique product name


# add parcel function
AddParcel <- function(parcelName, quant, parcel.dat, parcel){
  # parcelName: product
  # quant: product quantity
  # parcel.dat: default parcel data
  # parcel: parcel data frame
  #
  # return parcel data frame 
  
  new.parcel <- parcel.dat %>% filter(parcelName==ProductName) %>% mutate(Quantity=quant)
  parcel <- rbind(parcel, new.parcel)
  
  return(parcel)
}

# delete parcel

DeleteParcel <- function(parcelName, parcel) {
  subset(parcel, ProductName!=parcelName)
}
  

# add parcel example
# parcel <- NULL
# parcel <- AddParcel("Ethyl Proxitol (EP)", 150, d, parcel)
# parcel <- AddParcel("Isopropyl alcohol (IPA) - USP", 1000, d, parcel)
# parcel <- AddParcel("Methyl Proxitol (MP)", 300, d, parcel)
# parcel <- AddParcel("Caradol SP30-45", 500, d, parcel)
# parcel <- AddParcel("Caradol SP37-25V", 250, d, parcel)
# parcel <- AddParcel("Caradol SP44-10V", 250, d, parcel)
# parcel <- AddParcel("SBP 80/110", 200, d, parcel)



SimulationPernis  <- function(parcel, method="statistical", n.sample=500, j.model = data.frame(jetty=c("J4","J17","J18", "J35", "J35A"), ant_from=rep(0,5), ant_to=rep(0,5))) {
#############################
# Simulation Pernis
#############################

# Order parcel by Jetty number then by cluster
parcel <- parcel %>% arrange(Jetty, Cluster)

# Load historical waiting time data at Pernis
pernis.waiting.time <- read.csv("./data/waiting_time.csv", header=T, as.is=T)

preloading.t.dat <- pernis.waiting.time %>% select(PreloadingT) %>% filter(!is.na(PreloadingT))
postloading.t.dat <- pernis.waiting.time %>% select(PostloadingT) %>% filter(!is.na(PostloadingT))
jetty.t.dat <- pernis.waiting.time %>% select(starts_with("J"))  # jetty waiting time

# Load parameters at Pernis
pernis.par <- read.csv("./data/pernis_par.csv", header=T, as.is=T)
N2AmtPct.purging <- pernis.par[1,2]
N2.rate.purging <- pernis.par[1,3]
N2AmtPct.padding <- pernis.par[2,2]
N2.rate.padding <- pernis.par[2,3]


# estimate total shift time: number of shift = total number of parcel - 1 ??
t.per.shift <- 1
t.tot.shift <- t.per.shift * (nrow(parcel)-1)

  
# T_wating: sample waiting time for each jetty  
if (method == "statistical") {
  t.waiting <- apply(jetty.t.dat, 2, sample, size=n.sample, replace=T)
  } else if (method == "anticipated") {
    n.jetty   <- length(j.model$jetty)
    t.waiting <- matrix(runif(n.jetty*n.sample, 
                              min = j.model$ant_from,
                              max = j.model$ant_to),
                        ncol = n.jetty,
                        byrow = TRUE
                        ) 
    colnames(t.waiting) <- j.model$jetty
}

# T_preprocess: sample preprocessing time for each jetty
t.preload <- sample(preloading.t.dat[,1], n.sample, replace=T)  

# T_postprocess: sample postpocessing time for each jetty
t.postload <- sample(postloading.t.dat[,1], n.sample, replace=T)

# T_purging and T_padding: calculate purging and padding time for each parcel
parcel <- parcel %>% 
  mutate(PurgingT = ifelse(NitrogenPurO2Pct!=0, (1+ N2AmtPct.purging/100)*Quantity/N2.rate.purging * log(21/NitrogenPurO2Pct)/log(2)*1.0, 0)) %>%  # purging time
  mutate(PaddingT1 = ifelse(NitrogenPadO2Pct!=0, (N2AmtPct.padding/100)*Quantity/N2.rate.padding * log(21/NitrogenPadO2Pct)/log(2)*1.0, 0)) %>% # padding time
  mutate(PaddingT = ifelse(PaddingT1!=0, PaddingT1+0.5, PaddingT1)) %>% 
  select(-PaddingT1)


T.terminal <- NULL
for (i in 1:n.sample){
  
  # T_processing: calculate pumping time for each parcel
  parcel.i <- parcel %>% 
    mutate(PumpingT = Quantity/(AvgPumpingRate + rnorm(1)*DeviationPumpingRate))

  # group by jetty then by cluster and calculate Processing time for each cluster
  by_jetty_cls <- parcel.i %>% 
                    group_by(Jetty, Cluster) %>% 
                    summarise(NCls=n(), TotPumpingT=sum(PumpingT), TotPaddingT=sum(PaddingT)) %>% 
                    mutate(SwitchClsT=(NCls-1)*0.5) %>% 
                    mutate(ClsProcessT=TotPumpingT+TotPaddingT+SwitchClsT)
  
  # group by jetty and calculate Jetty time
  by_jetty <- by_jetty_cls %>% 
                  group_by(Jetty) %>% 
                  summarise(JettyProcessT=max(ClsProcessT)) %>% 
                  mutate(JettyPreloadT=t.preload[i], JettyPostloadT=t.postload[i]) %>% 
                  mutate(JettyT=JettyProcessT + JettyPreloadT + JettyPostloadT + t.waiting[i, paste0("J",Jetty)])
  
  # Terminal Time
  t.terminal <- sum(by_jetty$JettyT) + t.tot.shift 
  
  T.terminal <- c(T.terminal, t.terminal)
    
}            

T.terminal

}






  
