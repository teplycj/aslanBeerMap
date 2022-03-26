mapPackages <- c("shiny","shinyWidgets","leaflet","stringr") #packages that my program depends on
machinePackages <- installed.packages() #all packages downloaded on your local machine

for(p in mapPackages){
  if(!(p %in% rownames(machinePackages))){
    print(paste("downloading:", p))
    install.packages(p)
  }
}
