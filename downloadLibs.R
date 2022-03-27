mapPackages <- c("shiny","shinyWidgets","leaflet","stringr") #packages that my program depends on
machinePackages <- installed.packages() #all packages downloaded on your local machine

#check to see if the packages needed for my app are already on your machine, and if not, download them!
for(p in mapPackages){
  if(!(p %in% rownames(machinePackages))){
    print(paste("downloading:", p))
    #UNCOMMENT THE INSTALL.PACKAGES(P) LINE TO ACUTALLY DOWNLOAD THEM!
    #install.packages(p) 
  }
}
