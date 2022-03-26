#copyright - Corey Teply of Aslan Brewing Co. Bellingham, WA - 2022
#R-Shiny app to display all locations our beers are located outside of our three retail locations

require(shiny)
require(shinyWidgets)
require(leaflet)
require(stringr)

beers <- read.csv('Aslan Beer Finder Locations - Sheet1.csv') #csv containing all of the restaurants and grocery stores that Aslan sells beer at
leCols <- colnames(beers) #obtain the column names of the beers DF

earthRad <- 3958.8 #radius of Earth, in miles
distVec <- c(1,5,10,25,50,100,10000000) #in miles (the last values is an arbitrarily large number to denote 'all locations')

#the vector that contains all of the different options for distance. Each option correlates to the distance at the same index in the 'distVec' vector above.
toSelectVec <- c("Three Doors Down (1 mile)","Superfast Jellyfish (5 miles)", "Making My Way Downtown (10 miles)", "He's Going the Distance, He's Going for Speed (25 miles)",
                 "Over the Hills and Far Away (50 miles)", "Starship Trooper (100 miles)","Universally Speaking (all locations)")
venueVec <- c("Both","Grocery Store","Restaurant // Tap Room") #the type of location the user wants to select beer from
zipCodes <- read.csv('betterZips.csv') #all U.S. zip codes that Aslan beer could potentially be sold in (and nearby neighbors that are within 100 miles)
bcZips <- read.csv("refinedBCzips.csv") #all BC zip codes
noBeerText <- "<h5 style='color:#e0de1a'> Sorry, no Aslan beer here mate! Try a new zipcode, venue, or widening the search radius.</h5>" #display if no results are found

# a single icon is declared for leaflet icon styling purposes
awesome <- makeAwesomeIcon(
  icon = "circle",
  iconColor = "#fff",
  markerColor = "cadetblue",
  library = "fa"
)

ui <- tagList(
  #a css section to style the page and make it look nice on mobile
  tags$style("html,body{background-color: black;color:white;font-family:sans-serif}
                #bleepBlorp{
                  font-family: sans-serif;
                  font-style: normal; 
                  font-variant: normal; 
                  font-weight: 700;
                  font-size:14px;
                  background-color: #e0de1a; 
                  color: black; 
                  padding: 12px 28px;
                  border-color:black;
                  border-radius: 16px;
                  transition-duration: 0.4s;
                }
                #bleepBlorp:hover{
                  font-family: sans-serif;
                  font-style: normal; 
                  font-variant: normal; 
                  font-weight: 700;
                  font-size:14px;
                  background-color: #e0de1a; 
                  color: black; 
                  opacity: 0.8;
                  border-color:black;
                  border-radius: 16px;
                }
                .div_image{
                  width: 45px;
                  height: 43px;
                  display: inline-block;
                }
                .topic_name{
                  margin-right: 0px;
                  margin-top: 12px;
                  position: absolute;
                  text-align: right;
                  border: 1px solid red;  
                }
                .image_topic{
                  margin-left: 0px;
                  border: 1px solid red;
                  float: right
                }
                
                .image > img{
                    display: inline-block;
                    max-width: 45px;
                    max-height: 45px;
                    padding: 0px;
                    margin: 0px;
                    width:auto;
                    border-top-left-radius: 10px 5px;
                    border-bottom-right-radius: 10% 5%;
                    border-top-right-radius: 10px;
                }
                .container{
                    width: auto;
                    margin: 0 auto;
                    padding: 0;
                    vertical-align:center;
                }
                .center {
                  display: block;
                  margin-left: auto;
                  margin-right: auto;
                  width: 70%;
                }
                #mainpage{
                    width:100%;
                    background-color: black;
                    color: white;
                    font-family:sans-serif;
                    font-size:13px;
                }
               @media screen and (min-width: 1000px){
                .container{
                    width: 1000px;
                }
               }"
  ),
  tags$div(class="container",
           fluidPage(
             mainPanel(id="mainpage",
                       br(),
          fluidRow(
            column(2,
              tags$style("#zipCode {font-size:12.5px;}"), #for user input for zip code
              textInput('zipCode',"Zip Code:",value = 98225)),
            column(7,
              #user input for distance range they are willing to travel for beer
              pickerInput('distRange', "Within a '________' kind of distance.",choices = toSelectVec,selected = toSelectVec[3],width = "100%")),
            column(3,
              #type of location the user is wanting to buy our beer from 
              pickerInput("placeType","Location Type",choices = venueVec,selected = venueVec[2],width = "100%"))),
          fluidRow(
            column(2,
              #a button to submit the request
              actionButton('findBeers',"FIND MY BEER"))),
        
              br(),
              htmlOutput("noBeerHere"), #a place holder to let the user know that beer may not be available given their desired inputs
              br(),
              leafletOutput("map"), #the container to display the map with all of the locations pinned
              br(),
              #some help text to guide the user and to provide some info on what to expect
              HTML("<h6 style='color:white; text-align:center'><i> * While we update this page frequently, stock and draft lists rotate often. Please contact the store or bar directly for more details on what Aslan beers are currently available. * </i></h6>"),
              HTML("<h6 style='color:white; text-align:center'><i> * This map doesn't contain every place that sells Aslan beer but highlights locations that buy our beer often! If you are a retailer and would like to be added to the map, please email sales@aslanbrewing.com. * </i></h6>")
    )
  
)))


server <- function(input, output, session) {
  
  #######################
  #functions for map queries
  
  #checks to see if a zip code that starts with V is in the bc zip code file, returning T or F
  canadaEh <- function(ziperoo){
    toReturn <- F
    if(str_length(ziperoo)==3){ #all bc zips have a string length of 3
      for (r in 1:nrow(bcZips)) {
        if(grepl(as.character(bcZips[r,1]),ziperoo,ignore.case = T)){ #using a grep function so user input isn't invalid due to the case sensitive 'match()' function
          return(T) #return true if it is a BC zip code
        }
      }
    }
    return(toReturn) #if zip code isn't found, return false
  }
  
  #returns the row where the bc zip code lies
  ohCanada <- function(ziperoo){
    toReturn <- 0
    for(r in 1:nrow(bcZips)) {
      if(grepl(as.character(bcZips[r,1]),ziperoo,ignore.case = T)){ #if user zip code is found, return the row of the BC zip code file that was loaded earlier
        return(r)
      }
    }
    return(toReturn)
  }
  
  #converts angles to radians to be used for R trig functions
  toRad <- function(angle){
    toReturn <- angle*pi/180
    return(toReturn)
  }
  
  #finds arc distance of the GREAT CIRCLE, duh duh duh!! 
  #calculates the distance between 2 points on the globe as the crow flies (disregarding any obstacles or elevation)
  #using law of cosines, it provides a computationally faster alternative to calculating the haversine
  findArcDistance <- function(centralCoords, coords2check){
    deltaSig <- acos(sin(centralCoords[1])*sin(coords2check[1])+cos(centralCoords[1])*cos(coords2check[1])*cos(abs(centralCoords[2]-coords2check[2])))
    haverDist <- earthRad*deltaSig
    return(haverDist)
  }
  
  #finds locations within range of the user's specified parameters
  withinRange <- function(centralCoords,goodDist){
    distIndex <- match(as.character(input$distRange),toSelectVec)
    closeIndex <- c() #vector to store all of the rows that are within range of a specified zip code
    for(r in 1:nrow(beers)) {
      deezCoords <- c(toRad(as.numeric(beers[r,5])),toRad(as.numeric(beers[r,6]))) #create a vector of the latitude and longitude of the coordinates for row r in the beer location csv file
      tempDist <- findArcDistance(centralCoords,deezCoords) #find the distance between the coordinates of the user inputted zip code and the coordinates of the current location being checked in the beer location csv file 
      if(tempDist <= goodDist && (as.character(beers[r,7])==input$placeType || input$placeType == "Both")){ #if it's within distance and is the proper venue for selling beer, add it to the list!
        closeIndex <- append(closeIndex,r)
      }
    }
    return(closeIndex) #return all of the locations that meet the users inputted requests
  }
  
  #formats the phone number to look nice when a user selects a location on the map
  formatPhoneNumber <- function(phone){
    one <- substr(phone,1,1)
    area <- substr(phone,2,4)
    three <- substr(phone,5,7)
    four <- substr(phone,8,11)
    niiice <- paste("(",area,") ",three,"-",four,sep = "")
    return(niiice)
  }
  
  #compiles DF of the desired indeces within range of user specified zip code
  filterGoodZips <- function(goodVec){
    locationVec <- c()
    addressVec <- c()
    phoneVec <- c()
    googleVec <- c()
    latVec <- c()
    longVec <- c()
    restVec <- c()
    for(g in 1:length(goodVec)) {
      r <- goodVec[g] #get the actual row of the location in the beers DF
      if(as.character(beers[r,7]) == input$placeType || input$placeType=="Both"){ #make sure it's the correct venue
        locationVec <- append(locationVec,as.character(beers[r,1]))
        addressVec <- append(addressVec,as.character(beers[r,2]))
        phoneVec <- append(phoneVec,formatPhoneNumber(as.character(beers[r,3])))
        googleVec <- append(googleVec,as.character(beers[r,4]))
        latVec <- append(latVec,beers[r,5])
        longVec <- append(longVec,beers[r,6])
        restVec <- append(restVec,as.character(beers[r,7]))
      }
    }
    toReturn <- data.frame(locationVec,addressVec,phoneVec,googleVec,latVec,longVec,restVec)
    colnames(toReturn)<-c("Location.Name","Address","Phone.Number","Google.Maps.URL", "Lat","Long","BusinessType")
    return(toReturn)
  }
  
  #on startup, display all locations that have Aslan beer
  output$map <- renderLeaflet({
    leaflet(data = beers,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron)%>%
      addAwesomeMarkers(~Long, ~Lat,icon = awesome, popup = ~as.character(paste("<b><a target='_blank' rel='noopener noreferrer' href =",Google.Maps.URL,">", Location.Name,"</a></b>","<br/>",Address,"<br/>",formatPhoneNumber(Phone.Number))), label = ~as.character(Location.Name))
  })
  
  observeEvent(input$findBeers,{ #this code block is for when the user submits a request
    
    disZip <- input$zipCode #get the user inputted zip code
    distIndex <- match(as.character(input$distRange),toSelectVec) #get the index the user selected for their desired distance range
    distVal <- distVec[distIndex] #the actual numeric distance that the user wants to search
    eh <- F #a flag for checking if canadian zip codes need to be checked
    if(substr(disZip,1,1)=="V"){ #if it is a potential BC zip code
      eh <- canadaEh(as.character(disZip)) #check to see if it's in the given BC zip code csv file  
    }
    if(disZip %in% zipCodes[,1] || eh){ #if the zip code is either in the US or BC zip code files
      ogZip <- NULL #make a place holder for the longitude & latitude values to go
      if(substr(disZip,1,1)=="V"){
        zipIndex <- ohCanada(disZip) #get the row where the BC zip code is located
        disLat <- bcZips[zipIndex,4] #get the longitude and latitude values in that row
        disLong <- bcZips[zipIndex,5]
        ogZip <- c(toRad(disLat),toRad(disLong)) #store them in the variable above as a vector
      }else{
        zipIndex <- match(disZip, zipCodes[,1]) #get the row where the US zip code is located
        disLat <- as.numeric(zipCodes[zipIndex,4]) #get the longitude and latitude values in that row
        disLong <- as.numeric(zipCodes[zipIndex,5])
        ogZip <- c(toRad(disLat),toRad(disLong)) #store them in the variable above as a vector
      }
      output$noBeerHere <- renderUI({HTML("")}) #clear the 'noBeerHere' value since we found a valid zip code to compare against 
      goodZipCodes <- withinRange(ogZip,distVal) #get a list of rows where potential locations the have Aslan beer are located
      if(length(goodZipCodes)>0){ #if it finds any locations within the user's inputted specs...
        newdf <- filterGoodZips(goodZipCodes) #make a data.frame of those values to be displayed on a leaflet map
        colnames(newdf)<-leCols 
        
        #render a leaflet map
        output$map <- renderLeaflet({
          leaflet(data = newdf,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron)%>%
            addAwesomeMarkers(~Long, ~Lat,icon = awesome, popup = ~as.character(paste("<b><a target='_blank' rel='noopener noreferrer' href =",Google.Maps.URL,">", Location.Name,"</a></b>","<br/>",Address,"<br/>",Phone.Number)), label = ~as.character(Location.Name))
        })
      }else{
        output$noBeerHere <- renderUI({HTML(noBeerText)})
      }
    }else if(distIndex==7){ #this is a catch in case the user is just looking for where all of our beer is located (meaning that distance from a specific zip code is irrelevant)
      output$noBeerHere <- renderUI({HTML("")}) #clear the 'noBeerHere' value since we will display all locations of a given venue
      newdf <- filterGoodZips(c(1:nrow(beers))) #include all rows of the 'beers' DF, but it will only return locations that are for the specified venue
      output$map <- renderLeaflet({
        leaflet(data = newdf,options = leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron)%>%
          addAwesomeMarkers(~Long, ~Lat,icon = awesome, popup = ~as.character(paste("<b><a target='_blank' rel='noopener noreferrer' href =",Google.Maps.URL,">", Location.Name,"</a></b>","<br/>",Address,"<br/>",formatPhoneNumber(Phone.Number))), label = ~as.character(Location.Name))
      })
    }else{
      output$noBeerHere <- renderUI({HTML(noBeerText)})
    }
  })
  
}

shinyApp(ui=ui, server = server)