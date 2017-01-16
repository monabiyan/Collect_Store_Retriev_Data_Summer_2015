##Google API and use of static map
## Author - Martin Schedlbauer and Yatish Jain
## Version 1.1

library(RCurl)
library(RJSONIO)
library(plyr)

#getLocation function returns the latitude and longitude of any address via google maps api.

#fetchUrl function gives the url to fetch json string.
fetchUrl <- function(address) {
  root <- "http://maps.google.com/maps/api/geocode/"   #root url 
  u <- paste(root, "json", "?address=", address, "&sensor=false", sep = "") 
  return(URLencode(u)) #encoding the url
}

getLocation <- function(address) {
  url <- fetchUrl(address) #getting the url for the address, I suggest to take a look at this URL before further exploring the code.
  json <- getURL(url)
  x <- fromJSON(json,simplify = FALSE) #getting the json
  if(x$status=="OK") { #checking if the Address url used produces the correct json string 
    lat <- x$results[[1]]$geometry$location$lat
    lng <- x$results[[1]]$geometry$location$lng
    location_type <- x$results[[1]]$geometry$location_type
    formatted_address <- x$results[[1]]$formatted_address
    return(c(lat, lng, location_type, formatted_address))
    Sys.sleep(0.5)
  } else {
    return(c(NA,NA,NA,NA))
  }
}

address <- c("Northeastern Universty, Boston, MA", "Harvard University, Boston, MA")
locations <- ldply(address, function(x) getLocation(x))
names(locations) <- c("lat", "lon", "location_type", "formatted")
locations



#Another function to access static maps

staticURL<-function(latitude,longitude,zoom,maptype){
base="http://maps.googleapis.com/maps/api/staticmap?center="
suffix ="&size=800x800&sensor=false&format=png"

target <- paste0(base,latitude,",",longitude,"&zoom=",zoom,"&maptype=",maptype,suffix)
return(target)
}

test<-staticURL(locations$lat[1],locations$lon[1],"13","hybrid") #enter latitude and longitude, zoom values range from 1 - 18, maptypes can be- hybrid,satellite,terrain,roadmap
browseURL(test)
download.file(test,"test.png", mode = "wb")
