############################################################
##          Big Data with YouTube
##  Collecting YouTube Data from Straming API
## 
## 
##
## To run this script, you need to generate your own "browserKey" by registering your application with Google. 
##It is a simple process, just pick a name for your project. ##Register it here: https://console.developers.google.com/project 
##
##           Collecting Data via WebAPIs in R
##           Martin Schedlbauer, Ph.D., Yatish Jain
##
## Prepared by  Yatish Jain
##     email: jain.ya@husky.neu.edu
##            y.jain@neu.edu
##
##      
##
############################################################

## change the working directory to desktop to keep track of output file.
setwd("/Users/Yatish/Desktop")
getwd()

# Load required libraries
library(rjson)
library(RCurl)

#Generate your own key as per the instructions in document and paste it here to configure the API
key<- "XXXX"


#Funtion to check connection. This getStats function will fetch the statistics of any video given the video ID and key. 
#Read instructions on how to get the video ID under the environment setup section

getStats <- function(id,key){
  url=paste("https://www.googleapis.com/youtube/v3/videos?id=",id,"&key=",key,"&part=statistics,snippet",sep="")
  raw.data <- getURL(url) 
  rd  <- fromJSON(raw.data)
  title<- rd$items[[1]]$snippet$title
  channelTitle<- rd$items[[1]]$snippet$channelTitle
  views<- rd$items[[1]]$statistics$viewCount
  likes<- rd$items[[1]]$statistics$likeCount
  dislikes<- rd$items[[1]]$statistics$dislikeCount
  fav<- rd$items[[1]]$statistics$favoriteCount
  comments<- rd$items[[1]]$statistics$commentCount
  return(data.frame(title,channelTitle,views,likes,dislikes,fav,comments))
}

id<- ("4OIDdeGI7f8") #Change this video ID with some other
stats<-getStats(id,key)
stats

id<- ("QcIy9NiNbmo")
stats<-getStats(id,key)
stats




#getVideos function return the list of videos along with their statistics given the channelID and key.
# Read the instructions on how to get channelID under the environment setup section

getVideos<- function(channelID,key){
  url=paste("https://www.googleapis.com/youtube/v3/search?key=",key,"&channelId=",channelID,"&part=snippet,id&order=date&maxResults=10",sep="")
  raw.data <- getURL(url) 
  rd  <- fromJSON(raw.data)
  perPage<- rd$pageInfo$resultsPerPage
  totalResults<-rd$pageInfo$totalResults
  totalVideos<-min(perPage,totalResults)
stats<-c(NA,NA,NA,NA,NA,NA,NA)
for (i in 1:totalVideos){
  kind<- rd$items[[i]]$id$kind 
  if(kind == "youtube#video"){
  videoID<- rd$items[[i]]$id$videoId 
  
    stats<-rbind(stats,getStats(videoID,key))

}
else if(kind == "youtube#playlist"){
  playlistID<- rd$items[[i]]$id$playlistId
  url=paste("https://www.googleapis.com/youtube/v3/playlistItems?part=snippet%2CcontentDetails&maxResults=10&playlistId=", playlistID,"&key=",key,sep="")
  raw.data <- getURL(url) 
  rd1  <- fromJSON(raw.data)
  perPage<- rd1$pageInfo$resultsPerPage
  totalResults<-rd1$pageInfo$totalResults
  totalVideos<-min(perPage,totalResults)
  for(i in 1:totalVideos){
    videoID<-rd1$items[[i]]$contentDetails$videoId
    stats<-rbind(stats,getStats(videoID,key))
  }
}  
}

return(stats)
}


channelID<-"UCANLZYMidaCbLQFWXBC95Jg" #change this channelID and run the function again
getVideos(channelID,key)





#getChannelsOrPlaylists function return the list of videos and their statistics associated with a keyword search on YouTube.
# When you search a keyword on youtube sometimes playlists also end up in search and we fetch the data from those playlists as well which might not be directly related to our search keyword, but it will fetch the data of similar searches.

getChannelsOrPlaylists<- function(search, key){
  search<-URLencode(search)
  url<-paste("https://www.googleapis.com/youtube/v3/search?q=",search,"&key=",key,"&type=channel&part=snippet&maxResults=50",sep="")
  raw.data <- getURL(url) 
  rd  <- fromJSON(raw.data)
  perPage<- rd$pageInfo$resultsPerPage
  totalResults<-rd$pageInfo$totalResults
  totalChannels<- min(perPage,totalResults)
  data<-NA
  for(i in 1:totalChannels){
    channelID<- rd$items[[i]]$id$channelId
    print(channelID)
    if(!is.null(channelID)){
      data<-rbind(data,getVideos(channelID,key))
    }
  }
  data<- data[complete.cases(data),]
  if(exists("data.csv"))
  {
    file.remove("data.csv")
  }
  write.csv(data,"data.csv")
 
}

search<-"taylor swift"
getChannelsOrPlaylists(search,key)

