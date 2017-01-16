############################################################
##          Big Data with Twitter
##  Collecting Twitter Data from Straming API

## Sample intro course material. Demonstrates how to collect
## some basic data from Twitter (though streaming API).
##
## To run this script, you need to generate your own "consumerKey", "consumerSecret", and an oAuth token by ##registering your application with Twitter. It is a simple process, just pick a name for your application. ##Register it here: https://dev.twitter.com/apps 
##
## Documentation of Twitter Streaming API:
##   https://dev.twitter.com/docs/streaming-apis/streams/public
##   https://dev.twitter.com/docs/auth/authorizing-request
##   http://www.foundations-edge.com/blog/oauth_in_R.html
##   https://dev.twitter.com/docs/streaming-apis/processing
##
## Prepared by Christoph Riedl
##      D'Amore-McKim School of Business & 
##          College of Computer and Information Science
##
##      email: c.riedl@neu.edu 
##      web: http://www.christophriedl.net
## Modified by Brinal Pereira
##    College of Computer and Information Science
##    
##    email: pereira.b@husky.neu.edu
############################################################

## Change working directory: create folder and adjust according to taste
setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week8(twitter)")                                                                                 
# Load required libraries                                                                                          



library(RCurl)                                                                                             
library(ROAuth)                                                                                               
library(streamR)                                                                                            
library(twitteR)                                                                                              
download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")                                   
my_oauth$handshake(cainfo="cacert.pem")                                                                         
# Configuration for twitter                                                                                 
outFile         <- "tweets_sample.json"                                                                        
# Twitter configuration                                                                                         
requestURL              <- "https://api.twitter.com/oauth/request_token"                                              
accessURL               <- "https://api.twitter.com/oauth/access_token"                                               
authURL                 <- "https://api.twitter.com/oauth/authorize"                                                  
consumerKey         <- "u7rybd7SW8qXlP0djsddiiRCw"                                                                                   
consumerSecret      <- "1PXJfsHc8TyVEooKMehIcNUPCGVfLD8WbNmC3tj1CixBSKNw2K"                                                                                   
oauth_token             <- "3014891403-dxPSIkXYaJ2Gn7TLFh6FIGchXesmkGa2jOpOXmD"                                                                                      
oauth_token_secret  <- "modwQKUHQcsSL1Ux8bATfmKQOvVjFfco8nATCW4iAiVvL"                                                                                    
my_oauth <- OAuthFactory$new(   consumerKey=consumerKey,
                                consumerSecret=consumerSecret, 
                                requestURL=requestURL,
                                accessURL=accessURL, authURL=authURL)                                           
my_oauth$handshake(cainfo="cacert.pem")   

##Once executing the above code returns true.                                                                        
##You will be given a link to authorize your application to get twitter feeds.                                     
##Copy the link in your browser. Click on Authorize MyApplication. You will receive a pin number.                     
##Copy the pin number and paste it in the console.                                                                  
##Once your application has been authorized you need to register your credentials.                                  


#registerTwitterOAuth(my_oauth)
setup_twitter_oauth(consumerKey, consumerSecret, oauth_token, oauth_token_secret, credentials_file=NULL)

# Press 1 to allow the file to access the credentials 

##Now start reading tweets
sampleStream( file=outFile, oauth=my_oauth )

##Alternative: a little more advanced if you want to filter for things
follow   <- ""    # TwitterIDs (not screennames!) of peple to follow
track    <- "Boston,RedSoxs"  # Comma-separated list of words to filter for
location <- c(23.786699, 60.878590, 37.097000, 77.840813)  # Geolocation of tweets to filter for (see documentation)

filterStream( file.name=outFile, follow=follow, track=track, locations=location, oauth=my_oauth, timeout=10800)
'''

This creates a file on the desktop tweets_sample.json in which the tweet data will be stored.                      

## References :                                                                                                  
* http://tryr.codeschool.com                                                                            
* https://dev.twitter.com/oauth/pin-based                                              

