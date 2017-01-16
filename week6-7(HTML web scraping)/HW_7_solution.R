#Mohsen Nabian, HW7 Solution

#problem: I have <U+0092> for "'" in some rest. names. But it doesn't go away with gsub. 




#QUESTION: 
#The objective of this assignment is to learn how to extract data from web pages through R
#programming.
#??? Select a website from which to scrape the data
#??? Declare (in your R code as comments) exactly what data the code scrapes and from
#where
#??? Make the scraping parameterized, i.e., allow a data scientist to select search parameters choose
#a website that uses GET requests
#??? Write a function to scrape the data and return the data as a data frame
#??? Perform some simple statistical analysis or searching to ensure that the data was
#retrieved properly, i.e., provide scaffolding to test your code

#################################################################################################



cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.
require(RCurl)    #Downloading Package
require(XML)
library(RCurl)     #Using package in this program
library(XML)

#Set the working directory to your workspace
setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week6-7")

#YelpParse Function parses data in the YELP search pages. It provides Name,Type,Price, the number of reviews and the Tell number for each element.
#The Elements could be restaurant or bars or coffee-shops or so many other places.
#It is requiered to find the link in YELP website and put it in the YelpParse function here.
#User is able to choose which information he/she wants as output by putting TRUE or FALSE in the correponding places for input.
#TYPE,PRICE,REVIEW,TELL should be substituted by TRUE or FALSE based on User's need.
YelpParse<-function(link,TYPE,PRICE,REVIEW,TELL)   
{
          
          ########## To understand which information the user wants######
          choice=0;
          choice[1]=1
          choice[2]=as.numeric(TYPE)*2
          choice[3]=as.numeric(PRICE)*3
          choice[4]=as.numeric(REVIEW)*4
          choice[5]=as.numeric(TELL)*5
          ################################################################
          # This is the URL of the website we need scrape to get information on the 
          
          theurl <- link
          theurl<-gsub(" ","",theurl);   #No extra spaces
          webpage <- getURL(theurl)
          # convert the page into a line-by-line format rather than a single string
          tc <- textConnection(webpage)
          webpage <- readLines(tc) #webpage is now a vector of string each elament is a line of string
          close(tc)
          pagetree <- htmlTreeParse(webpage, useInternalNodes = TRUE)  #pagetree is now in html format and parseable with xpath syntax.
          
          
          
          #########################  NAME  ###############################
          
          restaurant.name<- unlist(xpathApply(pagetree,"//*/span[@class='indexed-biz-name']/a[@*][@*][@*]",xmlValue))
          if(length(restaurant.name)==11)   #Sometimes it gives 11 ellements and the first one is wrong" 
          {restaurant.name<-restaurant.name[2:11]}
          restaurant.name<-as.character(restaurant.name)
          restaurant.name<-gsub("<U+0092>","",restaurant.name)
          restaurant.name
          
          
          
          ############################   REVIEW COUNT  #############################
    
          review.count<-unlist(xpathApply(pagetree,"//*/span[@class='review-count rating-qualifier']",xmlValue))
          review.count
          review.count<-gsub("\n            ", "",review.count) #Removing extra characters"
          review.count<-gsub("\n    ", "",review.count)
          review.count<-gsub(" reviews", "",review.count)
          if(length(review.count)==11)
          {review.count<-review.count[2:11]}
          review.count<-as.numeric(review.count)
          review.count
          
          ############################# PRICE #############################
          
          restaurant.price<-unlist(xpathApply(pagetree,"//*/span[@class='business-attribute price-range']",xmlValue))
          restaurant.price
          for (i in 1:length(restaurant.price))     #Scaling price notations to 1,2,3,4 accordingly where 4 is very epensive.
          {
              if (restaurant.price[i]=="$")  {restaurant.price[i]="1"; }
              if (restaurant.price[i]=="$$")  {restaurant.price[i]="2"; }
              if (restaurant.price[i]=="$$$")  {restaurant.price[i]="3"; }
              if (restaurant.price[i]=="$$$$")  {restaurant.price[i]="4"; }
          }
          restaurant.price
          if(length(restaurant.price)==11)
          {restaurant.price<-restaurant.price[2:11]}
          restaurant.price<-as.numeric(restaurant.price)
          restaurant.price
          
          
          #########################  TYPE   ############################
          
          restaurant.type<-unlist(xpathApply(pagetree,"//*/span[@class='category-str-list']/a[@*][1]",xmlValue))  ##Some times there are several <a> tags. We need the first one.
          if(length(restaurant.type)==11)
          {restaurant.type<-restaurant.type[2:11]}
          restaurant.type<-as.character(restaurant.type)
          restaurant.type
          
          ########################   ADDRESS  ###############################
          restaurant.address<-unlist(xpathApply(pagetree,"//*/span[@class='neighborhood-str-list']",xmlValue))
          
          restaurant.address<-gsub("\n            ", "",restaurant.address)
          restaurant.address<-gsub("        ", "",restaurant.address)
          if(length(restaurant.address)==11)
          {restaurant.address<-restaurant.address[2:11]}
          restaurant.address<-as.character(restaurant.address)
          restaurant.address
          
          ############################  TELL  ##############################
          
          restaurant.tell<-unlist(xpathApply(pagetree,"//*/div[@class='secondary-attributes']/span[@class='biz-phone']",xmlValue))
          restaurant.tell<-gsub("\n        ", "",restaurant.tell)
          restaurant.tell<-gsub("\n    ", "",restaurant.tell)
          if(length(restaurant.tell)==11)
          {restaurant.tell<-restaurant.tell[2:11]}
          restaurant.tell<-as.character(restaurant.tell)
          restaurant.tell
          
          
          ############################### Putting ALL in a DATA FRAME ##############################
          
          
          restaurant.data<-data.frame(NAME=restaurant.name,TYPE=restaurant.type,PRICE= restaurant.price,REVIEW_COUNT=review.count,TELL=restaurant.tell)
          
          
          
          ##################Statistical and Searching Questions   ###############
          
          print(restaurant.data[,choice])
          print(paste("The average price levels is",mean(restaurant.data$PRICE)))
          print(paste("The standard deviation of price levels is",sd(restaurant.data$PRICE)))
          print(paste("The average number of reviews for restaurant is",mean(restaurant.data$REVIEW_COUNT) ))
          print(paste(restaurant.data$NAME[restaurant.data$PRICE==1],"  is inexpensive. ENJOY!!"))
         
          
          #####################  Examples of Using the function#################
}

for (i in 1:1000)
{
  address<-paste("http://www.yelp.com/search?find_desc=Restaurants&find_loc=Boston%2C+MA&start=",as.character(i*10),sep="")
  YelpParse(address,TRUE,TRUE,TRUE,TRUE)
}      
        
        
        
        
        