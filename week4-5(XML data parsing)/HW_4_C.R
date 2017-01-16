# This excercise is to parse xml data formats that are not simply fitted to a data frame.




cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.


library(openxlsx)
library(XLConnect)
library(lubridate)
library(stringr)
library(XML)

#1. (25 Points) Load and then parse the XML document at the URL
#( http://www.cs.washington.edu/research/xmldatasets/data/auctions/ebay.xml ) using
#xmlTreeParse() . The data sets contains bidding information about items on eBay.
#Create any intermediate data objects as deemed necessary to write functions to
#answer the following question: how many auctions had more than 5 bids

url<-"http://www.cs.washington.edu/research/xmldatasets/data/auctions/ebay.xml"
xmlobj<-xmlTreeParse(url)
r<-xmlRoot(xmlobj)
head(r)
xmlName(r)
xmlSize(r)
index<-0

for (i in 1:xmlSize(r))
    {
      
      print(xmlValue(r[[i]][[5]][[5]]))
    
      if(as.numeric(xmlValue(r[[i]][[5]][[5]]))>5) 
      {
        index<-(index+1)
      }
    }
index








#######################################################################################################
#2. (75 Points) Take a look at the data set on trades during a single day for ESZ13 futures
#trades at the URL http://www.barchartmarketdata.com/datasamples/ getHistory15.xml .
#After loading the data, write functions to answer these retrieval queries:

cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.


library(openxlsx)
library(XLConnect)
library(lubridate)
library(stringr)
library(XML)


#a. what was the highest closing price for the security?



url<-"http://www.barchartmarketdata.com/data-samples/getHistory15.xml"

maxClosePrice<-function(url)
{
      xmlobj<-xmlTreeParse(url)
      rt<-xmlRoot(xmlobj)
      xmlName(rt)
      max_close_price<-xmlValue(rt[[2]][[7]]);
      for (i in 3:xmlSize(rt))
      {
        if(xmlValue(rt[[i]][[7]])> max_close_price)
        {
          max_close_price<-xmlValue(rt[[i]][[7]]);
        }
      }
      return (max_close_price)
}
maxClosePrice(url);
##############################################

#b. what was the total volume traded?




url<-"http://www.barchartmarketdata.com/data-samples/getHistory15.xml"
TotalVolumeTrade<-function(url)
{
      xmlobj<-xmlTreeParse(url)
      rt<-xmlRoot(xmlobj)
      xmlName(rt)
      total_volume_traded<-0
      for (i in 2:xmlSize(rt))
        {
          total_volume_traded=total_volume_traded+as.numeric(xmlValue(rt[[i]][[8]]));
        }
      
      return (total_volume_traded)
}
TotalVolumeTrade(url)

##############################################

#c. what was the average trading volume during each HOUR of the trading day;
#place the result into a data




url<-"http://www.barchartmarketdata.com/data-samples/getHistory15.xml"
AverageTradeVolumeHour<-function(url)  #the function returns a data frame with 1st column of day, 2nd column hour and the 3rd column the average volume. 
{
  xmlobj<-xmlTreeParse(url)
  rt<-xmlRoot(xmlobj)
  xmlName(rt)
  df_data<-data.frame(day=rep(0,xmlSize(rt)),hour=rep(0,xmlSize(rt)),volume=rep(0,xmlSize(rt)))
  result<-data.frame(day=0,hour=0,average=0)
  for(i in 1:(xmlSize(rt)-1))
  {
    df_data[i,1]<-as.numeric(day(ymd_hms(xmlValue(rt[[i+1]][[2]]))))
    df_data[i,2]<-as.numeric(hour(ymd_hms(xmlValue(rt[[i+1]][[2]]))))
    df_data[i,3]<-as.numeric(xmlValue(rt[[i+1]][[8]]));
  }
  head(df_data)
  
 
  for (h in 0:23)
  {
          result[h,1]=29  #result data frame[,1] keeps the day
          result[h,2]=h    #result data frame[,2] keeps the hour
          result[h,3]=mean(df_data[((df_data[,2]==h)&(df_data[,1]==29)),3]) #result data frame[,3] keeps the mean of the volume based on hour
  }
  
  

  for (h in 0:23)
  {
    if (df_data[i,1]==30)
    {
        result[h+24,1]=30
        result[h+24,2]=h
        result[h+24,3]=mean(df_data[((df_data[,2]==h)&df_data[,1]==30),3])
    }
  }
  
      return(na.omit(result))
}
      
AverageTradeVolumeHour(url)

  
####################################################