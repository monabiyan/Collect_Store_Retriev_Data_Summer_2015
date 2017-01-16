
#Mohsen Nabian
#NU ID : 001100429
#Assignment 2
#5/24/2015
#Input dataset is "Airline Delays Analysis"


#############################################   PREPRATION    #####################################################
library(lubridate)
#Clearing everything#
cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.



# We first need to specify our working directory address in the hard drive.
setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week3/HW3")



#Now we want to upload the data file into our work space. 
data_original<-read.table("Acquisitions.csv",header=TRUE,sep=",",stringsAsFactors=FALSE,strip.white = TRUE, na.strings = c("NA",""))
#The airline variable is considered as data frame here. Data fram is composed of several vectors of different type but the same size.
#for this project, working with dataframe is the best choice since we can call the desirerd vector elements by typing:airline$VectorName[i] 
#In fact we have access to all data in one data fame which is "airline".

##############################################   Function 1   #######################################################################3

leastInvInterval<-function(data_frame)
{
    day_count<-as.numeric(as.Date(data_frame$Date,format="%m/%d/%Y"))  #chaning dates into a day number since 1/1/1970
    min_interval=9999999;
    for (i in 1:(length(day_count)-1))
    {
      for (j in (i+1):length(day_count))
        {
            temp=abs(day_count[i]-day_count[j])
            if (temp<min_interval) min_interval<-temp;
        }
    }
    return(min_interval);
    print(min_interval);
}

leastInvInterval(data_original)

##############################################################################################################################





