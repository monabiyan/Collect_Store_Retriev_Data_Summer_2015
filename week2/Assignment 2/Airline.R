
#Mohsen Nabian
#NU ID : 001100429
#Assignment 2
#5/24/2015
#Input dataset is "Airline Delays Analysis"


#############################################   PREPRATION    #####################################################

#Clearing everything#
cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.



# We first need to specify our working directory address in the hard drive.
setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week2/Assignment 2")



#Now we want to upload the data file into our work space. 
airline<-read.table("AirLineDelays.txt",header=TRUE,sep=",")
#The airline variable is considered as data frame here. Data fram is composed of several vectors of different type but the same size.
#for this project, working with dataframe is the best choice since we can call the desirerd vector elements by typing:airline$VectorName[i] 
#In fact we have access to all data in one data fame which is "airline".



#MISSING ELEMENTS & OUR STRATEGY

#The strategy regarding missing data in all of our functions  is that we keep them in our data frame but once we encounter them, we simply ignore them and 
#we skip to the next value
#We did not remove the whole rows with delay for 2 reasons: 1) too many or almost all rows contain NA. 2) Evenif we have NA in a row
# They still convey important information for their non-missing elemnet that must be taken into account for more accurate analysis.   


#To have a sense of the available values in our data frame we are trying to see the first 6 rows of the dataset. 
head(airline);
##################################################################################################################







###############################################   FUNCTION 1    ###############################################

#TotalNumDelays(carrier) finds the total number of any type of positive delays occured for the chosen carrier name. 
#Missing Elements are simply skipped.

TotalNumDelays<-function(Carrier)
{ 
  if(any(ls() %in% "airline")==FALSE)  #To make sure data is loaded and to avoid duplication in data loading 
  {
    setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week2/Assignment 2");
    airline<-read.table("AirLineDelays.txt",header=TRUE,sep=",");
  }
  if(any(ls() %in% "airline")==FALSE) stop('The file is not loading correctly');   #gives a message if the address is wrong and exits the function
  numdelay=0;
  for (i in 1:nrow(airline))
  { 
    if (airline[i,8] == 0)                          #Cancelled flight are not counted.
    {  
      if (airline$CARRIER[i]==Carrier)
      {
        for (j in c(6,7,9,10,11,12))               #All delay vectors are counted.
        {
          if (is.na(airline[i,j]==TRUE)) 
          {
            j=j+1;
          }
          else if (airline[i,j]>0)
              numdelay=numdelay+1
        }
      }
    }
  }
  return (numdelay)
  print(numdelay)
}
############################################################################################################################






###############################################   FUNCTION 2   ###############################################################
#TotalDelaysByOrigin(Origin) prints and return the total positive delays (min) occured in the origin location enterd as input. 
#Missing Elements are simply skipped.   
TotalDelaysByOrigin<-function (Origin)
{
  
  #To make sure data is loaded and to avoid duplication in data loading
  if(any(ls() %in% "airline")==FALSE)
  {
    setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week2/Assignment 2");
    airline<-read.table("AirLineDelays.txt",header=TRUE,sep=",");
  }
  if(any(ls() %in% "airline")==FALSE) stop('The file is not loading correctly');  #gives a message if the address is wrong and exits the function
  min_delay=0;
  for (i in 1:nrow(airline))
  { 
    if (airline[i,8]==0)                            #Cancelled flight are not counted.
    {  
      if (airline$ORIGIN[i]==Origin)
      {
        for (j in c(6,7,9,10,11,12))                #All delay vectors are counted.
        {
          if ((is.na(airline[i,j])==TRUE) | airline[i,j]<0) 
          {
            j=j+1;
          }
          else 
          {
            min_delay<-min_delay+ airline[i,j];
          }
        }
      }
    }
  }
  return(min_delay)
  print(min_delay)
}
############################################################################################################################




################################################  FUNCTION 3  ##############################################################
#AvgDelay(Carrier,Dest) function  prints and return the average of positive delays (min) occured in the input carrier and destination.  
#Missing Elements are simply skipped.
AvgDelay<-function(Carrier,Dest)
{
  if(any(ls() %in% "airline")==FALSE)
  {
    setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week2/Assignment 2");
    airline<-read.table("AirLineDelays.txt",header=TRUE,sep=",");
  }
  if(any(ls() %in% "airline")==FALSE) stop('The file is not loading correctly');  #gives a message if the address is wrong and exits the function
  average<-0;
  totdelay<-0;
  s<-0;
  for (i in 1:nrow(airline))
  { 
    if (airline[i,8]==0)                               #Cancelled flight are not counted.
    {
      if ((airline$CARRIER[i]==Carrier))
      {
        if(airline$DEST[i]==Dest)
        {
          for (j in c(6,7,9,10,11,12))                 #All delay vectors are counted.
          {
            if ((is.na(airline[i,j])==TRUE) | airline[i,j]<0)
            {
              j<-j+1;
            }
            else 
            {
              totdelay<-totdelay+ airline[i,j];
              s<-s+1;
            }
          }
        }
      }
    }
  }
  average_delay=totdelay/s;
  return(average_delay);
  print(average_delay);
}
#######################################################################################################################





#####################################################  EXAMPLES  ######################################################

TotalNumDelays("AA");
TotalDelaysByOrigin("BOS")
AvgDelay("AA","BOS")  

#######################################################################################################################


