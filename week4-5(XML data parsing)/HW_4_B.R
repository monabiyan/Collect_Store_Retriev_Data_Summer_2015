cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.
library(openxlsx)
library(XLConnect)
library(lubridate)
library(stringr)
library(XML)


##################################################################
#1. (25 Points) Load the XML document at the URL
#( http://www.xmldatasets.net/temp/179681356453762.xml ) directly into a data frame.


url<-"http://www.senate.gov/general/contact_information/senators_cfm.xml"
xmldf<-xmlToDataFrame(url)

#######################################################################
#2. (50 Points) Write a function that returns the names of the senators for a given state,
#i.e., the function takes a state as an argument and returns the names of the senators
#for that state in a vector.



Sennator_name_state<-function (STATE_name)
{
            STATE_name<-as.character(STATE_name) #Make sure it is character
            l=length(xmldf[,1])
            
            index<-1
            Sen_name<-""    #Initializing
            for (i in 1:l)
            {
                  if(!(is.na(xmldf[i,2])|(is.na(xmldf[i,3])|is.na(xmldf$state[i])))) #Skipping NA values
                    { 
                      if (xmldf$state[i]==STATE_name)
                      {
                      Sen_name[index]<-str_c(xmldf[i,3],xmldf[i,2],sep = " ")
                      index<-index+1
                      }
                  }
            }
            return (Sen_name)
}
Sennator_name_state("TN")

##################################################################
#3. (25 Points) Write a function that returns the phone number for a given senator.


Phone_number<-function (Senator_name)    #As input user can put "Last name" or "First and Last name"
{
          splitted<-str_split(Senator_name, " ")[[1]]
          
          if (splitted[1]==" ") {return("Please do not use SPACE")}  #Avoid Using space in the beginning
          if (splitted[length(splitted)]==" ") {return("Please do not use SPACE")} #Avoid using space in the last
          if (length(splitted)==1)   #Just Family name
              {
                Family_name<-Senator_name;
              }
          if(length(splitted)==2)    # First name and Last name
              {
                First_name<-splitted[1]
                Family_name<-splitted[2]
              }
          l=length(xmldf[,1])
          index<-1
          for (i in 1:l)
          {
            if(!(is.na(xmldf[i,2])|(is.na(xmldf[i,3])|is.na(xmldf$phone[i]))))  #Skipping NA values
                {
                  if (Family_name==xmldf[i,2])
                  {
                    return(as.character(xmldf$phone[i]))
                  }
                }  
          }
}

Phone_number("Kelly Ayotte")
Phone_number("Ayotte")

#####################################################################