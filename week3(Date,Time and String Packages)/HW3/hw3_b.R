
#Mohsen Nabian
#NU ID : 001100429
#Assignment 2
#5/24/2015
#Input dataset is "Airline Delays Analysis"


#############################################   PREPRATION  and Data Scanning  #####################################################
install.packages("lubridate")
library(lubridate)
install.packages("ggplot")
library(ggplot2)

#Clearing everything#
cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.



# We first need to specify our working directory address in the hard drive.
setwd("C:/Users/nabian.m/OneDrive/Summer 2015/Collect,retrieve data/week3(Date,Time and String Packages)/HW3")


#Now we want to Scan the data file into our work space. 
data_original<-read.csv("Bird Strikes.csv",header=TRUE,sep=",",stringsAsFactors=FALSE,strip.white = TRUE, na.strings = c("NA",""))
data_original_doubled<-read.csv("Bird Strikes_doubled.csv",header=TRUE,sep=",",stringsAsFactors=FALSE,strip.white = TRUE, na.strings = c("NA",""))
data_original_tripled<-read.csv("Bird Strikes_tripled.csv",header=TRUE,sep=",",stringsAsFactors=FALSE,strip.white = TRUE, na.strings = c("NA",""))
head(data_original)

#The missing data is considered as both NA as well as empty values. The system automatically put NA as missing.
#The airline variable is considered as data frame here. Data fram is composed of several vectors of different type but the same size.
#for this project, working with dataframe is the best choice since we can call the desirerd vector elements by typing:airline$VectorName[i] 
#In fact we have access to all data in one data fame which is "airline".

########################### Function 1 ##################################
#  Question 1)   How many bird strikes were not reported, i.e. , for which there is no value
#for " Reported: Date ".

how_many_empty<-function(data_frame)
{
  vector=data_frame[,22];
  index=0;
  str_date<-as.character(vector)   ####Not necessarilly needed since the uploaded data inn the data fram are either character or numeric.
  
  empty_no=sum(is.na(vector));
  return(empty_no)
  print(empty_no)
}
how_many_empty(data_original);

##############################################################################



####################   Function  A    ########################################
#This function, recives a vector and cleans all missing data and outputs 
#data frame with the first column as the data index and the second as the string cleaned data. 

complete_date_with_index <-function(vector)
{
  index<-0;
  full_index<-0;
  full_string<-"";
  str_date<-as.character(vector);
  for (i in 1:length(vector))    # length(vector) is O(1) in R
  {
    if (is.na(vector[i])==0)  
    {
      index<-index+1;
      full_index[index]<-i;
      full_string[index]<-vector[i];
    }
  }
  CLEAN_date<-data.frame(full_index,full_string);
  return(CLEAN_date) 
}
head(COMPLETE_DATE_TIME<-complete_date_with_index(data_original[,22]))
#######################################




################################ Function 3   #######################
#Question 3) How many bird strikes were there for each year? Place the result into a
#data frame.

How_many_strike_each_year<-function(data_frame)
{
  library(lubridate)
  date_time_vector=data_frame[,22];
  COMPLETE_DATE_TIME<-complete_date_with_index(date_time_vector)  # We use this complementary function to clean our data
  tm.lub<-mdy_hm(COMPLETE_DATE_TIME[,2])
  YEAR_vector=year(tm.lub);
  YEAR_table<-as.data.frame(table(YEAR_vector))
  YEAR_table[,1]<-as.character(YEAR_table[,1])  #To change the years into character for easier manupulations
  return(YEAR_table)
  print(YEAR_table)
}
How_many_strike_each_year(data_original)
###########################################################################



##############################  Function 2 #######################################
#Question 2) Which year had the most bird strikes? Write a function to calculate.

which_year_most_strike<-function(data_frame)
{
  Year_dataframe<-How_many_strike_each_year(data_frame)
  most_strike_year<- Year_dataframe[(Year_dataframe[,2]==max(Year_dataframe[,2])),1]     #~O(2N)=O(N)
  return(most_strike_year);
  print(most_strike_year);
}
which_year_most_strike(data_original)
####################################################################################


#################################     Function 4  ##############################
#Question 4)    Which airline had the most bird strikes? Write a function to calculate the
#number of birds strikes per airlines and then put those results into a data frame.
#Determine the answer from the data frame by writing another function to which you
#pass the frame. (Note that the most bird strikes could not be associated with an airline,
#so UNKNOWN is actually the most common, but that's not an actual airline but a
#marker for a missing value; in that case report the second most as it's an actual
#airline.)

which_airline_most_strike<-function(data_frame)
{
    airline_vector_chr<-as.character(data_frame[,15]) #reading the airlines as a character vector
    AIRLINE_table<-as.data.frame(table(airline_vector_chr))  #put the airline information into a table to hve the frequency and puting the table into a data frame to easily work with it.
    AIRLINE_table[,1]<-as.character(AIRLINE_table[,1])   #By default it has the class of factor and the mode of numeric. We better to change it to character which would be easy to work with.
    AIRLINE_table$Freq[AIRLINE_table[,1]=="UNKNOWN"]<-(-1);  #trying to minimize (-1) the frequency of the UNKNOWN element in order not be considered as maximum.
    Nmax=max(AIRLINE_table$Freq)
    most_strike_airline=AIRLINE_table[(AIRLINE_table[,2]==Nmax),1]     #O(N)
    return(most_strike_airline)
    print(most_strike_airline)
}
which_airline_most_strike(data_original)

###################################   Q5    #############


 #In order to investigate the time and space complexities, we first estimate the order of complexitis in each function.
 # There are some notes regarding complexities of some important sentences.  

#  Function 1:   
            #   N=37 * n       n= number of data in each column
            #   Time complexity O(n)     #Space complexity O(N)

#  Function A:   
            #   N=37 * n       n= number of data in each column
            #   Time complexity O(n)     #Space complexity O(N)

#  Function 2:   
            #   N=37 * n       n= number of data in each column
            #   Time complexity O(n)     #Space complexity O(N)
            # Comment:  This function used function A nad Function 3 which are both O(n). However, based on the algorithm used, the complexity remains O(N)

#  Function 3:   
            #  N=37 * n       n= number of data in each column
            #Time complexity O(n)     #Space complexity O(N)
            #Comment:  This function used function A which is O(n). However, based on the algorithm used, the complexity remains O(N)
#  Function 4:     
            #  N=37 * n       n= number of data in each column
            #Time complexity O(n)     #Space complexity O(N)


#It turns out that the complexities in each function is Time O(n) ans space O(N)
# consiquently, if we have 2X,10X,100X,1000X more data, we would have respectively 2,10,100 and 1000 timely and spacely expensive. 
#
#####################################################


##########################   Q6     ##############################

func1_run_time_n<-system.time(how_many_empty(data_original))
f1_n<-(as.numeric(func1_run_time_n))[1]
f1_n


func2_run_time_n<-system.time(which_year_most_strike(data_original))
f2_n<-(as.numeric(func2_run_time_n))[1]
f2_n


func3_run_time_n<-system.time(How_many_strike_each_year(data_original))
f3_n<-(as.numeric(func3_run_time_n))[1]
f3_n


func4_run_time_n<-system.time(which_airline_most_strike(data_original))
f4_n<-(as.numeric(func4_run_time_n))[1]
f4_n



func1_run_time_2n<-system.time(how_many_empty(data_original_doubled))
f1_2n<-(as.numeric(func1_run_time_2n))[1]
f1_2n


func2_run_time_2n<-system.time(which_year_most_strike(data_original_doubled))
f2_2n<-(as.numeric(func2_run_time_2n))[1]
f2_2n


func3_run_time_2n<-system.time(How_many_strike_each_year(data_original_doubled))
f3_2n<-(as.numeric(func3_run_time_2n))[1]
f3_2n

func4_run_time_2n<-system.time(which_airline_most_strike(data_original_doubled))
f4_2n<-(as.numeric(func4_run_time_2n))[1]
f4_2n


func1_run_time_3n<-system.time(how_many_empty(data_original_tripled))
f1_3n<-(as.numeric(func1_run_time_3n))[1]
f1_3n


func2_run_time_3n<-system.time(which_year_most_strike(data_original_tripled))
f2_3n<-(as.numeric(func2_run_time_3n))[1]
f2_3n


func3_run_time_3n<-system.time(How_many_strike_each_year(data_original_tripled))
f3_3n<-(as.numeric(func3_run_time_3n))[1]
f3_3n


func4_run_time_3n<-system.time(which_airline_most_strike(data_original_tripled))
f4_3n<-(as.numeric(func4_run_time_3n))[1]
f4_3n

# Now we make data frame out of the running time data
n<-length(data_original[,1])
run_time<-data.frame(N=c(n,2*n,3*n),func1=c(f1_n,f1_2n,f1_3n),func2=c(f2_n,f2_2n,f2_3n),func3=c(f3_n,f3_2n,f3_3n),func4=c(f4_n,f4_2n,f4_3n));
run_time

mr1<-lm(func1 ~ N ,data=run_time)
summary(mr1) 
mr2<-lm(func2 ~ N ,data=run_time)
summary(mr2) 
mr3<-lm(func3 ~ N ,data=run_time)
summary(mr3) 
mr4<-lm(func4 ~ N ,data=run_time)
summary(mr4) 

ggplot(run_time, aes(N,func1)) + geom_line()
ggplot(run_time, aes(N,func2)) + geom_line()
ggplot(run_time, aes(N,func3)) + geom_line()
ggplot(run_time, aes(N,func4)) + geom_line()
############  PLOT ?








##############################################