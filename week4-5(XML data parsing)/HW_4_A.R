#Mohsen Nabian HW4_A

#QUESTION
#Before diving into the programming problems, study the data file "2013 Geographric Coordinate Spreadsheet for U S  Farmers Markets 8'3'1013"  that is provided for the assignment: 
#(25 Points) Load the data file into a data frame.
#(50 Points) The seasons are not standardized and would make analysis difficult. Create six levels of seasons: Summer, Fall, Winter, Spring, Year-Round, Half-Year and convert each provided season (in the Season1Date column) to one of the seasons. Come up with reasonable rules, for example, June to August would be Summer, while 5/1 to 10/30 would be Half-Year. You need to use string processing functions to parse the strings and shape the data into the categories. If there are missing end dates, ignore the entire row.
#(25 Points) Write a retrieval function that allow a data scientist to find which markets accepts WIC.

##################################################################################################

cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.



Season_Categorizer<-function(Fileaddress)
{

                  #####################################################################################
                  #1. (25 Points) Load the data file into a data frame.
                  library(openxlsx)
                  library(XLConnect)
                  library(lubridate)
                  library(stringr)
  
  
                  data_orig <- read.xlsx(Fileaddress,sheet=1,startRow =3,colName=TRUE)
                  
                  #wb <- loadWorkbook("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week4/Geographic.xlsx")
                  #data_orig <- readWorksheet(wb, sheet = "Export", header = TRUE,startRow = 3,startCol = 1, endCol = 45)  #data_orig is a data frame containing all the data. 
                  head(data_orig)
                  ####################################################################################
                  #2. (50 Points) The seasons are not standardized and would make analysis difficult.
                  #Create six levels of seasons: Summer, Fall, Winter, Spring, YearRound,HalfYear
                  #and convert each provided season (in the Season1Date column) to one of the
                  #seasons. Come up with reasonable rules, for example, June to August would be
                  #Summer, while 5/1 to 10/30 would be HalfYear.You need to use string processing
                  #functions to parse the strings and shape the data into the categories. If there are
                  #missing end dates, ignore the entire row.
                  
                  date_vector<-as.character(data_orig$Season1Date)   
                  l<-length(date_vector)
                  
                  date_frame<-data.frame(index_1=c(1:l),Date_2=date_vector,contain_to_3=rep(1,l),start_date_4=rep("Blank",l),finish_date_5=rep("Blank",l),start_month_6=rep(0,l),finish_month_7=rep(0,l),start_year_8=rep(0,l),finish_year_9=rep(0,l),category_10=rep("Blank",l))    # we further want to omit all NA rows but we need to keep the index of each rows to simply not to loose each row's address.First_check creats a vector of zeros for future use.  
                  
                  date_frame$index_1<-as.numeric(date_frame$index_1)
                  date_frame$Date_2<-as.character(date_frame$Date_2)
                  date_frame$contain_to_3<-as.logical(date_frame$contain_to_3)
                  date_frame$start_date_4<-as.character(date_frame$start_date_4)
                  date_frame$finish_date_5<-as.character(date_frame$finish_date_5)
                  date_frame$start_month_6<-as.numeric(date_frame$start_month_6)
                  date_frame$finish_month_7<-as.numeric(date_frame$finish_month_7)
                  date_frame$start_year_8<-as.numeric(date_frame$start_year_8)
                  date_frame$finish_year_9<-as.numeric(date_frame$finish_year_9)
                  date_frame$category_10<-as.character(date_frame$category_10)
                  
                  
                  
                  dates_frame<-na.omit(date_frame)
                  l<-length(dates_frame[,1])  # l is updated, because of omitting NA rows.
                  
                  
                  
                  ##### Some correction in the address element to make them more universal and detectable################
                  
                  dates_frame[,2]<-sub(" Sept ", "September ",x=dates_frame[,2])
                  dates_frame[,2]<-sub("toSeptember", "to September",x=dates_frame[,2])
                  dates_frame[,2]<-sub(" Date ", " ",x=dates_frame[,2])
                  dates_frame[,2]<-sub(" Date ", " ",x=dates_frame[,2])
                  dates_frame[,2]<-sub("Start ", "",x=dates_frame[,2])
                  dates_frame[,2]<-sub("End ", "",x=dates_frame[,2])
                  
                  ################################################################
                  
                  
                  
                  
                  
                  ############### Those who have "to" as the last word would get -1, doesn't have "to" get 0, has "Date to" format, gets 2###
                  for (i in 1:l)
                  {
                    split<-str_split(dates_frame$Date_2[i]," ")[[1]]
                    if(split[length(split)]=="to")
                    {
                      dates_frame[i,3]<-(-1)                           # -1 for those who have nothing after "to"
                    }
                    if(sum(split=="to")==0) dates_frame[i,3]<-(0)      # 0 do not have "to". the format is like "41441" meaningless. there is only 5 of them. we ignore them.    
                    if (sum(grepl("Date",split))>0)  dates_frame[i,3]<-(2)    # 2 for those who are in the format of "Start Date June 11 to End Date October 8 "
                  }
                  
                  
                  ###################################################################################
                  
                  
                  
                  
                  
                  ############### We want to seprate start and finish date with "to" seprator######
                  string_one<-"Blank"
                  string_two<-"Blank"
                  for (i in 1:l)
                  {
                    print("please wait.")
                    if(dates_frame[i,3]==1)
                    {
                        h=0;
                        split<-str_split(dates_frame[i,2]," ")[[1]]
                        for (j in 1:length(split))
                        {
                          if(split[j]=="to")  { h<-j }
                        }
                        
                          string_one<-str_c(split[1:(h-1)],collapse = " ")
                          dates_frame[i,4]<-string_one
                        
                          string_two<-str_c(split[(h+1):length(split)],collapse = " ")
                          dates_frame[i,5]<-string_two
                    }
                  }
                  #####################################################################################
                  
                  
                  
                  
                  
                  
                  
                  
                  ############# We want to find the month and year of both starting and finishing dates############
                  
                  
                  for (i in 1:l)
                  {
                    print("please wait..")
                    INDEX1<-FALSE
                    INDEX2<-FALSE
                    
                    if(dates_frame[i,3]==1)
                    {
                      if (grepl("/", dates_frame[i,4])==TRUE)    #some have the format "10/23/2012"
                      {
                        date<-mdy(dates_frame[i,4])
                        dates_frame[i,6]<-month(date)
                        dates_frame[i,8]<-year(date)
                        INDEX1<-TRUE                          #That means start data has been read.
                      }
                      if (grepl("/", dates_frame[i,5])==TRUE)     
                      {
                        date<-mdy(dates_frame[i,5])
                        dates_frame[i,7]<-month(date)
                        dates_frame[i,9]<-year(date)
                        INDEX2<-TRUE                           #That means finish data has been read.
                      }
                      
                      if (grepl(",", dates_frame[i,4])==TRUE)      #some have the format  "June 17, 2012"
                      {
                        date<-mdy(dates_frame[i,4])
                        dates_frame[i,6]<-month(date)
                        dates_frame[i,8]<-year(date)
                        INDEX1<-TRUE                            #That means start data has been read.
                      }
                      if (grepl(",", dates_frame[i,5])==TRUE)
                      {
                        date<-mdy(dates_frame[i,5])
                        dates_frame[i,7]<-month(date)
                        dates_frame[i,9]<-year(date)
                        INDEX2<-TRUE                            #That means finish data has been read.
                      }
                  
                      if (INDEX2==FALSE)
                      {
                        dates_frame[i,7]<-grep(str_split(dates_frame[i,5]," ")[[1]][1],c(month.name))  # still we have some with the format "June 11 to October 8" so we try to get the first part which indicates the month.
                        dates_frame[i,9]<-2013
                        INDEX2<-TRUE  
                      } 
                      if ((INDEX1==FALSE)&(INDEX2==FALSE))
                      {
                        dates_frame[i,6]<-grep(str_split(dates_frame[i,4]," ")[[1]][1],c(month.name))
                        dates_frame[i,8]<-2013
                        INDEX1<-TRUE  
                      }
                      if ((INDEX1==FALSE)&(INDEX2==TRUE))           #Some times we have the follwing format:"June to August 29, 2012 "
                      {
                        dates_frame[i,6]<-grep(str_split(dates_frame[i,5]," ")[[1]][1],c(month.name))
                        dates_frame[i,8]<-dates_frame[i,9]
                        INDEX1<-TRUE  
                      }
                      
                    }
                  }
                  ####################################################################################
                  
                  
                  
                  
                  
                  
                  
                  ##################  Here we add 12 and 24 to all month info with year=2013 and 2014 respectively############
                  for (i in 1:l)
                  {
                    print("please wait...")
                    if(dates_frame[i,3]==1)
                    {
                        if( dates_frame[i,8]==2013) 
                        {
                          dates_frame[i,6]<-(dates_frame[i,6]+12);
                        }
                        if( dates_frame[i,9]==2013) 
                        {
                          dates_frame[i,7]<-(dates_frame[i,7]+12);
                        }
                        if( dates_frame[i,8]==2014) 
                        {
                          dates_frame[i,6]<-(dates_frame[i,6]+24);
                        }
                        if( dates_frame[i,9]==2014) 
                        {
                          dates_frame[i,7]<-(dates_frame[i,7]+24);
                        }
                    }
                  }
                  
                  #######################################################################
                  
                  
                  
                  
                  
                  
                  ######################### Here we construct our decision matrix  #######################
                  # The strategy is that month{3,4,5} are spring;month{6,7,8} are summer;month{9,10,11} are fall;month{12,1,2} are winter;
                  #We consider 8 month and more as Full Year
                  #We consider 4 month and more up to 7 month as Half Year
                  
                  decision_mat<-matrix(rep("0",(36*36)),nrow=36,ncol=36)
                  
                  
                  s_list<-c("winter","winter","spring","spring","spring","summer","summer","summer","fall","fall","fall","winter","winter","winter","spring","spring","spring","summer","summer","summer","fall","fall","fall","winter");
                  
                  
                  for (i in 1:36)
                  {
                    decision_mat[i,i]<-s_list[i]
                  }
                  
                  for (i in 1:36)
                  {
                    for (j in i:36)
                    {
                      if ((j-i)>7)  {decision_mat[i,j]<-"Full Year"}
                      if (((j-i)>3)&((j-i)<8))  {decision_mat[i,j]<-"Half Year"}
                      if ((j-i)==3) {decision_mat[i,j]<-s_list[j]}
                      if ((j-i)==2) {decision_mat[i,j]<-s_list[j]}
                      if ((j-i)==1) {decision_mat[i,j]<-s_list[i]}
                    }
                  }
                  
                  ##################################################################################################################
                  ###############Here we do the categorization and put it into the 10th column of our DF   ########################################################3
                  for (i in 1:l)
                  {
                    if(dates_frame[i,3]==1)
                    {  
                        print("please wait....")
                        dates_frame[i,10]<-decision_mat[dates_frame[i,6],dates_frame[i,7]]
                    }
                  }
                  ############################################################################
                  return(dates_frame[,c(1,2,10)])

}

Season_Categorizer("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week4-5/Geographic.xlsx")








########################################################################################################
####################################################################################################
#########################################################################################################



#3. (25 Points) Write a retrieval function that allow a data scientist to find which markets
#accepts WIC.



WIC_list<-function(Fileaddress)
{
  
                ############################################## HERE we want to list all companies who have WIC ##################
                library(openxlsx)
                library(XLConnect)
                library(lubridate)
                library(stringr)
                
                data_orig <- read.xlsx(Fileaddress,sheet=1,startRow =3,colName=TRUE)
                l<-length(data_orig[,2])
                data_orig[,2]<-as.character(data_orig[,2])
                data_orig[,21]<-as.character(data_orig[,21])
                index=1
                Company_List_WIC<-"BLANK"
                for (i in 1:l)
                {
                  if (data_orig$WIC[i]=="Y")
                  {
                    Company_List_WIC[index]<-data_orig[i,2]
                    index=index+1;
                  }
                }
                #############################  However, we have some companies who have different locations, thus listed multiple times.
                # we need to detect them and removed the repetitions.
                
                
                ###########  Here we try to standardize the arbitrary formats to "Farmers" and will change the names accordingly
                l=length( Company_List_WIC)
                Farmers<-c("farmers","farmer's","farmers'","Farmer's","Farmers'");
                for (i in 1:l)
                {
                  for (k in 1:5)
                  {
                    Company_List_WIC[i]<-sub(Farmers[k],"Farmers",x=Company_List_WIC[i])  
                  }
                }
                
                ###################################################################
                
                ########## This is a first Filter to remove the exact same names.
                l=length( Company_List_WIC)
                rep<-0
                for (i in 1:(l-1))
                  {
                    for (j in (i+1):l)
                      {
                        if((Company_List_WIC[j]!="REPEATED"))
                        {
                          if (Company_List_WIC[i]==Company_List_WIC[j])
                          {
                            Company_List_WIC[j]<-"REPEATED"
                            rep<-rep+1
                          }
                        }
                      }
                  }
               return  (Company_List_WIC)  
                ######################################################
}

WIC_list("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week4-5/Geographic.xlsx")

              