cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.


# in this program I have a big table of data and I want to normalize the data into 
#several tables. Then I want to create a database and put the tables in the data base. 
#Finally, I will write a few queries to make sure things are in the right place. 

############################################################################

install.packages("sqldf")
library(sqldf)
#1)Preparing Dataframes for our database Tables
setwd("C:/Users/monabiyan/Desktop")
dataframes<-read.csv("C:/Users/monabiyan/Desktop/Bird_Strikes.csv",header=TRUE,sep=",",stringsAsFactors=FALSE)
for (i in 1:length(dataframes[1,]))
{dataframes[,i]<-as.character(dataframes[,i])}


Flight_ID<-rep("1",length(dataframes$Record.ID))
AirCraft_ID<-rep("1",length(dataframes$Record.ID))
AirPort_ID<-rep("1",length(dataframes$Record.ID))
WildLife_ID<-rep("1",length(dataframes$Record.ID))


birdstrike_df<-data.frame((dataframes$Record.ID),  #1
                          (Flight_ID),    #2
                          (AirCraft_ID), #3
                          (AirPort_ID), #4
                          (WildLife_ID), #5
                         (dataframes$When..Time..HHMM.), #6
                         (dataframes$Reported..Date),#7
                         (dataframes$When..Phase.of.flight), #8
                         (dataframes$Conditions..Sky), #9
                         (dataframes$Pilot.warned.of.birds.or.wildlife.), #10
                         (dataframes$Wildlife..Number.struck), #11
                         (dataframes$Feet.above.ground), #12
                         (dataframes$Speed..IAS..in.knots), #13
                         (dataframes$Conditions..Sky), #14
                         (dataframes$Cost..Repair..inflation.adj.), #15
                         (dataframes$Cost..Aircraft.time.out.of.service..hours.),#16
                         (dataframes$Effect..Impact.to.flight), #17
                         (dataframes$Effect..Indicated.Damage), #18
                         (dataframes$Effect..Other), #19
                         (dataframes$Remains.of.wildlife.collected.), #20
                         (dataframes$Location..Nearby.if.en.route), #21
                         (dataframes$Remarks)); #22

aircraft_df<-data.frame(AirCraft_ID,
                        (dataframes$Aircraft..Make.Model),
                        (dataframes$Aircraft..Type),
                        (dataframes$Altitude.bin),
                        (dataframes$Aircraft..Number.of.engines.))


airport_df<-data.frame((AirPort_ID),
                        (dataframes$Airport..Name),
                        (dataframes$Origin.State))

flight_df<-data.frame((Flight_ID),
                       (dataframes$Aircraft..Flight.Number),
                      (dataframes$FlightDate),
                      (dataframes$Aircraft..Airline.Operator),
                      (dataframes$Airport..Name))

wildlife_df<-data.frame(WildLife_ID,
                        dataframes$Wildlife..Species,
                        dataframes$Wildlife..Size)

#############################   Removing repetitive raws

aircraft_df<-aircraft_df[!duplicated(aircraft_df), ]
airport_df<-airport_df[!duplicated(airport_df), ]
flight_df<-flight_df[!duplicated(flight_df), ]
wildlife_df<-wildlife_df[!duplicated(wildlife_df), ]
##########################################################


##################  Now renewing the first column(index column) #####
aircraft_df[,1]<-c(1:length(aircraft_df[,1]))
airport_df[,1]<-c(1:length(airport_df[,1]))
flight_df[,1]<-c(1:length(flight_df[,1]))
wildlife_df[,1]<-c(1:length(wildlife_df[,1]))

head(aircraft_df)
###################################################
############change all to charcter############

for (i in 1:length(birdstrike_df[1,]))
{birdstrike_df[,i]<-as.character(birdstrike_df[,i])}

for (i in 1:length(aircraft_df[1,]))
{aircraft_df[,i]<-as.character(aircraft_df[,i])}

for (i in 1:length(airport_df[1,]))
{airport_df[,i]<-as.character(airport_df[,i])}

for (i in 1:length(flight_df[1,]))
{flight_df[,i]<-as.character(flight_df[,i])}

for (i in 1:length(wildlife_df[1,]))
{wildlife_df[,i]<-as.character(wildlife_df[,i])}



for (i in 1:length(dataframes[,1]))
{
   print(i)
   search<-(dataframes$Airport..Name[i]==airport_df$X.dataframes.Airport..Name.)&
   (dataframes$Origin.State[i]==airport_df$X.dataframes.Origin.State.)
 
   if (sum(search))
    {
      birdstrike_df$X.AirPort_ID.[i]=airport_df$X.AirPort_ID.[which(search==TRUE)]
    }
}



for (i in 1:length(dataframes[,1]))
{
  
   search<-(dataframes$Aircraft..Make.Model[i]==aircraft_df$X.dataframes.Aircraft..Make.Model.)&
    (dataframes$Aircraft..Type[i]==aircraft_df$X.dataframes.Aircraft..Type.)&
    (dataframes$Altitude.bin[i]==aircraft_df$X.dataframes.Altitude.bin.)&
    (dataframes$Aircraft..Number.of.engines.[i]==aircraft_df$X.dataframes.Aircraft..Number.of.engines..)
  
  if (sum(search))
  {
    birdstrike_df$X.AirCraft_ID.[i]=aircraft_df$AirCraft_ID[which(search==TRUE)]
    print(i)
  }
  
}



for (i in 1:length(dataframes[,1]))
{
  
  search<-(dataframes$FlightDate[i]==flight_df$X.dataframes.FlightDate.)&
    (dataframes$Aircraft..Flight.Number[i]==flight_df$X.dataframes.Aircraft..Flight.Number.)&
    (dataframes$Aircraft..Airline.Operator[i]==flight_df$X.dataframes.Aircraft..Airline.Operator.)&
    (dataframes$Airport..Name[i]==flight_df$X.dataframes.Airport..Name.)
  
  if (sum(search))
  {
    birdstrike_df$X.Flight_ID.[i]=flight_df$X.Flight_ID.[which(search==TRUE)]
    print(i)
  }
  
}

 for (i in 1:length(dataframes[,1]))
{
  
  search<-
    (dataframes$Wildlife..Species[i]==wildlife_df$dataframes.Wildlife..Species)&
    (dataframes$Wildlife..Size[i]==wildlife_df$dataframes.Wildlife..Size)
   
  
  if (sum(search))
  {
    birdstrike_df$X.WildLife_ID.[i]=wildlife_df$WildLife_ID[which(search==TRUE)]
    print(i)
  }
  
}


######### Changing the column names of our data frame to be fit with our database table##






######writing dataframes in excel file #######
write.csv(birdstrike_df,file="birdstrike.csv",row.names = FALSE)  #make sure names are false 
write.csv(aircraft_df,file="aircraft.csv",row.names = FALSE)
write.csv(airport_df,file="airport.csv",row.names = FALSE)
write.csv(flight_df,file="flight.csv",row.names = FALSE)
write.csv(wildlife_df,file="wildlife.csv",row.names = FALSE)
##################################

######reading dataframes in excel file #######   #We always can start here since the cleaned data are saved###


library(sqldf)


setwd("C:/Users/monabiyan/Desktop/Bird_Strikes.csv")

birdstrike_df<-read.csv(file="birdstrike.csv")
aircraft_df<-read.csv(file="aircraft.csv")
airport_df<-read.csv(file="airport.csv")
flight_df<-read.csv(file="flight.csv")
wildlife_df<-read.csv(file="wildlife.csv")
##################################
ncol(birdstrike_df)




#### Creating the main database with all definitions #####


db1 <- dbConnect(SQLite(), dbname="birdstrike.sqlite")

#dbRemoveTable(conn=db1,"BirdStrike")
dbSendQuery(conn = db1,  "CREATE TABLE BirdStrike (
            RecordID INTEGER PRIMARY KEY, 
            Flight_ID INTEGER REFERENCES Flight_info(Flight_ID),
            AirCraft_ID INTEGER REFERENCES AirCraft_info(AirCraft_ID),
            Airport_ID INTEGER REFERENCES Airport_info(Airport_ID),
            Wildlife_ID INTEGER REFERENCES Wildlife_info(Wildlife_ID),
            When_time TEXT,
            Reported_Date TEXT,
            When_Phase_of_Flight TEXT,
            Condition_Sky TEXT, 
            Pilot_Warned TEXT,
            Wildlife_Number_Strike INTEGER,
            Feet_Above_Ground TEXT,
            Speed_Knots TEXT,
            Condition_Percipitation TEXT,
            Cost_Repair TEXT,
            Cost_Aircraft_no_service TEXT,
            Effect_Flight_impact TEXT,
            Effect_Image TEXT,
            EFFECT_Other TEXT,
            Remains_Wildlife TEXT,
            Location_Nearby TEXT,
            Remarks TEXT)") 

dbSendQuery(conn = db1,  "CREATE TABLE AirCraft_info (AirCraft_ID INTEGER PRIMARY KEY, AirCraft_Model TEXT , Type TEXT, Altiude TEXT, Number_Engines INTEGER)") 

dbSendQuery(conn = db1,  "CREATE TABLE Flight_info (Flight_ID INTEGER PRIMARY KEY, Flight_Number TEXT, Flight_date TEXT,
            Airline TEXT, Airport TEXT)") 

dbSendQuery(conn = db1, "CREATE TABLE Airport_info (Airport_ID INTEGER PRIMARY KEY, Airport_Name TEXT, Location TEXT)")

dbSendQuery(conn = db1, "CREATE TABLE Wildlife_info (Wildlife_ID INTEGER PRIMARY KEY, Species TEXT, Wildlif_size TEXT)")

dbListTables(db1)
dbListFields(db1, "Birdstrike")
#################################################################


#colnames(birdstrike_df)<-dbListFields(db1, "Birdstrike")

#colnames(birdstrike_df)

#class(birdstrike_df[-1,])
##########Puting our data frame into the db1 database tabels. ###############
dbWriteTable(conn=db1,name="BirdStrike",value=birdstrike_df[-1,],append = T)  #first raw is the column names
dbListFields(db1, "Birdstrike")


dbWriteTable(conn=db1,name="AirCraft_info",value=aircraft_df[-1,],append = T)
head(aircraft_df)
dbListFields(db1, "AirCraft_info")

dbWriteTable(conn=db1,name="Flight_info",value=flight_df[-1,],append = T)


dbWriteTable(conn=db1,name="Airport_info",value=airport_df[-1,],append = T)

dbWriteTable(conn=db1,name="Wildlife_info",value=wildlife_df[-1,],append= T)

#dbRemoveTable(conn=db1,name="AirCraft_info")

#####################  So All cleaned normalized tables are imported in the database#########################


####################  Writing Query ######################
download.packages("sqldf")

library(sqldf)
setwd("C:/Users/nabian.m/OneDrive/Summer 2015/Collect,retrieve data/week10(RSQlite)/normalized tables and the databse file")
db1 <- dbConnect(SQLite(), dbname="birdstrike.sqlite")
dbListTables(conn=db1)
dbListFields(db1, "AirCraft_info")

res1<-dbSendQuery(conn = db1,"SELECT RecordID FROM BirdStrike")

query<-(fetch(res1,n=100000))
query
#dbClearResult(res1)

res2<-dbSendQuery(conn = db1,"SELECT Number_Engines FROM AirCraft_info ")
query<-(fetch(res2,n=100000))
query


res3<-dbSendQuery(conn = db1,"SELECT Flight_Number FROM Flight_info ")
query<-(fetch(res3,n=100000))
query

res4<-dbSendQuery(conn = db1,"SELECT Location FROM Airport_info")
query<-(fetch(res4,n=100000))
query

res5<-dbSendQuery(conn = db1,"SELECT Species FROM Wildlife_info")
query<-(fetch(res5,n=100000))
query












