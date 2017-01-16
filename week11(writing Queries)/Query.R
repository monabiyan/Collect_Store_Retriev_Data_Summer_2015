#Revisit the Bird Strike database created in the last assignment. Using SQL SELECT statements,
#write R programs that retrieve data from the database. Specifically, answer the following
#"questions"/queries:



####################  Writing Query ######################

####################  Writing Query ######################
download.packages("sqldf")

library(sqldf)
setwd("C:/Users/nabian.m/Documents")
getwd()
db1 <- dbConnect(SQLite(), dbname="birdstrike.sqlite")
dbListTables(conn=db1)
dbListFields(db1, "Flight_info")
dbListFields(conn=db1,"BirdStrike")
dbListFields(conn=db1,"Airline_info")



dbGetQuery(conn = db1,"SELECT RecordID FROM BirdStrike")

#############################
#Q1)
#1. Which bird strikes occurred for American Airlines? This is a single number.

Americ_Airline_BirdStrike<-dbGetQuery(conn = db1,"SELECT BirdStrike.RecordID FROM BirdStrike INNER JOIN Flight_info ON BirdStrike.Flight_ID=Flight_info.Flight_ID
           WHERE Flight_info.Airline='AMERICAN AIRLINES'")

length(Americ_Airline_BirdStrike[,1])

#Q2)
#2. How many bird strikes were there for each airline? Show the airline name, including #UNKNOWN, and the number of strikes

Distinct_Airline_strike<-dbGetQuery(conn = db1,"SELECT Flight_info.Airline, count(Flight_info.Airline) FROM Flight_info GROUP BY Flight_info.Airline")
Distinct_Airline_strike



#Q3)
#3. Which airline had the most bird strikes, excluding military and unknown?
Distinct_Airline_strike<-dbGetQuery(conn = db1,"SELECT Flight_info.Airline, count(Flight_info.Airline) FROM Flight_info GROUP BY Flight_info.Airline")

Distinct_Airline_strike[(Distinct_Airline_strike[,2]==max(Distinct_Airline_strike[!((Distinct_Airline_strike[,1]=="UNKNOWN")|(Distinct_Airline_strike[,1]=="MILITARY")),2])),1]


#Q4)

#4. Which bird strikes occurred for Helicopters? List all the bird strike incidents with date.
dbListFields(conn = db1,"AirCraft_info")
dbListFields(conn = db1,"Birdstrike")

Helicopter<-dbGetQuery(conn=db1,"SELECT BirdStrike.RecordID,Birdstrike.Reported_Date FROM BirdStrike INNER JOIN AirCraft_info ON BirdStrike.Aircraft_Model=AirCraft_info.Aircraft_Model 
                       WHERE AirCraft_info.Type='Helicopter'")


Helicopter                       
                       
#Q5)
#5. Which airlines had more than 10 bird strikes? List the airline names only excluding military and unknown.




Distinct_Airline_strike<-dbGetQuery(conn = db1,"SELECT Flight_info.Airline, count(Flight_info.Airline) FROM Flight_info GROUP BY Flight_info.Airline")

Distinct_Airline_strike[(Distinct_Airline_strike[,2]>10)&(!(Distinct_Airline_strike[,1]=="UNKNOWN"))&(!(Distinct_Airline_strike[,1]=="MILITARY")),1]


