#Mohsen Nabian: A report on how to work with "sqlite".

#SQLite is an open source, embedded relational database.
#"Embedded" means that the database engine has been designed to coexist inside other applications. 
#This means that there is no negotiating access across networks, nor any administrative setting up (usernames, passwords, dns, etc).
#It means that the database engine is installed along with the application. 
#When the RSQLite package (the package that lets R play with SQLite) is installed, SQLite comes with it. 
#There is no need for separate installation of an SQLite server.

#Each of two Packages 'RSQLite' and 'sqldf' do the work.
#sqldf makes use of RSQLite, and so installing sqldf will also install 
#all the necessary packages.

install.packages("sqldf")


library(sqldf)  # Both RSQLite and sqldf (and others too) are loaded by the following command.


#1)Create a database:

setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week10/example")

db <- dbConnect(SQLite(), dbname="Test.sqlite")

#2)Adding data to the database - the hard way

dbSendQuery(conn = db,"CREATE TABLE School(SchID INTEGER, Location TEXT,Authority TEXT,SchSize TEXT)")

h<-data.frame(A=c(1,2,3),B=c("T","T","T"),C=c("G","G","G"),D=c("big","big","big"))
dbWriteTable(conn=db,name="School",value=h) 
dbSendQuery(conn = db,"INSERT INTO School VALUES h")


dbSendQuery(conn = db,"INSERT INTO School VALUES (1, 'urban', 'state', 'medium')")
dbSendQuery(conn = db,"INSERT INTO School VALUES (2, 'urban', 'independent', 'large')")
dbSendQuery(conn = db,"INSERT INTO School VALUES (3, 'rural', 'state', 'small')")


dbListTables(db)              # The tables in the database
dbListFields(db, "School")    # The columns in a table
dbReadTable(db, "School")     # The data in a table
dbRemoveTable(db, "School")     # Remove the School table.

#3)Entering data from a data frame to a database Table

my.data = data.frame(X = c("US", "UK", "Canada", "Australia","Newzealand"), Y = c(52, 36, 74, 10, 98)) 
dbWriteTable(conn=db,name="sometablename",value=my.data) 
dbListTables(db) 

#############################################
#3_a) How to read a Table in a Database and put it as a data frame? 
H<-dbReadTable(db, "sometablename")

#############################################
#4)Entering data from csv files

dbWriteTable(conn = db, name = "Student", value = "student.csv",row.names = FALSE, header = TRUE)
dbWriteTable(conn = db, name = "Class", value = "class.csv",row.names = FALSE, header = TRUE)
dbWriteTable(conn = db, name = "School", value = "school.csv",row.names = FALSE, header = TRUE)
dbListTables(db)                   # The tables in the database
dbListFields(db, "School")         # The columns in a table
dbReadTable(db, "School")          # Shows The data in the table 



#5)Creating Tables with primary key and foreign keys


   #First Remove Previous 'Student' and 'School' Tables:
dbRemoveTable(db, "School")     # Remove the School table.
dbRemoveTable(db, "Student")     # Remove the School table.

   # Creating Tables and specifying its foreign keys
dbSendQuery(conn = db,  "CREATE TABLE School (SchID INTEGER PRIMARY KEY,Location TEXT, Authority TEXT, SchSize TEXT)")
dbSendQuery(conn = db,  "CREATE TABLE Student (StudentID INTEGER PRIMARY KEY,Name TEXT, GPA INTEGER, SchoolID INTEGER, foreign key (SchoolID) references School(SchID))")  #You need to do something like this.

    # Again Puting a few data in School table
dbSendQuery(conn = db, "INSERT INTO School VALUES (1, 'urban', 'state', 'medium')")
dbSendQuery(conn = db,"INSERT INTO School VALUES (2, 'urban', 'independent', 'large')")
dbSendQuery(conn = db,"INSERT INTO School VALUES (3, 'rural', 'state', 'small')")


    # Turn on Foreign Keys 
dbSendQuery(conn = db, "pragma foreign_keys=on;") # Very important to turn on the foreign keys in sqlite
  
    
dbSendQuery(conn = db,"INSERT INTO Student VALUES (211, 'Yatish', 4, 1)")
dbSendQuery(conn = db,"INSERT INTO Student VALUES (221, 'XYZ', 4, 4)")  # I am adding here school id 4 which doesnt exist in school table to check the relationship




