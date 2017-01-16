cat("\014")    #This clears the Consol
rm(list=ls())    #This removes all the variables previously existed in Global Environment.

install.packages("stringr")
library(stringr)



#setwd("C:/Users/monabiyan/SkyDrive/Summer 2015/Collect,retrieve data/week4")


getwd()
setwd("C:/Users/monabiyan/Documents")
data1<-scan("movies.list", what="character", sep=";")


film<-0;
film_name=0;
film_year<-0;
j=1;
for (i in 1:length(data1))   #length(data1)
{
  if(str_detect(data1[i],'\\{'))
    {
     film[j]<-substr(data1[i],1,(str_locate_all(pattern ='\\{',data1[i])[[1]][1,1]-2))

     j=j+1
     print(j)
    }
}

film<-unique(film)



for (j in 1:length(film))   #length(data1)
{
    film_name[j]<-substr(film[j],1,(str_locate_all(pattern ='\\(',film[j])[[1]][1,1]-2))
    film_year[j]<-substr(film[j],(str_locate_all(pattern ='\\(',film[j])[[1]][1,1]+1),(str_locate_all(pattern ='\\)',film[j])[[1]][1,1]-1))
    print(j)
}


film_df<-data.frame(FilmName=film_name,FilmYear=film_year)



film_df
head(film_df)


