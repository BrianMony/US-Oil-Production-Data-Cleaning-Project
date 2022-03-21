# Loading the data into R

produnclean <- read.csv("C:\\Users\\User\\Desktop\\attachment_2 (4).csv", header= F, strip.white=T, skip =3)

#Inspecting the Loaded Data

head(produnclean)

str(produnclean)

names(produnclean)

#Beginning the Cleaning Process by eliminating unnecessary rows and columns

library(dplyr)

library(tidyr)

library(stringr)

library (tidyverse)

library (lubridate)

attach(produnclean)

#Generating Week One's full dates

produnclean <- produnclean %>% separate(V1, into =c('Year', 'Month'), sep='-') %>% subset(select=-Month)

head (produnclean)

Week1Date<- paste(produnclean$Year, "/", produnclean$V2)

produnclean <- produnclean %>% mutate(Week1Date)%>% subset(select=-V2)

head(produnclean)

#Generating Week two's full dates

Week2Date<- paste(produnclean$Year, "/", produnclean$V4)

produnclean <- produnclean %>% mutate(Week2Date)%>% subset(select=-V4)

head(produnclean)

#Generating Week three's full dates

Week3Date<- paste(produnclean$Year, "/", produnclean$V6)

produnclean <- produnclean %>% mutate(Week3Date)%>% subset(select=-V6)

#Generating week four's full dates

Week4Date<- paste(produnclean$Year, "/", produnclean$V8)

produnclean <- produnclean %>% mutate(Week4Date)%>% subset(select=-V8)

#Generating week five's full dates

Week5Date<- paste(produnclean$Year, "/", produnclean$V10)

produnclean <- produnclean %>% mutate(Week5Date)%>% subset(select=-V10)%>% subset(select=-Year)

head(produnclean)

#Matching week dates to their production values

produnclean <- produnclean %>% unite('Allweek1prod',Week1Date,V3, sep='_') %>% unite('Allweek2prod',Week2Date,V5, sep='_') %>% unite('Allweek3prod',Week3Date,V7, sep='_') %>% unite('Allweek4prod',Week4Date,V9, sep='_') %>% unite('Allweek5prod',Week5Date,V11, sep='_')

head(produnclean)

#Sub-setting the various production weeks into different data frames for easier analysis

##For week 1

produncleanWk1 <- produnclean %>% select(Allweek1prod)

produncleanWk1 <- produncleanWk1 %>% separate(Allweek1prod, into =c('Date', 'Production'), sep ='_')

head(produncleanWk1)

##For Week 2

produncleanWk2 <- produnclean %>% select(Allweek2prod)

produncleanWk2 <- produncleanWk2 %>% separate(Allweek2prod, into =c('Date', 'Production'), sep ='_')

##For Week 3

produncleanWk3 <- produnclean %>% select(Allweek3prod)

produncleanWk3 <- produncleanWk3 %>% separate(Allweek3prod, into =c('Date', 'Production'), sep ='_')

##For Week 4

produncleanWk4 <- produnclean %>% select(Allweek4prod)

produncleanWk4 <- produncleanWk4 %>% separate(Allweek4prod, into =c('Date', 'Production'), sep ='_')

##For Week 5

produncleanWk5 <- produnclean %>% select(Allweek5prod)

produncleanWk5 <- produncleanWk5 %>% separate(Allweek5prod, into =c('Date', 'Production'), sep ='_')
#Merging the 5 weeks' data frames into one

cleanprodlist <- list(produncleanWk1, produncleanWk2, produncleanWk3,produncleanWk4,produncleanWk5)

cleanprod <- cleanprodlist %>% reduce(full_join, by='Date')

is.data.frame(cleanprod)

cleanprod <- cleanprod %>% subset(select= c(Date,Production.x))

cleanprod <- cleanprod %>% rename(Production=Production.x)

head(cleanprod)

#Removing missing values

cleanprod <- cleanprod %>% na.exclude(cleanprod)

sum(is.na(cleanprod))

str(cleanprod)

#Removing the dates that lack production (resulting from week 5 data)

cleanprod <- cleanprod %>% subset(Date!="1983 / " & Date!="1984 / " & Date!="1985 / " & Date!="1986 / " & Date!="1987 / " & Date!="1988 / " & Date!="1989 / " & Date!="1990 / " & Date!="1991 / " & Date!="1992 / " & Date!="1993 / " & Date!="1994 / " & Date!="1995 / " & Date!="1996 / " & Date!="1997 / " & Date!="1998 / " & Date!="1999 / " & Date!="2000 / " & Date!="2001 / " & Date!="2002 / " & Date!="2003 / " & Date!="2004 / " & Date!="2005 / " & Date!="2006 / " & Date!="2007 / " & Date!="2008 / " & Date!="2009 / " & Date!="2010 / " & Date!="2011 / " & Date!="2012 / " & Date!="2013 / " & Date!="2014 / " & Date!="2015 / " & Date!="2016 / " & Date!="2017 / " & Date!="2018 / " & Date!="2019 / " & Date!="2020 / " & Date!="2021 / ")

head(cleanprod)

#Unifying date format

cleanprod <- cleanprod %>% mutate (Date = gsub ('\\ /', "/", Date))

cleanprod <- cleanprod %>% mutate (Date = gsub (" ", "", Date))

cleanprod <- cleanprod %>% mutate (Production = gsub (",", "", Production))

head(cleanprod)

#Making the date variable an POSIXt and production into numeric

attach(cleanprod)

cleanprod <- cleanprod %>% mutate (Date = as.POSIXct(Date, format = "%Y/%m/%d"))

cleanprod <-cleanprod %>% mutate (Production = as.numeric(Production))

str(cleanprod)

head(cleanprod)

sum(is.na(cleanprod))

#Creating time series plot for weekly Oil Production

library(ggplot2)

library(plotly)

wkplt <- ggplot(cleanprod, aes(x = Date, y = Production)) +
  geom_line(col="blue", alpha=0.5, lwd=1) +
  labs(title = 'Weekly U.S. Field Production of Crude Oil', subtitle='Thousand Barrels Per Day', x='Week Date', y='Amount Produced')

ggplotly(wkplt)