##GaoSiqin_2017310895  L3
##In this part,I will try to use different commands in dolyr

### the foundation of dply r####

library( "nycflights13" )
library ( "tidyverse" )
##Use the data from nycflights13###
flights
View(flights)

##  1. filter()  ## chose the row

mnth.1 <- filter ( flights , month ==1 , day == 1)
mnth.11.12 <- filter ( flights ,month %in% c ( 11 ,12) )

#month == 11 | month ==12 equals month %in% c ( 11 ,12)
 
## 1.1 how many delaying hours >=2
nrow ( filter ( flights , arr_delay >= 120))

## 1.2 Fly to IAN or MIA
nrow ( filter ( flights , dest %in% c( "IAN" , "HOU" ))) 

## 1.3 Search the missing value
nrow ( filter ( flights , is.na (dep_time )))

## 2. between() ##

nrow ( filter ( flights , between ( month , 6, 9 )))

## 3.arrange()## change the order of row

arrange ( flights ,  year, month , day  )

arrange(flights, desc(arr_delay))

arrange ( flights , air_time)[1,]

## 3.select()## Choose the colum
select ( flights , year , month )

selcet ( flights , year : day )

select ( flights , -( year : day ))  

select ( flights , starts_with ("dep"))

select ( flights , ends_with ( "time"))

select ( flights , contains ( "time"))

## 4.mutate() ##Adding a new var

flights_sml <- select ( flights,
                        year:day,
                        ends_with("delay"),
                        distance,
                        air_time)
mutate ( flights_sml , 
         hours = air_time / 60 )


## 5. summarize() and group_by ##
by_day <- group_by ( flights , year , month , day)
summarize ( by_day , delay = mean (dep_delay , na.rm = TRUE))

## 6.rename() ##
myFlights <- group_by ( flights , year , month , day , dest )

myFlights <- rename (myFlights ,destination = dest)

myFlights


## as.***
mtrx <- matrix ( runif(16) , 4 , 4 )
cha_mtrx <- as.vector ( mtrx )



