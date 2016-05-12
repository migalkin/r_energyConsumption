## Define paths and print them to ensure they are correct
path <- "/Users/mikhailgalkin/git/R_energyConsumption/"
print("The path is")
print(path)
path1 <- paste(path,"csv-1.csv",sep="")
path2 <- paste(path,"csv-2-fixed.csv",sep="")
path3 <- paste(path,"csv-3-fixed.csv",sep="")
temperature_path <- paste(path,"temperature.txt",sep="")
print(path1)
print(path2)
print(path3)

## Load data into a data frame
table1 <- read.csv(path3, header = TRUE, sep = ';', stringsAsFactors = FALSE)
#print(table1[1:1,ncol(table1)])
#print(table1[1,4])
#print(nrow(table1))

## Data cleaning - replace all NAs and outliers with 0
for (i in 1:nrow(table1)){
    if (is.na(table1[i,4]) || (table1[i,4]<0) || (table1[i,4]>100000000)) {
        table1[i,4] <- 0 
    }
    #print(table1[i,4])
}

#Extract only :00 dates - every 12th row of the first column
dates <- table1[seq(1,nrow(table1),12),1]
#print(dates)
## Accumulate in the vector the sum of 12 rows of Watt consumption
k <- 0
s <- 0
sum <- c()
for (i in 1:nrow(table1)){
        #count sum
        s <- s + table1[i,4]
        k <- k+1
        if (k==12){
            #populate a dataframe
            sum <- c(sum,s)
            #print(table1[i,])
            s <- 0
            k <- 0
        }
}
#add the last portion of the data before the round clock
if (k!=0){
    sum <- c(sum,s)
}

## Create an output data frame
sum_df <- data.frame(Date = dates,
                     Wt_consumption = sum,
                     stringsAsFactors=FALSE)

#plot(sum_df$Date[1:50],sum_df$Wt_consumption[1:50])
#write temp results in .csv files
#write.table(sum_df,file="res2.csv",sep=",")

somedata <- "01.10.2015 0:00"

#transform date format to the format in the temperature.txt
#parse1 <- sapply(strsplit(somedata,"\\:"), `[`, 1)
#Given date format in csv:
# DD.MM.YYYY HH:MM
# have to transform it to
# YYYYMMDDHH

# sapply produces an array [DD.MM.YYYY ; HH:MM]
# get DD.MM.YYYY, first element
dayDate <- sapply(strsplit(dates," "), `[`, 1)
# get HH:MM, second element
hourDate <- sapply(strsplit(dates," "), `[`, 2)
# get HH
hoursPartOfDate2 <- sapply(strsplit(hourDate,"\\:"), `[`, 1)
# the function to add 0 to hours<10, e.g. 3 -> 03
checkstr <- function(x){
    if (as.integer(x)<10) {
        y <- paste("0",x,sep="")
    } else {y<- x}
    y
}
#reverse a list, so that 09 11 2015 becomes 20151109
reverseList <- function(x){
    y<- paste(x[3],x[2],x[1],sep = "")
    y
}

# use the function to normalize HH format to 03 instead of 3
adjustedHoursDate2 <- sapply(hoursPartOfDate2,checkstr)

#split the string 09.11.2015 to a list [09, 11, 2015]
split_dayDate <- lapply(strsplit(dayDate,"\\."), `[`)

#reverse the string 20151109
reverseDate <- lapply(split_dayDate, reverseList)
#attach an hour to the date
completeDate <- paste(reverseDate,adjustedHoursDate2,sep = "")

#read the temperatures table
temperatures <- read.table(temperature_path,header = TRUE, sep=";")
#use complete_date as index to search for LUFTTEMPERATUR

temperatureAtDate <-c ()
for (i in completeDate){
    temperatureAtDate <- c(temperatureAtDate,temperatures$LUFTTEMPERATUR[temperatures$MESS_DATUM==i])
}
#temperatures$LUFTTEMPERATUR[temperatures$MESS_DATUM==completeDate[3]]
sum_df$Temperature <- temperatureAtDate

write.table(sum_df,file="Wt_temp3.csv",sep=",")
