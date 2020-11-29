###ISYE 3770 Final Project ###
#Author: Thomas Talbot 
#Last Date Modified: 11/24/20
#Description: The purpose of this file is to make caclculations to conduct a two sided hypothesis test on the 
#mean traffic volume on holidays versus non-holidays 

# first save the path to the WD

projectWD<- "/Users/thomastalbot/Desktop/ISYE3770/CalculatingCI"

#Then set the WD
setwd(projectWD)

#STEP 1: READ ALL TRAFFIC DATA INTO A DATA FRAME
trafficData <- read.csv(file= 'Metro_Interstate_Traffic_Volume.csv', header = T)

holidayNames<-c("Christmas Day", "Columbus Day", "Independence Day", "Labor Day", "Martin Luther King Jr Day","Memorial Day", "New Years Day", "State Fair", "Thanksgiving Day", "Veterans Day", "Washingtons Birthday")

#STEP 2: EXTRACT THE STARTING ROWS OF EACH HOLIDAY 

onlyHolidayData<-data.frame()
for(holidayName in holidayNames)
{
	tempDataFrame<-trafficData[which(trafficData$holiday == holidayName),]
	onlyHolidayData<-rbind(onlyHolidayData, tempDataFrame)
	
}

#filter out duplicate rows 
keeps<-c("holiday", "date_time", "traffic_volume")
onlyRelevantCols<-onlyHolidayData[keeps]
onlyRelevantCols<- dplyr::distinct(onlyRelevantCols)




#STEP 3: TRANSFORM THE STARTING DATES OF EACH HOLIDAY INTO DATE-TIME OBJECTS 

startingDates <-c() #make an array of the starting dates 
for(i in 1:nrow(onlyRelevantCols))
{
	startingDates<-append(startingDates, as.Date(onlyRelevantCols[i, "date_time"], format='%m/%d/%y') , length(startingDates))
}


#STEP 4: EXTRACT ALL ROWS FROM THE DATA SET THAT OCCUR ON A HOLIDAY 

allHolidayData<-c()
allHolidayData<-trafficData[as.Date(trafficData$date_time, format='%m/%d/%y') %in% startingDates,]


#STEP 5: EXTRACT ALL ROWS FROM THE DATA SET THAT DON'T OCCUR ON A HOLIDAY 

nonHoliday<-trafficData[!(trafficData$date_time %in% allHolidayData$date_time),]


# I now have two data frames one with holiday data "allHolidayData" the other data frame contians "nonHolidayData"

#STEP 6: REMOVE ANY DUPLICATE ENTRIES 

keeps<-c("holiday", "date_time", "traffic_volume")

allHolidayData<-allHolidayData[keeps]
allHolidayData<- dplyr::distinct(allHolidayData)

print(nrow(allHolidayData))

nonHoliday<-nonHoliday[keeps]
nonHoliday<- dplyr::distinct(nonHoliday)

print(nrow(nonHoliday))

#STEP 7: CREATE A DATA FRAME TO HOLD RECORDS FOR THE T TEST
tTestRecords<-data.frame(matrix(ncol=6, nrow=3))
tTestRows<-c("Sample Size", "Sample Mean", "Sample Standard Dev")
tTestCols<-c("Non-Holidays", "Holidays", "DF","Test Stat", "T Critical Value 2-sided", "T Critical Value 1-sided")
rownames(tTestRecords)<-tTestRows
colnames(tTestRecords)<-tTestCols

#STEP 8: FOR EACH POPULATION, CALCULATE MEAN AND STANDARD DEV. 

#population 1 is traffic volume measurements made on non-holidays, and population 2 is traffic volume measurements on holidays


#randSampleNonHol<-sample(nonHoliday$traffic_volume, 1000, replace=FALSE, prob=NULL) # a random #sample of 1000 non holiday traffic volumes
#randSampleHol<-sample(allHolidayData$traffic_volume, 120, replace=FALSE, prob=NULL) # a racndom #sample of 120 holiday traffic volumes 

n1<-length(nonHoliday$traffic_volume)
n2<-length(allHolidayData$traffic_volume)

#Find the sample mean 
x1bar <- mean(nonHoliday$traffic_volume)
x2bar<-mean(allHolidayData$traffic_volume)

#Find the sample standard deviation
s1 <- sd(nonHoliday$traffic_volume)
s2<- sd(allHolidayData$traffic_volume)

#Save the size, mean, and standard dev for each population
tTestRecords["Sample Size",  "Non-Holidays"] <-n1
tTestRecords["Sample Size",  "Holidays"] <-n2
tTestRecords["Sample Mean",  "Non-Holidays"] <-x1bar
tTestRecords["Sample Mean", "Holidays"]<-x2bar
tTestRecords["Sample Standard Dev",  "Non-Holidays"] <-s1
tTestRecords["Sample Standard Dev",  "Holidays"] <-s2


#STEP 8: CALCULATE THE T-Test STATISTIC FOR THE DIFFERENCE IN THE MEANS OF THE TWO POPULATIONS

tnaught <-(x1bar-x2bar)/(sqrt((s1^2/n1)+(s2^2/n2)))

#save into the data frame
tTestRecords[1, "Test Stat"]<-tnaught

#STEP 9: CALCULATE THE DEGREE OF FREEDOM OF THE TEST STATISTIC 

numeratorDf<-((s1^2/n1)+(s2^2/n2))^2
denomenatorDf<-((s1^2/n1)^2/(n1-1))+((s2^2/n2)^2/(n2-1))

df<-numeratorDf/denomenatorDf

#save the degree of freedom into the data frame
tTestRecords[1, "DF"]<-df

#STEP 10: GET THE T CRITICAL VALUE FOR THE 2-SIDED TEST 

tTestRecords[1, "T Critical Value 2-sided"]<-qt(0.975, df)

#STEP 11: GET THE T CRITICAL VALUE FOR THE 1-SIDED TEST

tTestRecords[1, "T Critical Value 1-sided"]<-qt(0.95, df)

#STEP 12: OUTPUT THE T-TEST CALCULATIONS TO CSV FILE

write.csv(tTestRecords, "/Users/thomastalbot/Desktop/ISYE3770/Nov24NewCode/tTestCalc.csv",row.names=TRUE)

