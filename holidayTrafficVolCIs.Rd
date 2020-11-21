###ISYE 3770 Final Project###
#Author: Thomas Talbot 
#Last Date Modified: 11/21/20
#Description: This script calculates 2-sided 95% confidence intervals on for the mean traffic volume on holidays. 
 
# first save the path to the WD

projectWD<- "/Users/thomastalbot/Desktop/ISYE3770/CalculatingCI"

#Then set the WD
setwd(projectWD)

#STEP 1: READ ALL TRAFFIC DATA INTO A DATA FRAME

trafficData <- read.csv(file= 'Metro_Interstate_Traffic_Volume.csv', header = T)

holidayNames<-c("Christmas Day", "Columbus Day", "Independence Day", "Labor Day", "Martin Luther King Jr Day","Memorial Day", "New Years Day", "State Fair", "Thanksgiving Day", "Veterans Day", "Washingtons Birthday")

holidayTrafficVol<-data.frame("Christmas Day" =c(), "Columbus Day"=c(), "Independence Day" =c(), "Labor Day"=c(), "Martin Luther King Jr Day"=c(),"Memorial Day"=c(), "New Years Day" = c(), "State Fair"=c(), "Thanksgiving Day" = c(), "Veterans Day" = c(), "Washingtons Birthday" = c())



#STEP 2: EXTRACT THE STARTING ROWS OF EACH HOLIDAY 

startHolidayData<-data.frame()
for(holidayName in holidayNames)
{
	tempDataFrame<-trafficData[which(trafficData$holiday == holidayName),]
	startHolidayData<-rbind(startHolidayData, tempDataFrame)
	
}


#Start holiday data contains the starting entry for each holiday 
#Now go through the rows of holiday data and extract the traffic_volume 



#filter out duplicate rows and sort by the holiday 
keeps<-c("holiday", "date_time", "traffic_volume")

startHolidayData<-startHolidayData[keeps]
startHolidayData<- dplyr::distinct(startHolidayData)
startHolidayData<-startHolidayData[order(startHolidayData$holiday),] # order by the holiday name 

#STEP 3: TRANSFORM THE STARTING DATES OF EACH HOLIDAY INTO DATE-TIME OBJECTS

startingDates <-c() #make an array of the starting dates 

for(i in 1:nrow(startHolidayData))
{
	startingDates<-append(startingDates, as.Date(startHolidayData[i, "date_time"], format='%m/%d/%y') , length(startingDates))
}


#STEP 4: EXTRACT ALL ROWS FROM THE DATA SET THAT OCCUR ON A HOLIDAY ORDERED BY HOLIIDAY

allHolidayData<-c()
for(i in 1:length(startingDates))
{
	tempDf<-trafficData[as.Date(trafficData$date_time, format='%m/%d/%y') %in% startingDates[i],]
	allHolidayData<-rbind(allHolidayData, tempDf)
}

#STEP 5: REMOVE ANY DUPLICATE ENTRIES  

allHolidayData<-allHolidayData[keeps]
allHolidayData<-dplyr::distinct(allHolidayData) # get rid of duplicate entries

#STEP 6: CREATE A DATA FRAME FOR 95% TWO SIDED CONFIDENCE INTERVALS ON MEAN TRAFFIC VOLUME

#the rows of the data frame are the numerical values needed for the confidence interval like: sample size, standared dev, half width, sample mean, 
#and lower and upper bounds of the confidence intervals. The columns of the data frame are the holidays. 

twoSidedCI<-data.frame(matrix(ncol=11, nrow=6))
ciRows<-c("Sample Size", "Standard Dev", "Half Width","Xbar", "Lower", "Upper" )
colnames(twoSidedCI)<-holidayNames #columns are holiday names 
rownames(twoSidedCI)<-ciRows # the rows are the numerical values needed for the CI

#STEP 7: CALCULATE THE 95% CONFIDENCE INTERVAL FOR EACH HOLIDAY
#The calculations include: sample size,  sample mean, sample standard deviation, half width, and lower & upper confidence interval bounds. 

tempTrafficVol<-c() #a vector of traffic volume for a single holiday 
currHoliday<-"" #a string indicating the current holiday 
for(i in 1:nrow(allHolidayData))
{
	if(!identical(currHoliday, allHolidayData[i, "holiday"]) && !identical("None", allHolidayData[i, "holiday"]))
	{
		#then we are extracting data from a new holiday 
		if(!identical(currHoliday, ""))
		{
			# Before resetting currHoliday make calcualtions on the sample
			
			n = length(tempTrafficVol) # get the sample size
			s<- sd(tempTrafficVol) #get sample standard dev
			xbar<-mean(tempTrafficVol) #get the sample mean 
			hw<- qt(0.975, df=n-1)*(s/sqrt(n)) #get the half width using a t-distribution 
			lower<- xbar-hw #the lower bound on the CI 
			upper<-xbar+hw #the upper bound on the CI 
	
			#Save the confidence interval calculations into the twoSidedCI data frame 
	
			twoSidedCI["Sample Size", currHoliday] <-n
			twoSidedCI["Standard Dev", currHoliday]<-s
			twoSidedCI["Half Width", currHoliday]<-hw
			twoSidedCI["Xbar", currHoliday]<-xbar	
			twoSidedCI["Lower", currHoliday]<-lower
			twoSidedCI["Upper", currHoliday]<-upper			

			tempTrafficVol<-c() #reset the vector of traffic volume 
			
		}
		
		currHoliday = allHolidayData[i,"holiday"] #update the current holiday 
		
		
	}# endif
	
	tempTrafficVol<-append(tempTrafficVol, allHolidayData[i, "traffic_volume"])
	
}

# save the traffic volume data for the last holiday 

n = length(tempTrafficVol) # get the sample size
s<- sd(tempTrafficVol)
xbar<-mean(tempTrafficVol)
hw<- qt(0.975, df=n-1)*(s/sqrt(n))
lower<- xbar-hw
upper<-xbar+hw
	
#Save the confidence interval calculations into the twoSidedCI data frame 
	
twoSidedCI["Sample Size", currHoliday] <-n
twoSidedCI["Standard Dev", currHoliday]<-s
twoSidedCI["Half Width", currHoliday]<-hw
twoSidedCI["Xbar", currHoliday]<-xbar	
twoSidedCI["Lower", currHoliday]<-lower
twoSidedCI["Upper", currHoliday]<-upper	

#STEP 8: WRITE THE CONFIDENCE INTERVALS TO A CSV FILE

write.csv(twoSidedCI, "/Users/thomastalbot/Desktop/ISYE3770/CalculatingCI/twoSidedHolidayCIv2.csv",row.names=TRUE)


				
				

















































