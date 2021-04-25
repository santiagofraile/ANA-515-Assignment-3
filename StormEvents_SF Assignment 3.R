getwd()
install.packages("tidyverse")

#I will change file name and also leave those relevant columns.
#select columns dataframe to: the beginning and ending dates and times, the episode ID, 
#the event ID, the state name and FIPS, the “CZ” name, type, and FIPS, the event type, 
#the source, and the beginning latitude and longitude and ending latitude and longitude.


StormEvents_SF <- StormEvents_details_ftp_v1_0_d1989_c20170717[c(7:10,13:16,18,20,27,45:48)]

#Now I will change the Beginning and Ending Dates to a "Date-Time" class.


mutate(StormEvents_SF, "BEGIN_DATE_TIME" = dmy_hms(BEGIN_DATE_TIME), "END_DATE_TIME" = dmy_hms(END_DATE_TIME))

#Now I will change Sate and County names to title case

str_to_title(string = StormEvents_SF$STATE)
str_to_title(string = StormEvents_SF$CZ_NAME)


#Limit to the events listed by county FIPS (CZ_TYPE of “C”) and then remove the CZ_TYPE column

filter(StormEvents_SF, StormEvents_SF$CZ_TYPE == "C")
select(StormEvents_SF, -CZ_TYPE)

# Pad the state and county FIPS with a “0” at the beginning 
#(hint: there’s a function in stringr to do this) and then unite 
#the two columns to make one fips column with the 5-digit county FIPS code

str_pad(StormEvents_SF$STATE_FIPS, width = 3, side = "left", pad = "0")
str_pad(StormEvents_SF$CZ_FIPS, width = 3, side = "left", pad = "0")

StormEvents_SF %>%
unite("STATE_CZ_FIPS", STATE_FIPS, CZ_FIPS)

# Change all the column names to lower case

rename_all(StormEvents_SF, tolower)

# There is data that comes with R on U.S. states (data("state")). 
#Use that to create a dataframe with the state name, area, and region

data("state")

us_state_info <- data.frame(state = state.name, region = state.region, area = state.area)

#Create a dataframe with the number of events per state in the year 
#of your birth. Merge in the state information dataframe you just created. 
#Remove any states that are not in the state information dataframe

Statefrequency <- data.frame(table(StormEvents_SF$STATE))
head(Statefrequency)

merged <- merge(Statefrequency, us_state_info, by.x = "STATE", by.y = "state")

statefrequency1 <- rename(Statefrequency, c("state" = "Var1")) 

merged <- merge(statefrequency1, us_state_info, by.x = "state", by.y = "state")
head(merged)

statefrequency1 <- mutate_all(statefrequency1, toupper)
us_state_info <- mutate_all(us_state_info, toupper)

merged <- merge(statefrequency1, us_state_info, by.x = "state", by.y = "state")
head(merged)

#Create the following plot

library(ggplot2)

Storm_plot <- ggplot(merged, aes(x = area, y = Freq)) + geom_point(aes(color = region)) + labs(x = "Land area (square miles)", y = "# of storm events in 1989")
Storm_plot

