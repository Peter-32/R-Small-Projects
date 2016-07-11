#Setup
# first run: install.packages("ggplot2")
#            install.packages("gridExtra")
library("ggplot2")
library("gridExtra")

# Load Global Temperatures file
file <- read.table("GlobalLandTemperatures/GlobalTemperatures.csv",header=TRUE,sep=",")
rows <- dim(file)[1]

# Remove NAs
globalTemperatures <- file[34:rows,1:3]
rows <- dim(globalTemperatures)[1]

# Update names, add column(s), change data types
names(globalTemperatures) <- c("date","land_avg_temp","land_avg_temp_uncertainty")
globalTemperatures$date <- as.Date(globalTemperatures$date, "%Y-%m-%d")
globalTemperatures$month <- months(globalTemperatures$date)
attach(globalTemperatures)
#Loging Test: sapply(globalTemperatures,class)

# Exploratory Analysis
#How does the variance change over time?
qplot(date, land_avg_temp_uncertainty, data=globalTemperatures)
#Result: the variance is low after 1850
#Why this matters: will keep in mind there is more variance
#                  before 1850
#                  Only want to apply a fit so heteroskedasticity doesn't matter


#first look at the average temperature data
qplot(date, land_avg_temp, data=globalTemperatures)
#Result: It looks like seasonality has a large part in temperature

#Adding coloring for each month
qplot(date, land_avg_temp, data=globalTemperatures,color=month)
#Result: It will be best to use time series on a single month each year
#Result: Will choose June and December to work with more

#Subsetting and plotting the data for June and December
juneGlobalTemperatures = globalTemperatures[month=="June", 1:2]
decemberGlobalTemperatures = globalTemperatures[month=="December", 1:2]
qplot(date, land_avg_temp, data=juneGlobalTemperatures)
qplot(date, land_avg_temp, data=decemberGlobalTemperatures)
#Result: This looks ready to have a time series fit.

#Applying time series
#Applying y = mx + b line to the qplots in order to see the slope over time
#of temperature change for June and December
#JUNE
g <- ggplot(juneGlobalTemperatures, aes(date,land_avg_temp))
g <- g + labs(title="June Global Average Land Temperature 1753 to 2015") +
  labs(x = "Date") + labs(y = "Average Land Temperature")
g <- g + geom_smooth(size=1, linetype=1,method = "lm", se=FALSE)
g <- g + theme_bw(base_family ="Times")
g <- g + geom_line()
#DECEMBER
g2 <- ggplot(decemberGlobalTemperatures, aes(date,land_avg_temp))
g2 <- g2 + labs(title="December Global Average Land Temperature 1752 to 2015") +
  labs(x = "Date") + labs(y = "Average Land Temperature")
g2 <- g2 + geom_smooth(size=1, linetype=1,method = "lm", se=FALSE)
g2 <- g2 + theme_bw(base_family ="Times")
g2 <- g2 + geom_line()
#UNCERTAINTY
gUncertainty <- ggplot(globalTemperatures, aes(date,land_avg_temp_uncertainty)) + geom_point
gUncertainty <- gUncertainty + labs(title="Uncertainty of Measurements 1752 to 2015") +
  labs(x = "Date") + labs(y = "Average Land Temperature Uncertainty")
gUncertainty <- gUncertainty + geom_smooth(size=1, linetype=1,method = "lm", se=FALSE)
gUncertainty <- gUncertainty + theme_bw(base_family ="Times")
#ALL MONTHS TOGETHER
gAllMonths <- ggplot(globalTemperatures, aes(date,land_avg_temp)) + geom_point(color=month)
gAllMonths <- gAllMonths + labs(title="Uncertainty of Measurements 1752 to 2015") +
  labs(x = "Date") + labs(y = "Average Land Temperature")
gAllMonths <- gAllMonths + geom_smooth(size=1, linetype=1,method = "lm", se=FALSE)
gAllMonths <- gAllMonths + theme_bw(base_family ="Times")

grid.arrange(g,g2,gUncertainty,gAllMonths,ncol=2)
