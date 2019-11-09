#Running through an ARMA model using Sonoma County Wine data 
#It  is of cases sales and total revenue generated over a period 
#of about six years – specifically from December 18th, 2004 to April 2nd, 2011.

#The goal is to examine the relationship between total cases sold and various other 
#factors including seasonal variations, real income, and population changes. 
#The income and population metrics were obtained from Federal Reserve Economic Data. 
#The real income variable was generated using personal consumption expenditures related 
#directly to food, and a consumer price index with a base year of 2015.

wine <- read.csv('wine.csv')
head(wine)

attach(wine)

head(date)
#date is in the wrong format "15jan2005", reformatting to make it easier to use
newdate <- levels(date)
newdate <- as.Date(date, format = "%d%b%Y")
head(newdate)
month <- month(newdate)
year <- year(newdate)
#attaching newdate to date and replacing
wine$date <- newdate

wine_demand_plot <- ggplot(wine, aes(newdate, cases)) + geom_point() +
  theme_bw() +
  scale_y_continuous(name = "Total Cases Sold",
                     breaks = seq(20000, 70000, 10000),
                    limits=c(20000, 70000)) +
  labs(x = 'Date')
wine_demand_plot
#the firm is steadily increasing revenue and case sales over the 
#course of the ~6 years.

##same graph with a line
wine_demand_line <- ggplot(wine, aes(newdate, cases)) + geom_line() +
  theme_bw() +
  scale_y_continuous(name = "Total Cases Sold",
                     breaks = seq(20000, 70000, 10000),
                     limits=c(20000, 70000)) +
  labs(x = 'Date')
wine_demand_line 
#the firm is steadily increasing revenue and case sales over the 
#course of the ~6 years.

#Inlcude additional variables from FRED (Federal Reserve Economic Data)
#Real income, populatin, and CPI
#This can be done a number of ways. The easiest is using the package 'fredr'

library(fredr)
fredr_set_key("XXXXXX") ##you need a personal FRED API key

##FOR CPI###################################################################
cpi <- fredr(
  series_id = "CPALTT01USM661S",
  observation_start = as.Date("2004-12-01"),
)
head(cpi)
##drop unnessesary column, 'series_id'
cpi$series_id <- NULL
#adding month and year
cpi$month <- month
cpi$year <- year
colnames(cpi)[colnames(cpi)=="value"] <- "cpi"


##FOR INCOME#################################################################
income <- fredr(
  series_id = "DFXARC1M027SBEA",
  observation_start = as.Date("2004-12-01"),
)
head(income)
income$series_id <- NULL
income <- income[-c(178), ] #deleting unnecessesary row
income$month <- month
income$year <- year
colnames(income)[colnames(income)=="value"] <- "income"

##FOR POPULATION#############################################################

pop <- fredr(
  series_id = "POPTHM",
  observation_start = as.Date("2004-12-01"),
)
head(pop)
pop$series_id <- NULL
pop <- pop[-c(178), ] #deleting unnecessesary row
pop$month <- month
pop$year <- year
colnames(pop)[colnames(pop)=="value"] <- "pop"

##Merging and cleaning cpi and income
cpi_inc <- merge(cpi, income, by = 'date')
colnames(cpi_inc)[colnames(cpi_inc)=="month.y"] <- "month"
colnames(cpi_inc)[colnames(cpi_inc)=="year.y"] <- "year"
cpi_inc <- cpi_inc[ -c(3) ] #hit it twice

##Merging and cleaning cpi, income, and population
cpi_inc_pop <- merge(cpi_inc, pop, by = 'date')
colnames(cpi_inc_pop)[colnames(cpi_inc_pop)=="value"] <- "pop"
colnames(cpi_inc)[colnames(cpi_inc)=="month.y"] <- "month"
colnames(cpi_inc)[colnames(cpi_inc)=="year.y"] <- "year"
cpi_inc_pop <- cpi_inc_pop[ -c(3,4,6,7) ] #hit it twice

##Removing excess data
cpi_inc_pop <- cpi_inc_pop[-c(84:177), ] #deleting unnecessesary row

#merging into a new dataset
winef <- cbind(cpi_inc_pop, wine)
winef <- winef[ -c(1,5,6) ] #deleting excess date columns

#saving data for future use and analysis
write.csv(winef,'winef.csv')

#GRAPHING################################################################
#cpi graph
cpi_line <- ggplot(winef, aes(date, cpi)) + geom_line() +
  theme_bw() +
  scale_y_continuous(name = "Consumer Price Index",
                     breaks = seq(80, 100, 10),
                     limits=c(80, 100)) +
  labs(x = 'Date')
cpi_line

#income graph
inc_line <- ggplot(winef, aes(date, income)) + geom_line() +
  theme_bw() +
  scale_y_continuous(name = "Income",
                     breaks = seq(650, 850, 50),
                     limits=c(650, 850)) +
  labs(x = 'Date')
inc_line

#population graph
pop_line <- ggplot(winef, aes(date, pop)) + geom_line() +
  theme_bw() +
  scale_y_continuous(name = "Income",
                     breaks = seq(290000, 315000, 5000),
                     limits=c(290000, 315000)) +
  labs(x = 'Date')
pop_line

##CREATING REAL INCOME#####################################################
winef <- transform(winef, r_income = ((income / cpi)*100))

rinc_line <- ggplot(winef, aes(date, r_income)) + geom_line() +
  theme_bw() +
  labs(x = 'Date', y = 'Real Income')
rinc_line

dummy <- dummyCreator(winef$month, prefix = "month")
winef <- cbind(winef, dummy)

#running a simple multiple regression
wine_model1 = lm(formula = cases ~ month_2 + month_3 + month_4 + month_5 + month_6
         + month_7 + month_8 + month_9 + month_10 + month_11 + month_12 + r_income + pop, data = winef)

outreg(wine_model1)

#This is still a work in progress. Last updated 11/7/2019









