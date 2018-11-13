library('ggplot2')
library('forecast')
library('tseries')
library('tidyverse')

setwd("C:/xxx")

### TOTAL VISITS TO MUSEUMS AND GALLERIES IN ENGLAND
## ARIMA

Data <- read.csv("Monthly_museums_and_galleries_July_2018.csv", stringsAsFactors = F)
Data$date <- as.Date(Data$date, format = "%d/%m/%Y")

DataT <-Data[-grep("TOTAL",Data$museum),]

DataT$visits <- gsub(",", "", DataT$visits)
DataT <- DataT[, c("date","visits")]
DataT$visits <- as.numeric(DataT$visits)
DataT[is.na(DataT)] <- 0
DataTOTAL <- aggregate(visits ~ date, DataT, sum)

DataTTS <- ts(DataTOTAL$visits, frequency = 12, start = c(2004,04), end = c(2018,07)) 

ggplot() +
  geom_line(data = DataTOTAL, aes(x = date, y = visits)) + ylab('Total visits') + xlab('Date') +
  ggtitle('Total visits to museums and galleries in England')

AutoArima <- auto.arima(DataTTS, seasonal=T)
Summary <- summary(AutoArima)
AutoArimaF <- forecast(AutoArima, h=12)
plot(AutoArimaF)
summary(AutoArimaF)


## HOLT AND WINTERS METHOD
## This method is used when data shows trend and seasonality

fcHW1 <- hw(DataTTS,seasonal="additive", h=12)
forecast (fcHW1, PI = TRUE)

summary(fcHW1)
fcHW2 <- hw(DataTTS,seasonal="multiplicative", h=12)
summary(fcHW2)
autoplot(DataTTS) +
  autolayer(fcHW1, series="HW additive forecasts", PI=FALSE) +
  autolayer(fcHW2, series="HW multiplicative forecasts",
            PI=FALSE) +
  xlab("Year") +
  ylab("Total Visitors (millions)") +
  ggtitle("Forecasting total visits using the Holt-Winters method") +
  guides(colour=guide_legend(title="Forecast"))


----------------------------------------------------------------------------------------------
### WEBSITE VISITS TO MUSEUMS AND GALLERIES IN ENGLAND
## AUTO ARIMA
  
setwd("C:/xxx")

DataWeb <- read.csv("Website visits split by museum Annually.csv", stringsAsFactors = F)
DataWeb$date <- as.Date(DataWeb$date, format = "%d/%m/%Y")
DataWeb$Total <- gsub(",", "", DataWeb$Total)
DataWebT <- DataWeb[, c("date","Total")]
DataWebT$Total <- as.numeric(DataWebT$Total)
#DataWeb[is.na(DataWeb)] <- 0
DataWebT[is.na(DataWebT)] <- 0

DataWebTTS <- ts(DataWebT$Total, frequency = 1, start = 2009, end = 2018)

ggplot() +
  geom_line(data = DataWebT, aes(x = date, y = Total)) + ylab('Website visits') + xlab('Date') +
  ggtitle('Total website visits to museums and galleries in England')

AutoArima <- auto.arima(DataWebTTS, seasonal=T)
Summary <- summary(AutoArima)
AutoArimaF <- forecast(AutoArima, h=2)
plot(AutoArimaF)


## SIMPLE EXPONENTIAL SMOOTHING (SES)
##This method is suitable for forecasting data with no clear trend or seasonal pattern

fcWeb <- ses(DataWebTTS, h=2)

round(accuracy(fcWeb),2)

autoplot(fcWeb) +
  autolayer(fitted(fcWeb), series="Fitted") +
  ylab("Website visits") + xlab("Year")
summary(fcWeb)


## HOLT'S LINEAR TREND METHOD
## This method is used when data shows a trend

fcWebH <- holt(DataWebTTS, h=2)
autoplot(fcWebH) +
  autolayer(fitted(fcWebH), series="Fitted") +
  ylab("Educational visits") + xlab("Year")
summary(fcWebH)

----------------------------------------------------------------------------------------------
### EDUCATIONAL VISITS TO MUSEUMS AND GALLERIES IN ENGLAND
## AUTO ARIMA

setwd("C:/xxx")

DataEdu <- read.csv("Educational visits Annually.csv", stringsAsFactors = F)
DataEdu$date <- as.Date(DataEdu$date, format = "%d/%m/%Y")
DataEdu$Total <- gsub(",", "", DataEdu$Total)
DataEduT <- DataEdu[, c("date","Total")]
DataEduT$Total <- as.numeric(DataEduT$Total)
DataEduT[is.na(DataEduT)] <- 0

ggplot() +
  geom_line(data = DataEduT, aes(x = date, y = Total)) + ylab('Educational visits') + xlab('Date') +
  ggtitle('Total educational visits to museums and galleries in England')

DataEduTTS <- ts(DataEduT$Total, frequency = 1, start = 2013, end = 2018) 

AutoArima <- auto.arima(DataEduTTS, seasonal=F)
Summary <- summary(AutoArima)
AutoArimaF <- forecast(AutoArima, h=1)
plot(AutoArimaF)


## HOLT'S LINEAR TREND METHOD

fcEduH <- holt(DataEduTTS, h=2)

autoplot(fcEduH) +
  autolayer(fitted(fcEduH), series="Fitted") +
  ylab("Educational visits") + xlab("Year")
summary(fcEduH)


## SIMPLE EXPONENTIAL SMOOTHING (SES)

fcEdu <- ses(DataEduTTS, h=2, beta = NULL, gamma = NULL)

round(accuracy(fcEdu),2)

autoplot(fcEdu) +
  autolayer(fitted(fcEdu), series="Fitted") +
  ylab("Education visits") + xlab("Year")
summary(fcEdu)
