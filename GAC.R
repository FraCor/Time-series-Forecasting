library('ggplot2')
library('forecast')
library('tseries')
library('tidyverse')

setwd("C:/xxx")

DataTourGAC <- read.csv("Tours and Visitors.csv", stringsAsFactors = F)
DataTourGAC$date <- as.Date(DataTourGAC$date, format = "%d/%m/%Y")

### GAC TOURS
DataTourGACT <- DataTourGAC[, c("date","Tours")]
DataTourGACT$Tours <- as.numeric(DataTourGACT$Tours)

DataTourGACTTS <- ts(DataTourGACT$Tours, frequency = 12, start = c(2018,04), end = c(2018,08))

ggplot() +
  geom_line(data = DataTourGACT, aes(x = date, y = Tours)) + ylab('GAC tours') + xlab('Date') +
  ggtitle('Public tours to the GAC')

## SIMPLE EXPONENTIAL SMOOTHING (SES)
fcTour <- ses(DataTourGACTTS, h=2)
round(accuracy(fcTour),2)
autoplot(fcTour) +
 autolayer(fitted(fcTour), series="Fitted") +
 ylab("GAC tours") + xlab("Date")
summary(fcTour)

## HOLT'S LINEAR TREND METHOD
fcTourH <- holt(DataTourGACTTS, h=2)
autoplot(fcTourH) +
autolayer(fitted(fcTourH), series="Fitted") +
ylab("GAC tours") + xlab("Date")
summary(fcTourH)

### GAC VISITORS
  
DataTourGAC <- read.csv("Tours and Visitors.csv", stringsAsFactors = F)
DataTourGAC$date <- as.Date(DataTourGAC$date, format = "%d/%m/%Y")
DataVisitorGACT <- DataTourGAC[, c("date","Visitors")]
DataVisitorGACT$Visitors <- as.numeric(DataVisitorGACT$Visitors)

DataVisitorGACTTS <- ts(DataVisitorGACT$Visitors, frequency = 12, start = c(2018,04), end = c(2018,08))

ggplot() +
  geom_line(data = DataVisitorGACT, aes(x = date, y = Visitors)) + ylab('GAC Visitors')  + xlab('Date') +
  ggtitle('Visitors to the GAC')


## SIMPLE EXPONENTIAL SMOOTHING (SES)
fcVisitor <- ses(DataVisitorGACTTS, h=2)
round(accuracy(fcVisitor),2)
autoplot(fcVisitor) +
  autolayer(fitted(fcVisitor), series="Fitted") +
  ylab("GAC Visitors") + xlab("Date")
summary(fcVisitor)

## HOLT'S LINEAR TREND METHOD
fcVisitorH <- holt(DataVisitorGACTTS, h=2)
autoplot(fcVisitorH) +
 autolayer(fitted(fcVisitorH), series="Fitted") +
 ylab("GAC Visitors") + xlab("Date")
summary(fcVisitorH)
