library('ggplot2')
library('forecast')
library('tseries')
library('tidyverse')

setwd("C:/xxx")

DataC <- read.csv("Complaints.csv", stringsAsFactors = F)
DataC$date <- as.Date(DataC$date, format = "%d/%m/%Y")
DataCT <- DataC[, c("date","Total")]
DataCT$Total <- as.numeric(DataCT$Total)

DataCTTS <- ts(DataCT$Total, frequency = 4, start = c(2016,01), end = c(2018,02))

ggplot() +
  geom_line(data = DataCT, aes(x = date, y = Total)) + ylab('Total complaints') + xlab('Date') +
  ggtitle('Total complaints about library services')

### COMPLAINTS ABOUT LIBRARY SERVICES
## SIMPLE EXPONENTIAL SMOOTHING (SES)
fcComplaint <- ses(DataCTTS, h=2)
round(accuracy(fcComplaint),2)

autoplot(fcComplaint) +
  autolayer(fitted(fcComplaint), series="Fitted") +
  ylab("Website visits") + xlab("Date")
summary(fcComplaint)


## HOLT'S LINEAR TREND METHOD
fcComplaintH <- holt(DataCTTS, h=2)
autoplot(fcComplaintH) +
  autolayer(fitted(fcComplaintH), series="Fitted") +
  ylab("Total complaints") + xlab("Date")
summary(fcComplaintH)


## TIME SERIES CROSS VALIDATION FOR COMPARISON PURPOSE
F1 <- tsCV(DataCTTS, ses, h=1)
F2 <- tsCV(DataCTTS, holt, h=1)
F3 <- tsCV(DataCTTS, holt, damped = TRUE, h=1)
# Compare MSE:
mean(F1^2, na.rm=TRUE)
# [1] 1.53593
mean(F2^2, na.rm=TRUE)
# [1] 1.347896
mean(F3^2, na.rm=TRUE)
# [1] 1.431131
# Compare MAE:
mean(abs(F1), na.rm=TRUE)
# [1] 0.9710712
mean(abs(F2), na.rm=TRUE)
# [1] 0.9849289
mean(abs(F3), na.rm=TRUE)
# [1] 1.035029


-----------------------------------------------------------------------------------------
### VISITS TO PUBLIC LIBRARIES

setwd("C:/xxx")
DataLib <- read.csv("Library visits.csv", stringsAsFactors = F)
DataLib$date <- as.Date(DataLib$date, format = "%d/%m/%Y")
DataLibT <- DataLib[, c("date","Total")]
DataLibT$Total <- as.numeric(DataLib$Total)

DataLibTTS <- ts(DataLibT$Total, frequency = 1, start = 2007, end = 2017)

ggplot() +
  geom_line(data = DataLibT, aes(x = date, y = Total)) + ylab('Library visits') + xlab('Year') +
  ggtitle('Physical visits to public libraries')


## HOLT'S LINEAR TREND METHOD
fcLibH <- holt(DataLibTTS, h=2)
autoplot(fcLibH) +
  autolayer(fitted(fcLibH), series="Fitted") +
  ylab("Library visits") + xlab("Date")
summary(fcLibH)


## SIMPLE EXPONENTIAL SMOOTHING (SES)
#fcLib <- ses(DataLibTTS, h=2)
#round(accuracy(fcLib),2)
#autoplot(fcLib) +
  #autolayer(fitted(fcLib), series="Fitted") +
  #ylab("Library visits") + xlab("Year")
#summary(fcLib)
