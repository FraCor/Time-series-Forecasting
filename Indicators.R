### International Tourism Visits

TourismV <- read.csv("International Tourism Visits.csv", stringsAsFactors = F)
TourismV$date <- as.Date(TourismV$date, format = "%d/%m/%Y")
TourismV2 <- TourismV %>% select(-market) %>% group_by(date)  %>%  summarise_all(funs(sum))
TourismV2$Visits <- as.numeric(TourismV2$Visits)
# Convert data to time series
TourismV2TS <- ts(TourismV2$Visits, frequency = 4, start = c(2002,01), end = c(2018,03))
# Plot the data
ggplot() +
  geom_line(data = TourismV2, aes(x = date, y = Visits)) + ylab('Tourism Visits (million)') + xlab('Year') +
  ggtitle('International Tourism Visits')
  
# Exponential smoothing method
# We run an automatic procedure
# we need to fit the appropriate model, which is selected automatically via some information criterion, AIC
Tourism.ets <- ets(TourismV2TS)
print(Tourism.ets)
plot(Tourism.ets)
# And then we use this to forecast
fTourism.ets <- forecast(Tourism.ets,h=5)
# Since now we have a model we can produce analytical prediction intervals
print(Tourism.ets)
# Plot the resulting forecast and prediction intervals
plot(fTourism.ets)
summary(fTourism.ets)

# ARIMA method
# Use ARIMA to produce forecasts using the function auto.arima. This function has the advantage that it can
# automatically specify an appropriate ARIMA model (by default the AICc is used) 
# We build ARIMA model using the auto.arima function from the forecast package
Tourism.Arima <- auto.arima(TourismV2TS, seasonal=T)
summary(Tourism.Arima)
# We produce the forecast 
fTourism.arima <- forecast(Tourism.Arima, h=8)
plot(fTourism.arima)
summary(fTourism.arima)

# ------------------------------------------------------
### Overseas resident spend outside of London
DataSp <- read.csv("Spend.csv", stringsAsFactors = F)
DataSp$date <- as.Date(DataSp$date, format = "%d/%m/%Y")
DataSp2 <- DataSp %>% select(-market) %>% group_by(date)  %>%  summarise_all(funs(sum))
DataSp2$Spend <- as.numeric(DataSp2$Spend)
# Annually forecasts
# DataSpY <- DataSp %>% select(-market) %>% group_by(date=lubridate::year(date))  %>%  summarise_all(funs(sum))
DataSp2TS <- ts(DataSp2$Spend, frequency = 4, start = c(2002,01), end = c(2018,03))
ggplot() +
  geom_line(data = DataSp2, aes(x = date, y = Spend)) + ylab('Spend £m') + xlab('Year') +
  ggtitle('Overseas resident spend outside of London')

# Exponential smoothing method
# fit the appropriate model
Spend.ets <- ets(DataSp2TS)
print(Spend.ets)
plot(Spend.ets)
# And then we use this to forecast
fSpend.ets <- forecast(Spend.ets,h=8)
# Since now we have a model we can produce analytical prediction intervals
print(Spend.ets)
# Plot the resulting forecast and prediction intervals
plot(fSpend.ets)
summary(fSpend.ets)

# ARIMA method
Spend.Arima <- auto.arima(DataSp2TS, seasonal=T)
summary(Spend.Arima)
# We produce the forecast 
fSpend.arima <- forecast(Spend.Arima, h=8)
plot(fSpend.arima)
summary(fSpend.arima)

# ------------------------------------------------------
### GVA contributions from DCMS sectors
## GVA expressed in current prices
GVA <- read.csv("GVA.csv", stringsAsFactors = F)
GVA$date <- as.Date(GVA$date, format = "%d/%m/%Y")
GVAPrice <- GVA[, c("date","Prices")]
GVAPrice$Prices <- as.numeric(GVAPrice$Prices)
GVAPriceTS <- ts(GVAPrice$Prices, frequency = 1, start = 2010, end = 2017)
ggplot() +
  geom_line(data = GVAPrice, aes(x = date, y = Prices)) + ylab('Current prices £bn') + xlab('Year') +
  ggtitle('GVA contributions from DCMS sectors')

#par(mfrow=c(1,1))

# Exponential smoothing method
GVA1.ets <- ets(GVAPriceTS)
print(GVA1.ets)
plot(GVA1.ets)
fGVA1.ets <- forecast(GVA1.ets,h=6)
print(GVA1.ets)
plot(fGVA1.ets)
summary(fGVA1.ets)

# ARIMA method
GVA1.Arima <- auto.arima(GVAPriceTS, seasonal=T)
summary(GVA1.Arima)
fGVA1.arima <- forecast(GVA1.Arima, h=5)
plot(fGVA1.arima)
summary(fGVA1.arima)

## GVA expressed in chained volume measures
GVAVolume <- GVA[, c("date","Volumes")]
GVAVolume$Volumes <- as.numeric(GVAVolume$Volumes)
GVAVolumeTS <- ts(GVAVolume$Volumes, frequency = 1, start = 2010, end = 2017)
ggplot() +
  geom_line(data = GVAVolume, aes(x = date, y = Volumes)) + ylab('Chained volume measures £bn') + xlab('Year') +
  ggtitle('GVA contributions from DCMS sectors')

# Exponential smoothing method
GVA2.ets <- ets(GVAVolumeTS)
print(GVA2.ets)
plot(GVA2.ets)
fGVA2.ets <- forecast(GVA2.ets,h=6)
print(GVA2.ets)
plot(fGVA2.ets)
summary(fGVA2.ets)

# ARIMA method
GVA2.Arima <- auto.arima(GVAVolumeTS, seasonal=T)
summary(GVA2.Arima)
fGVA2.arima <- forecast(GVA2.Arima, h=6)
plot(fGVA2.arima)
summary(fGVA2.arima)

plot <- grid.arrange(fGVA1.arima,fGVA2.arima)

# ------------------------------------------------------
### Employment and exports from DCMS sectors
## Employment
Employ<- read.csv("Employment.csv", stringsAsFactors = F)
Employ$date <- as.Date(Employ$date, format = "%d/%m/%Y")
Employ$Employment <- as.numeric(Employ$Employment)
EmployTS <- ts(Employ$Employment, frequency = 1, start = 2011, end = 2017)
ggplot() +
  geom_line(data = Employ, aes(x = date, y = Employment)) + ylab('Employment 000s') + xlab('Date') +
  ggtitle('Employment from DCMS sectors')

# Exponential smoothing method
Employ.ets <- ets(EmployTS)
print(Employ.ets)
plot(Employ.ets)
fEmploy.ets <- forecast(Employ.ets,h=6)
print(Employ.ets)
plot(fEmploy.ets)
summary(fEmploy.ets)

# ARIMA method
Employ.Arima <- auto.arima(EmployTS, seasonal=T)
summary(Employ.Arima)
fEmploy.arima <- forecast(Employ.Arima, h=6)
plot(fEmploy.arima)
summary(fEmploy.arima)

# ------------------------------------------------------
## Exports of goods
Export<- read.csv("Exports.csv", stringsAsFactors = F)
Export$date <- as.Date(Export$date, format = "%d/%m/%Y")
ExportG <- Export[, c("date","Goods")]
ExportG$Goods <- gsub(",", "", ExportG$Goods)
ExportG$Goods <- as.numeric(ExportG$Goods)
ExportG_TS <- ts(ExportG$Goods, frequency = 1, start = 2010, end = 2016)
ggplot() +
  geom_line(data = ExportG, aes(x = date, y = Goods)) + ylab('Exports of goods') + xlab('Date') +
  ggtitle('Exports of goods from DCMS sectors')

# Exponential smoothing method
Goods.ets <- ets(ExportG_TS)
print(Goods.ets)
plot(Goods.ets)
fGoods.ets <- forecast(Goods.ets,h=6)
print(Goods.ets)
plot(fGoods.ets)
summary(fGoods.ets)

# ARIMA method
Goods.Arima <- auto.arima(ExportG_TS, seasonal=T)
summary(Goods.Arima)
fGoods.arima <- forecast(Goods.Arima, h=6)
plot(fGoods.arima)
summary(fGoods.arima)

## Exports of services
ExportS <- Export[, c("date","Services")]
ExportS$Services <- as.numeric(ExportS$Services)
ExportS_TS <- ts(ExportS$Services, frequency = 1, start = 2010, end = 2016)
ggplot() +
  geom_line(data = ExportS, aes(x = date, y = Services)) + ylab('Exports of services') + xlab('Date') +
  ggtitle('Exports of services from DCMS sectors')

# Exponential smoothing method
Services.ets <- ets(ExportS_TS)
print(Services.ets)
plot(Services.ets)
fServices.ets <- forecast(Services.ets,h=6)
print(Services.ets)
plot(fServices.ets)
summary(fServices.ets)

# ARIMA method
Services.Arima <- auto.arima(ExportS_TS, seasonal=T)
summary(Services.Arima)
fServices.arima <- forecast(Services.Arima, h=6)
plot(fServices.arima)
summary(fServices.arima)

# ------------------------------------------------------
### % adults participating in:
Adult <- read.csv("Adults percentage.csv", stringsAsFactors = F)
Adult$date <- as.Date(Adult$date, format = "%d/%m/%Y")
## Arts
AdultArt <- Adult[, c("date","Arts")]
AdultArt$Arts <- as.numeric(AdultArt$Arts)
AdultArt_TS <- ts(AdultArt$Arts, frequency = 1, start = 2006, end = 2018)
ggplot() +
  geom_line(data = AdultArt, aes(x = date, y = Arts)) + ylab('Proportion of adults') + xlab('Year') +
  ggtitle('Adults who have engaged with the Arts')

# Exponential smoothing method
Art.ets <- ets(AdultArt_TS)
print(Art.ets)
plot(Art.ets)
fArt.ets <- forecast(Art.ets,h=6)
print(Art.ets)
plot(fArt.ets)
summary(fArt.ets)

# ARIMA method
Art.Arima <- auto.arima(AdultArt_TS, seasonal=T)
summary(Art.Arima)
fArt.arima <- forecast(Art.Arima, h=6)
plot(fArt.arima)
summary(fArt.arima)

## Heritage sites
AdultHeritage <- Adult[, c("date","Heritage")]
AdultHeritage$Heritage <- as.numeric(AdultHeritage$Heritage)
AdultHeritage_TS <- ts(AdultHeritage$Heritage, frequency = 1, start = 2006, end = 2018)
ggplot() +
  geom_line(data = AdultHeritage, aes(x = date, y = Heritage)) + ylab('Proportion of adults') + xlab('Year') +
  ggtitle('Adults who have visited a Heritage site')

# Exponential smoothing method
Heritage.ets <- ets(AdultHeritage_TS)
print(Heritage.ets)
plot(Heritage.ets)
fHeritage.ets <- forecast(Heritage.ets,h=6)
print(Heritage.ets)
plot(fHeritage.ets)
summary(fHeritage.ets)

# ARIMA method
Heritage.Arima <- auto.arima(AdultHeritage_TS, seasonal=T)
summary(Heritage.Arima)
fHeritage.arima <- forecast(Heritage.Arima, h=6)
plot(fHeritage.arima)
summary(fHeritage.arima)

## Museums or galleries
AdultMus <- Adult[, c("date","Museums")]
AdultMus$Museums <- as.numeric(AdultMus$Museums)
AdultMus_TS <- ts(AdultMus$Museums, frequency = 1, start = 2006, end = 2018)
ggplot() +
  geom_line(data = AdultMus, aes(x = date, y = Museums)) + ylab('Proportion of adults') + xlab('Year') +
  ggtitle('Adults who have visited a Museum or Gallery')

# Exponential smoothing method
Mus.ets <- ets(AdultMus_TS)
print(Mus.ets)
plot(Mus.ets)
fMus.ets <- forecast(Mus.ets,h=6)
print(Mus.ets)
plot(fMus.ets)
summary(fMus.ets)

# ARIMA method
Mus.Arima <- auto.arima(AdultMus_TS, seasonal=T)
summary(Mus.Arima)
fMus.arima <- forecast(Mus.Arima, h=6)
plot(fMus.arima)
summary(fMus.arima)

## Libraries
AdultL <- Adult[, c("date","Libraries")]
AdultL$Libraries <- as.numeric(AdultL$Libraries)
AdultL_TS <- ts(AdultL$Libraries, frequency = 1, start = 2006, end = 2018)
ggplot() +
  geom_line(data = AdultL, aes(x = date, y = Libraries)) + ylab('Proportion of adults') + xlab('Year') +
  ggtitle('Adults who have used a public Library service')

# Exponential smoothing method
Lib.ets <- ets(AdultL_TS)
print(Lib.ets)
plot(Lib.ets)
fLib.ets <- forecast(Lib.ets,h=6)
print(Lib.ets)
plot(fLib.ets)
summary(fLib.ets)

# ARIMA method
Lib.Arima <- auto.arima(AdultL_TS, seasonal=T)
summary(Lib.Arima)
fLib.arima <- forecast(Lib.Arima, h=6)
plot(fLib.arima)
summary(fLib.arima)

## Sport website
AdultS <- Adult[, c("date","Sport")]
AdultS$Sport <- as.numeric(AdultS$Sport)
AdultS[is.na(AdultS)] <- 0
AdultS_TS <- ts(AdultS$Sport, frequency = 1, start = 2006, end = 2018)
ggplot() +
  geom_line(data = AdultS, aes(x = date, y = Sport)) + ylab('Proportion of adults') + xlab('Year') +
  ggtitle('Adults who have visited a public a Sport Website')

# Exponential smoothing method
Sport.ets <- ets(AdultS_TS)
print(Sport.ets)
plot(Sport.ets)
fSport.ets <- forecast(Sport.ets,h=6)
print(Sport.ets)
plot(fSport.ets)
summary(fSport.ets)

# ARIMA method
Sport.Arima <- auto.arima(AdultS_TS, seasonal=T)
summary(Sport.Arima)
fSport.arima <- forecast(Sport.Arima, h=6)
plot(fSport.arima)
summary(fSport.arima)
