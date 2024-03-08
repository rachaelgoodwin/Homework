
rm(list=ls())
read.data <- function(dataname,n)
{
  header <- c("Table Column Name","Site ID","Instrument co-location ID","Date start: 
  local time","Time start: local time","Date end: local time","Time end: local time","Time zone: 
  local","Date start: UTC","Time start: UTC","Date end: UTC",
 "Time end: UTC","PM10: mass","Validation","Missing")
  all_content <- readLines(dataname)
  #skipping the first 147 lines
  skip_lines <- all_content[-c(1:n)]
  data <- read.csv(textConnection(skip_lines), header = FALSE, 
                   stringsAsFactors = FALSE)
  colnames(data) <- header
  return(data)
}

#a helper function for combining data files
combine.data <- function(x,n)
{
  result <- c()
  for(i in 1:length(x))
  {
    data=read.data(x[i],n)
    num_rows=dim(data)[1]
   
    result <- rbind(result, read.data(x[i],n))
  }
  return(result)
}

#counting missing observations
count.missing <- function(x)
{
  return(sum(is.na(x)))
}
change_point<-function(transform, range=11:110, num_sim=1000)
{
  set.seed(1)
  change_point_ts <- c()
  x <- (1:length(transform))
  indices<-which(is.na(transform))
  
  {
    #finding a change point
  for (i in range)
    print(range)
    model_change <- lm(transform~  as.numeric(x >=i) +sin(2*pi/(12)*x)
                       +cos(2*pi/(12)*x)+sin(2*pi/(6)*x)+cos(2*pi/(6)*x))
    coeff<-summary(model_change)$coefficients
    no_season <- transform-as.numeric(coeff[3,1]*sin(2*pi/(12)*x)+coeff[4,1]*cos(2*pi/(12)*x)
                                      +coeff[5,1]*sin(2*pi/(6)*x)+coeff[6,1]*cos(2*pi/(6)*x))
    sse_overall <- sum((no_season-mean(no_season, na.rm=TRUE))^2, na.rm=TRUE)
    first <- no_season[1:(i-1)]
    second <- no_season[i:length(no_season)]
    sse_first <- sum((first-mean(first, na.rm=TRUE))^2, na.rm=TRUE)
    sse_second <- sum((second-mean(second, na.rm=TRUE))^2, na.rm=TRUE)
    #calculating test statistics
    statistic <- (sse_first+sse_second)/sse_overall
    change_point_ts <- c(change_point_ts, statistic)
    print(change_point_ts)
    
  }
  pdf(file = "change point 10 ts.pdf") 
  ts.plot(change_point_ts, main="Test Statistics of all Potential Change Points,PM10 Levels",xlab="Months", ylab="Test Statistic Values")
  dev.off()
  change_min <- min(change_point_ts)
  min_location_<-which.min(change_point_ts)
  
  model <- lm(transform~  as.numeric(x >=min_location_) +sin(2*pi/(12)*x)
              +cos(2*pi/(12)*x)+sin(2*pi/(6)*x)+cos(2*pi/(6)*x))
  #making a graph of the residuals
  pdf(file = "residuals histogram.pdf")  
  hist((arima(model$residuals, order = c(1,0,1))$residuals), breaks=20, freq=FALSE, main="Model Residuals", xlab="Residuals")
  dev.off()
  arima_coeff <- arima(model$residuals, order = c(1,0,1), include.mean=F)$coef
  minimum_sse <- c()
  min_location_sim <- c()
  sigma_2<-arima(model$residuals, order = c(1,0,1), include.mean=F)$sigma2
  #checking normality of the residuals of residuals
  wilk<-shapiro.test((arima(model$residuals, order = c(1,0,1))$residuals))
  pdf(file = "shapiro.pdf")  
  hist(arima(model$residuals, order = c(1,0,1))$residuals)
  dev.off()
  for (i in 1:num_sim)
  {change_point_ts_sim <- c()
  seasonal <- coeff[3,1]*sin(2*pi/(12)*x)+coeff[4,1]*cos(2*pi/(12)*x)
  +coeff[5,1]*sin(2*pi/(6)*x)+coeff[6,1]*cos(2*pi/(6)*x)
  #simulating the arima model and adding the seasonality to it
  arima_model <- arima.sim(list(order=c(1,0,1),ar=arima_coeff[1], ma= arima_coeff[2]), 
                           n=length(transform), rand.gen=rnorm, sd=sigma_2^(1/2))
  simulated_process <- seasonal+arima_model
  
  for (j in range)
  {simulated_model <- lm(simulated_process~  as.numeric(x >=j) +sin(2*pi/(12)*x)+cos(2*pi/(12)*x)+sin(2*pi/(6)*x)+cos(2*pi/(6)*x)
  )
  #calculating the residuals and the sse of the residuals
  coeff_sim<-summary(simulated_model)$coefficients
  model_resid <- simulated_process-as.numeric(coeff_sim[3,1]*sin(2*pi/(12)*x)+coeff_sim[4,1]*cos(2*pi/(12)*x)
                                              +coeff_sim[5,1]*sin(2*pi/(6)*x)+coeff_sim[6,1]*cos(2*pi/(6)*x))
  model_resid[indices]<-NA
  sse_sim_overall <- sum((model_resid-mean(model_resid,na.rm=TRUE))^2,na.rm=TRUE)
  #sse of the two halves of the data
  first_sim <- model_resid[1:(j-1)]
  second_sim <- model_resid[j:length(model_resid)]
  sse_first_sim <- sum((first_sim-mean(first_sim,na.rm=TRUE))^2,na.rm=TRUE)
  sse_second_sim <- sum((second_sim-mean(second_sim,na.rm=TRUE))^2,na.rm=TRUE)
  statistic_sim <- (sse_first_sim+sse_second_sim)/sse_sim_overall
  change_point_ts_sim <- c(change_point_ts_sim, statistic_sim)
  }
  #vectors of all the minimum locations and sse
  minimum_sse <- c(minimum_sse, min(change_point_ts_sim))
  min_location_sim <- c(min_location_sim, which.min(change_point_ts_sim))
  }
  pdf(file = "minimum sse 10.pdf")  
  
  hist(minimum_sse,main="PM10 Minimum SSE Values",xlab="Minimum SSE")
  dev.off()
  pdf(file = "minimum location 10.pdf")  
  hist(min_location_sim, main="PM10 Minimum Location", freq=F, breaks=20, xlab="Minimum Location")
  dev.off()
  return(list(statistic=change_min, location=min_location_+range[1]-1, 
              p_value=mean(minimum_sse<change_min), wilk=wilk))
}
#a vector containing all the data names 
datanames_1_10 <- c(
  "NARSTO_EPA_SS_FRESNO_TEOM-10-5MIN_19990710_19991231_V1.csv",
  "NARSTO_EPA_SS_FRESNO_TEOM-10-5MIN_20000101_20000630_V1.csv",
  "NARSTO_EPA_SS_FRESNO_TEOM-10-5MIN_20000701_20001231_V1.csv",
  "NARSTO_EPA_SS_FRESNO_TEOM-10-5MIN_20010101_20010630_V1.csv"
  
)
datanames_2_10<-c(
  "NARSTO_EPA_SS_FRESNO_TEOM-10-5MIN_20010701_20011231_V1.csv",
  "NARSTO_EPA_SS_FRESNO_TEOM-10-5MIN_20020101_20020630_V1.csv",
  "NARSTO_EPA_SS_FRESNO_TEOM-10-5MIN_20020701_20021231_V1.csv",
  "NARSTO_EPA_SS_FRESNO_TEOM-10-5MIN_20030101_20030331_V1.csv",
  "NARSTO_EPA_SS_FRESNO_TEOM-10-5MIN_20030401_20030630_V1.csv",
  "NARSTO_EPA_SS_FRESNO_TEOM-10-5MIN_20030701_20030930_V1.csv"
  
)
#combining data
final_data_1_10 <- combine.data(datanames_1_10,147)
final_data_2_10 <- combine.data(datanames_2_10,148)
final_data_10 <- rbind(final_data_1_10,final_data_2_10)
final_data_10 <- final_data_10[,c("Date start: 
  local time","Time start: local time","Date end: local time","Time end: local time","PM10: mass","Missing")]

write.csv(final_data_10, file="final_data_10.csv", quote=FALSE, row.names=FALSE)
#finding missing values, setting them to NA
missing_10<-which(final_data_10$`PM10: mass`>9999)
final_data_10$`PM10: mass`[missing_10]<-NA
#random code to make the tapply work, then averaging data over each day
Pm10<-final_data_10$`PM10: mass`
date_start_10<-final_data_10$`Date start: 
  local time`

#coercing the date start into date
date10<-as.Date(date_start_10)
#adding columns of year and date to the final data
final_data_10$year <- as.numeric(format(date10,'%Y'))
final_data_10$month <- as.numeric(format(date10,'%m'))
#random code to make the aggregate function work
year_10<-final_data_10$year
month_10<-final_data_10$month
#averaging monthly pm10 measurements
month_avg_10<-aggregate(x = Pm10, by = list(year_10, month_10), FUN = "mean",na.rm=T)
#resorting data in time order
month_avg_10$Year <- month_avg_10$Group.1[c(25,30,35,40,44,48,1,5,9,13,17,21,26,31,36,41,45,49,2,6,10,14,18,22,27,32,37,42,46,50,3,7,11,15,19,23,
                               28,33,38,43,47,51,4,8,12,16,20,24,29,34,39)]
month_avg_10$Month <- month_avg_10$Group.2[c(25,30,35,40,44,48,1,5,9,13,17,21,26,31,36,41,45,49,2,6,10,14,18,22,27,32,37,42,46,50,3,7,11,15,19,23,
                                               28,33,38,43,47,51,4,8,12,16,20,24,29,34,39)]
month_avg_10$Pm10 <- month_avg_10$x[c(25,30,35,40,44,48,1,5,9,13,17,21,26,31,36,41,45,49,2,6,10,14,18,22,27,32,37,42,46,50,3,7,11,15,19,23,
                                               28,33,38,43,47,51,4,8,12,16,20,24,29,34,39)]
#removing duplicate columns and making a time series, acf,pacf plot for analysis, checking for change point
month_avg_10<-month_avg_10[4:6]
ts.plot(log(month_avg_10[3]),main="Monthly Average PM10",ylab="Monthly Average",lwd=2)

#determining a change point
change_point(log(month_avg_10[,3]), 5:51, 1000)
# a graph of the data befroe and after the detected change point
clip(0,10,0,1000)
abline(h=mean(log(month_avg_10[,3])[1:10]),col="blue",lwd=2)
clip(10,51,0,1000)
abline(h=mean(log(month_avg_10[,3])[10:51],na.rm=TRUE), col="red",lwd=2)
#graphs of the acf and pacf
par(mfrow=c(2,2))
acf(log(month_avg_10$Pm10[1:24]), lag.max=50,main="ACF of Log Transformed \n PM10 Data, Months 1-24")
acf(log(month_avg_10$Pm10[29:51]), lag.max=50,main="ACF of Log Transformed \n PM10 Data, Months 29-51")
pacf(log(month_avg_10$Pm10[1:24]), lag.max=50,main="PACF of Log Transformed \n PM10 Data, Months 1-24")
pacf(log(month_avg_10$Pm10[29:51]), lag.max=50,main="PACF of Log Transformed \n PM10 Data, Months 29-51")

#model for log transformed first half of the data
y_10<-log(month_avg_10$Pm10)
x_10<-1:length(month_avg_10$Pm10)
lm_10_1<-lm(y_10[1:24]~sin(2*pi/(12)*x_10[1:24])+cos(2*pi/(12)*x_10[1:24])+cos(2*pi/(6)*x_10[1:24])+sin(2*pi/(6)*x_10[1:24]))
summary(lm_10_1)
plot(y_10[1:24]~x_10[1:24],type="l")
lines(x[1:24],predict(lm_10_1), col="blue")

#model for log transformed second half of the data
lm_10_2<-lm(y_10[29:51]~sin(2*pi/(12)*x_10[29:51])+cos(2*pi/(12)*x_10[29:51])+cos(2*pi/(6)*x_10[29:51])+sin(2*pi/(6)*x_10[29:51]))
summary(lm_10_2)
plot(y_10[29:51]~x_10[29:51],type="l")
lines(x[29:51],predict(lm_10_2), col="blue")
#missing_1999 <- apply(final_data_1999, 2, count.missing)
ts.plot(final_data_10$`PM10: mass`)

#model for the pm10 data
lm_10<-lm(y_10~sin(2*pi/(12)*x_10+cos(2*pi/(12)*x_10))
            +sin(2*pi/(6)*x_10)+cos(2*pi/(6)*x_10))
summary(lm_10)
plot(y_10~x_10,type='l')
lines(x_10,predict(lm_10), col='blue')
#calculating the seasonality
lm_10<-lm(y_10~sin(2*pi/(12)*x_10)+cos(2*pi/(12)*x_10)
          +sin(2*pi/(6)*x_10)+cos(2*pi/(6)*x_10))
predict_10<-c(predict(lm_10)[1:24],NA,NA,NA,NA,predict(lm_10)[25:47])
lines(x_10, predict_10, col='blue')
#removing the seasonality
coeff<-summary(lm_10)$coefficients
x<-1:length(month_avg_10$Pm10)
no_season_10<- log(month_avg_10$Pm10)-as.numeric(coeff[2,1]*sin(2*pi/(12)*x)+coeff[3,1]*cos(2*pi/(12)*x)
                                                +coeff[4,1]*sin(2*pi/(6)*x)+coeff[5,1]*cos(2*pi/(6)*x))
#summary statistics of the no seasonality data
hist(no_season_10)
shapiro.test(no_season_10)
qqnorm(no_season_10)
qqline(no_season_10)
acf(no_season_10,lag.max=50,na.action=na.pass)
pacf(no_season_10,lag.max=50,na.action=na.pass)
#testing how na values affect the lm function
model_change <- lm(y_10~  as.numeric(x_10 >=27) +sin(2*pi/(12)*x_10)
                   +cos(2*pi/(12)*x_10)+sin(2*pi/(6)*x_10)+cos(2*pi/(6)*x_10))
summary(model_change)
#add years to data, find all the missing observations, make matrices
final_data_split <- final_data[,c("PM10")]
length(final_data_split)
matrix=matrix(final_data_split,nrow= 24)
ts.plot(final_data_split)
#summary statistics of PM10
mean(final_data_split, na.rm = TRUE)
median(final_data_split, na.rm = TRUE)
sd(final_data_split, na.rm = TRUE)
IQR(final_data_split, na.rm = TRUE)