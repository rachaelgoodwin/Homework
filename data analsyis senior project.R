
#matrix that compares how much data is missing and the averages of each year
compare_data= matrix(c(percent_missing_2010, percent_missing_2011, percent_missing_2012, 
                       percent_missing_2013, percent_missing_2014, percent_missing_2015, 
                       percent_missing_2016, percent_missing_2017, percent_missing_2018, 
                       percent_missing_2019,  average_2010, average_2011, 
                       average_2012, average_2013, average_2014, average_2015, average_2016, 
                       average_2017, average_2018, average_2019), ncol=2)
colnames(compare_data) <- c("%Missing Data", "Monthly Average")
rownames(compare_data) <- c("Jan 2010","Feb 2010", "Mar 2010", "Apr 2010","May 2010", 
                            "Jun 2010", "Jul 2010", "Aug 2010", "Sep 2010", "Oct 2010", 
                            "Nov 2010", "Dec 2010", "Jan 2011", "Feb 2011", "Mar 2011", "Apr 2011",
                            "May 2011", "Jun 2011", "Jul 2011", "Aug 2011", "Sep 2011", "Oct 2011", 
                            "Nov 2011", "Dec 2011", "Jan 2012", "Feb 2012", "Mar 2012", "Apr 2012",
                            "May 2012", "Jun 2012", "Jul 2012", "Aug 2012", "Sep 2012", "Oct 2012", 
                            "Nov 2012", "Dec 2012", "Jan 2013", "Feb 2013", "Mar 2013", "Apr 2013",
                            "May 2013", "Jun 2013", "Jul 2013", "Aug 2013", "Sep 2013", "Oct 2013", 
                            "Nov 2013", "Dec 2013", "Jan 2014", "Feb 2014", "Mar 2014", "Apr 2014",
                            "May 2014", "Jun 2014", "Jul 2014", "Aug 2014", "Sep 2014", "Oct 2014", 
                            "Nov 2014", "Dec 2014", "Jan 2015", "Feb 2015", "Mar 2015", "Apr 2015",
                            "May 2015", "Jun 2015", "Ju1 2015", "Aug 2015", "Sep 2015", "Oct 2015", 
                            "Nov 2015", "Dec 2015", "Jan 2016", "Feb 2016", "Mar 2016", "Apr 2016",
                            "May 2016", "Jun 2016", "Jul 2016", "Aug 2016", "Sep 2016", "Oct 2016", 
                            "Nov 2016", "Dec 2016", "Jan 2017", "Feb 2017", "Mar 2017", "Apr 2017",
                            "May 2017", "Jun 2017", "Ju1 2017", "Aug 2017", "Sep 2017", "Oct 2017", 
                            "Nov 2017", "Dec 2017", "Jan 2018", "Feb 2018", "Mar 2018", "Apr 2018",
                            "May 2018", "Jun 2018", "Jul 2018", "Aug 2018", "Sep 2018", "Oct 2018", 
                            "Nov 2018", "Dec 2018", "Jan 2019", "Feb 2019", "Mar 2019", "Apr 2019",
                            "May 2019", "Jun 2019", "Jul 2019", "Aug 2019", "Sep 2019", "Oct 2019", 
                            "Nov 2019", "Dec 2019")
#graph of the time series
ts.plot(compare_data[,2], main="Monthly Average PM10",ylab="Monthly Average",lwd=2)
#calculating the mean before and after the calculated change point
mean(compare_data[,2][1:75])
clip(0,75,0,1000)
abline(h=44.59256,col="blue",lwd=2)

mean(compare_data[,2][76:120])
clip(76,120,0,1000)
abline(h=56.55853, col="red",lwd=2)
#plot of acf and pacf
acf(compare_data[,2], main="ACF Plot")
pacf(compare_data[,2], main="PACF Plot")
#making a log model of the data
transform <- log(compare_data[,2])
x <- (1:length(transform))
as.numeric(x >= 80)
#trying to find a candidate model

#removing seasonality from the data
no_season <- transform-as.numeric(0.28620*sin(2*pi/(12)*x)+0.22367*cos(2*pi/(12)*x)
                                  +-0.05939*sin(2*pi/(6)*x)+0.09488*cos(2*pi/(6)*x))

#function to find a potential change point
change_point<-function(transform, range=11:110, num_sim=1000)
  {
  set.seed(1)
  change_point_ts <- c()
  x <- (1:length(transform))
  for (i in range)
  {
    model_change <- lm(transform~  as.numeric(x >=i) +sin(2*pi/(12)*x)
                       +cos(2*pi/(12)*x)+sin(2*pi/(6)*x)+cos(2*pi/(6)*x)
    )
    coeff<-summary(model_change)$coefficients
    #removing the seasonality from the transformed data
    no_season <- transform-as.numeric(coeff[3,1]*sin(2*pi/(12)*x)+coeff[4,1]*cos(2*pi/(12)*x)
                                      +coeff[5,1]*sin(2*pi/(6)*x)+coeff[6,1]*cos(2*pi/(6)*x))
    sse_overall <- sum((no_season-mean(no_season))^2)
    first <- no_season[1:(i-1)]
    second <- no_season[i:length(no_season)]
    #calculating the sse before and after every i
    sse_first <- sum((first-mean(first))^2)
    sse_second <- sum((second-mean(second))^2)
    #dividing their sum by the overall sse to be a test statistic
    statistic <- (sse_first+sse_second)/sse_overall
    change_point_ts <- c(change_point_ts, statistic)
  }
  #storing results in a graph
  pdf(file = "change point ts.pdf") 
  ts.plot(change_point_ts, main="Test Statistics of all Potential Change Points",xlab="Months", ylab="Test Statistic Values")
  dev.off()
  
  #calculating the minimum test statistic, the potential change point
  change_min <- min(change_point_ts)
  min_location_<-which.min(change_point_ts)

  #fitting the model with the seasonality
  model <- lm(transform~as.numeric(x >=min_location_) +sin(2*pi/(12)*x)
              +cos(2*pi/(12)*x)+sin(2*pi/(6)*x)+cos(2*pi/(6)*x))
  #creating a graph of the residuals
  pdf(file = "residuals histogram.pdf")  
  hist((arima(model$residuals, order = c(1,0,1))$residuals), breaks=20, freq=FALSE, main="Model Residuals", xlab="Residuals")
  dev.off()
#simulating the same process to 
arima_coeff <- arima(model$residuals, order = c(1,0,1), include.mean=F)$coef
minimum_sse <- c()
min_location_sim <- c()
sigma_2<-arima(model$residuals, order = c(1,0,1), include.mean=F)$sigma2
#testing residuals of residuals for normality
wilk<-shapiro.test((arima(model$residuals, order = c(1,0,1))$residuals))
pdf(file = "shapiro.pdf")  
hist(arima(model$residuals, order = c(1,0,1))$residuals, freq=FALSE, main="Residuals of the ARMA(1,1) Process")
dev.off()
#simulating the result to test the significance of the identified change point
for (i in 1:num_sim)
{change_point_ts_sim <- c()
seasonal <- coeff[3,1]*sin(2*pi/(12)*x)+coeff[4,1]*cos(2*pi/(12)*x)
+coeff[5,1]*sin(2*pi/(6)*x)+coeff[6,1]*cos(2*pi/(6)*x)
arima_model <- arima.sim(list(order=c(1,0,1),ar=arima_coeff[1], ma= arima_coeff[2]), 
                         n=length(transform), rand.gen=rnorm, sd=sigma_2^(1/2))
simulated_process <- seasonal+arima_model

for (j in range)
{simulated_model <- lm(simulated_process~  as.numeric(x >=j) +sin(2*pi/(12)*x)+cos(2*pi/(12)*x)+sin(2*pi/(6)*x)+cos(2*pi/(6)*x)
)
coeff_sim<-summary(simulated_model)$coefficients
model_resid <- simulated_process-as.numeric(coeff_sim[3,1]*sin(2*pi/(12)*x)+coeff_sim[4,1]*cos(2*pi/(12)*x)
                                            +coeff_sim[5,1]*sin(2*pi/(6)*x)+coeff_sim[6,1]*cos(2*pi/(6)*x))
sse_sim_overall <- sum((model_resid-mean(model_resid))^2)
first_sim <- model_resid[1:(j-1)]
second_sim <- model_resid[j:length(model_resid)]
sse_first_sim <- sum((first_sim-mean(first_sim))^2)
sse_second_sim <- sum((second_sim-mean(second_sim))^2)
statistic_sim <- (sse_first_sim+sse_second_sim)/sse_sim_overall
change_point_ts_sim <- c(change_point_ts_sim, statistic_sim)
}
minimum_sse <- c(minimum_sse, min(change_point_ts_sim))
min_location_sim <- c(min_location_sim, which.min(change_point_ts_sim))
}
#plotting all minimum sse and locations, returning the wilk p value and minimum point
#as well as the p value of the simulation
pdf(file = "minimum sse.pdf")  

hist(minimum_sse)
dev.off()
pdf(file = "minimum location.pdf")  
hist(min_location_sim, main="Minimum Location", freq=F, breaks=20, xlab="Minimum Location")
dev.off()
return(list(statistic=change_min, location=min_location_+range[1]-1, 
            p_value=mean(minimum_sse<change_min), wilk=wilk))
}
#checking for a change point in the data, and then in the data before and after the change
change_point(transform)
first_half<- transform[1:76]
change_point(first_half, range=11:65)
hist(statistic)
second_half<-transform[77:110]
change_point(second_half, range=11:23)
ts.plot(model$residuals)
acf(model$residuals)
pacf(model$residuals)


#calculating an arima model for the data
lag <- arima(model$residuals, order = c(4,0,0), include.mean= FALSE)
lag
#residual analysis
ts.plot(lag$residuals)
acf(lag$residuals)
pacf(lag$residuals)
#fitting a different model to the data
ar.ols(model$residuals, aic= TRUE, order.max =20, demean = F, intercept = F)
arima_coeff <- arima(model$residuals, order = c(1,0,1), include.mean=F)$coef

#arima plot of the sin model residuals, residual analysis
ts.plot(arima(model$residuals, order = c(1,0,1))$residuals)
acf(arima(model$residuals, order = c(1,0,1))$residuals)
pacf(arima(model$residuals, order = c(1,0,1))$residuals)
#plot of the data differenced at lag 12 and then lag 1
ts.plot(diff(diff(compare_data[,2], 12)))
#analyzing normality of the residuals of the arima model
hist((arima(model$residuals, order = c(1,0,1))$residuals))
qqnorm((arima(model$residuals, order = c(1,0,1))$residuals))
qqline((arima(model$residuals, order = c(1,0,1))$residuals))
shapiro.test((arima(model$residuals, order = c(1,0,1))$residuals))
sd((arima(model$residuals, order = c(1,0,1))$residuals))

#an estimation of the coefficents for seasonality and a tsplot of them
seasonal <- 0.28620*sin(2*pi/(12)*x)+0.22367*cos(2*pi/(12)*x)+-0.05939*sin(2*pi/(6)*x)+0.09488*cos(2*pi/(6)*x)
ts.plot(seasonal)
minimum_sse <- c()
min_location_sim <- c()
#simulating 100 arima models and performing regression on them using the sin cos model and
#storing their mse in a vector. then storing the min mse of each in a vector, as well as the place
#it occurred

for (i in 1:10000)
{change_point_ts_sim <- c()
seasonal <- coeff[3,1]*sin(2*pi/(12)*x)+coeff[4,1]*cos(2*pi/(12)*x)
+coeff[5,1]*sin(2*pi/(6)*x)+coeff[6,1]*cos(2*pi/(6)*x)
  arima_model <- arima.sim(list(order=c(1,0,1),ar=arima_coeff[1], ma= arima_coeff[2]), n=120, rand.gen=rnorm, sd=0.1688226)
simulated_process <- seasonal+arima_model

for (j in range)
{simulated_model <- lm(simulated_process~  as.numeric(x >=j) +sin(2*pi/(12)*x)+cos(2*pi/(12)*x)+sin(2*pi/(6)*x)+cos(2*pi/(6)*x)
)
coeff_sim<-summary(simulated_model)$coefficients
model_resid <- simulated_process-as.numeric(coeff_sim[3,1]*sin(2*pi/(12)*x)+coeff_sim[4,1]*cos(2*pi/(12)*x)
                                  +coeff_sim[5,1]*sin(2*pi/(6)*x)+coeff_sim[6,1]*cos(2*pi/(6)*x))
sse_sim_overall <- sum((model_resid-mean(model_resid))^2)
first_sim <- model_resid[1:(j-1)]
second_sim <- model_resid[j:length(model_resid)]
sse_first_sim <- sum((first_sim-mean(first_sim))^2)
sse_second_sim <- sum((second_sim-mean(second_sim))^2)
statistic_sim <- (sse_first_sim+sse_second_sim)/sse_sim_overall
change_point_ts_sim <- c(change_point_ts_sim, statistic_sim)
}
minimum_sse <- c(minimum_sse, min(change_point_ts_sim))
min_location_sim <- c(min_location_sim, which.min(change_point_ts_sim))
hist(minimum_sse, main="Minimum SSE Values", xlab="Minimum SSE", ylab="Density", freq=f,breaks = 20)
}
minimum_sse
min_location_sim
#histogram of the location of all the minimum mse
hist(change_point_ts_sim)
hist(min_location_sim)
hist(minimum_sse)
#pvalue of testing for change point against 
mean(minimum_sse<change_min)
#seeing how many values in the matrix
#dim(stdev_matrix)
#the mean of all simulated mse less than the min of the mse of the sin model from the change point
#analysis
mean(minimum_mse<min(mse))
#finding the min of all the mse
min(mse)
#simulating an arima model
standard_deviation <- arima.sim(list(order=c(1,0,1),ar=0.1401, ma= 0.1395), n=120, rand.gen=rnorm, sd=0.1688226)
ts.plot(seasonal+standard_deviation)
#residual analysis before and after differencing at lag 12
acf(compare_data[,2])
acf(diff(compare_data[,2],12))