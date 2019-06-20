# compare the risk of stocks of each company and compare how these risks 
# changed in response to a certain event, like an issue of a successful show or 
# addition of a new feature

# Set the working directory
setwd("D:/HbgUniv/late spring 2019/ANLY 515 Risk Model/Project")

# Import the libraries
library(readxl)
library(ggplot2)
library(FRAPO)
library(fBasics)
library(GeneralizedHyperbolic)
library(timeSeries)
library(fGarch)
library(ghyp)

# Read the dataset files 
# Read the excel file for disney stock data
disney_10years <- read_excel("disney_10years.xlsx", col_types = c("date", "numeric", "numeric","numeric", "numeric", "numeric"))
# Read the excel file for netflix stock data
netflix_10years <- read_excel("netflix_10years.xlsx", col_types = c("date", "numeric", "numeric","numeric", "numeric", "numeric"))

# Explore the data
disney_10years <- disney_10years[-1,] # Excludes the first row
head(disney_10years)
str(disney_10years)
summary(disney_10years)

netflix_10years <- netflix_10years[-1,] # Excludes the first row
head(netflix_10years)
str(netflix_10years)


# Subset data with date and close prices for Disney and Netflix stocks
disney_close <- disney_10years[,1:2]
netflix_close <- netflix_10years[,1:2]
head(disney_close)
summary(disney_close)
summary(netflix_close)

# Visualize the subset data for Disney stocks
graphics::plot.default(
  x = disney_close$date,
  y = disney_close$close,
  type = "l",
  xlab = "Date",
  ylab = "Closing Value",
  main = "Disney (graphics::plot.default)"
)

# Visualize the subset data for Netflix stocks
graphics::plot.default(
  x = netflix_close$date,
  y = netflix_close$close,
  type = "l",
  xlab = "Date",
  ylab = "Closing Value",
  main = "Netflix (graphics::plot.default)"
)


# using the closing stock prices
disney_close_price<-disney_close$'close'
netflix_close_price <- netflix_close$'close'

# Calculate daily returns for the stock closing prices for Netflix and Disney

disney_ret <- returnseries(disney_close_price)
summary(disney_ret)
netflix_ret <- returnseries(netflix_close_price)
summary(netflix_ret)

# Average return rate for both companies' stock prices
disney <- mean(disney_ret)
netflix <- mean(netflix_ret) # Returns NA cause there are still some missing values
c(disney, netflix)

# Remove NAs from the returns values

disney<- mean(disney_ret, na.rm = TRUE)
netflix <- mean(netflix_ret, na.rm = TRUE)
c(disney, netflix)

# It looks like over the time period of 10 years, both the companies Netflix and Disney did lose 
# stock values


#Find Risk of stocks Measured as Standard Deviation
# Higher stdev, riskier the investing in stocks is
Risk_disney<-sqrt(var(disney_ret, na.rm=TRUE))
Risk_netflix <- sqrt(var(netflix_ret, na.rm = TRUE))
c(Risk_disney, Risk_netflix)

# From above risk analysis for historical daily returns for both the companies, Netflix stocks seemed
# riskier to invest in. It has a higher standard deviation. 

#############
# Choose the dates for special events
# split the above two datasets or extract the rows depending on the date
# calculate the returns for those rows

# Calculate daily returns for the stock closing prices for Netflix and Disney
date_disney <- disney_close['date']
disney_ret <- returnseries(disney_close_price)
disney_returns_dates <- cbind(date_disney, disney_ret)
head(disney_returns_dates)

date_netflix <- netflix_close['date']
netflix_ret <- returnseries(netflix_close_price)
netflix_returns_dates <- cbind(date_netflix, netflix_ret)

head(netflix_returns_dates)
summary(netflix_ret)

# Extract rows for the date of events 

# Subset the dataframe with returns and date as pre-event and post-event 
# extract the data for pre-event till 2017 and post-event from 2017-2019

pre_disney_2016 <- subset(disney_returns_dates, date >= "2009-05-03" & date <= "2017-11-06")
head(pre_disney_2016)
post_disney_2016 <- subset(disney_returns_dates, date >= "2017-11-05" & date <="2019-05-03")
post_disney_2016<-na.omit(post_disney_2016) # Remove NAs
tail(post_disney_2016)
summary(post_disney_2016)

####### Calculate risks for the events subsets  #######

Risk_disney_pre<-sqrt(var(pre_disney_2016$disney_ret, na.rm=TRUE))
Risk_disney_post<-sqrt(var(post_disney_2016$disney_ret, na.rm=TRUE))
c(Risk_disney_pre, Risk_disney_post)

#### Netflix pre-post, risk assessment

pre_netflix_2016 <- subset(netflix_returns_dates,date >= "2009-05-03" & date <= "2017-11-06")
tail(pre_netflix_2016)
post_netflix_2016 <- subset(netflix_returns_dates, date >= "2017-11-05" & date <="2019-05-03")
tail(post_netflix_2016)


Risk_netflix_pre <- sqrt(var(pre_netflix_2016$netflix_ret, na.rm=TRUE))
Risk_netflix_post <-sqrt(var(post_netflix_2016$netflix_ret, na.rm=TRUE))
c(Risk_netflix_pre, Risk_netflix_post)

###### Risk assessment using Generalized Hyperbolic Distribution and other distributions

######## Disney #########

# fit the ghd model to disney events data
pre_disney_times <- timeSeries(pre_disney_2016$disney_ret)

#Post disney
post_disney_times <- timeSeries(post_disney_2016$disney_ret)


## Diagnostics to check which model works best
AIC_pre_disney <- stepAIC.ghyp(pre_disney_times,control = list(maxit = 1000))
# The smaller the AIC value the better is the model
                    
AIC_pre_disney$fit.table
# compares fit of your data to other distributions
#lowest aic=5786.882, (student t test and symmetric)

## Log likelihood test to get the ratio of best model from above test
#predisneyLLH <- lik.ratio.test(ghdfit_pre_disney, tfit_pre_disney)      
# null hyp true= pick nigfit over ghd

# Fit distribution models
# Pre disney
# ghdfit_pre_disney <- fit.ghypuv(pre_disney_times, symmetric = FALSE, control = list(maxit = 1000), na.rm = TRUE)
# hypfit_pre_disney<- fit.hypuv(pre_disney_times, symmetric = FALSE, control = list(maxit = 1000), na.rm = TRUE)
# nigfit_pre_disney<- fit.NIGuv(pre_disney_times, symmetric = FALSE, control = list(maxit = 1000), na.rm = TRUE)
tfit_pre_disney <- fit.tuv(pre_disney_times,control = list(maxit = 1000), na.rm = TRUE)
summary(tfit_pre_disney)

### Plot the ghdfit data
### distribution fit for pre disney events
qqghyp(tfit_pre_disney, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "Pre Disney Student-t Distribution", cex = 0.8)
# qqghyp(hypfit_pre_disney, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
#        gaussian = FALSE, line = FALSE, cex = 0.8)
# qqghyp(nigfit_pre_disney, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
#        gaussian = FALSE, line = FALSE, cex = 0.8)
# legend("topleft", legend = c("GHD", "HYP", "NIG"),
#        col = col.def[-c(1,5)], pch = 1:3)


## AIC for post disney
AIC_post_disney <- stepAIC.ghyp(post_disney_times,control = list(maxit = 2000))

AIC_post_disney$fit.table # t-test (aic=2485.105, true symmetry)
#postdisneyLLH <- lik.ratio.test(ghdfit_post_disney, nigfit_post_disney)
# null hyp = false, pick ghd over nigfit

tfit_post_disney <- fit.tuv(post_disney_times,control = list(maxit = 1000), na.rm = TRUE)

### distribution fit for post disney events
qqghyp(tfit_post_disney, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "Post Disney Student-t Distribution", cex = 0.8)
# qqghyp(hypfit_post_disney, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
#        gaussian = FALSE, line = FALSE, cex = 0.8)
# qqghyp(nigfit_post_disney, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
#        gaussian = FALSE, line = FALSE, cex = 0.8)
# legend("topleft", legend = c("GHD", "HYP", "NIG"),
#        col = col.def[-c(1,5)], pch = 1:3)

##### Calculate VaR for DIsney events

# Probabilities
p <- seq(0.001, 0.05, 0.001)
# Column defintion colors
col.def <- c("black", "red", "blue", "green", "orange")
## VaR for Pre Disney === use the distribution that AIC suggests
t.PreDisney.VaR <- qghyp(p, tfit_pre_disney)
nor.PreDisney.VaR <- qnorm(p, mean = mean(pre_disney_times, na.rm=TRUE), sd = sd(c(pre_disney_times[, 1]), na.rm = TRUE))
emp.PreDisney.VaR <- quantile(x = pre_disney_times, probs = p, na.rm=TRUE)


# Plot of VaR for Pre Disney
plot(emp.PreDisney.VaR, type = "l", xlab = "", ylab = "VaR",main="Pre Disney", axes = FALSE,
     ylim = range(c(t.PreDisney.VaR, nor.PreDisney.VaR, emp.PreDisney.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.PreDisney.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.PreDisney.VaR,t.PreDisney.VaR, nor.PreDisney.VaR)))
lines(seq(along = p), t.PreDisney.VaR, col = "red")
lines(seq(along = p), nor.PreDisney.VaR, col = "blue")
legend("bottomright",
       legend = c("Empirical", "T", "Normal"),
       col = col.def, lty = 1)

## VaR for Post Disney
t.PostDisney.VaR <- qghyp(p, tfit_post_disney)
# hyp.PostDisney.VaR <- qghyp(p, hypfit_post_disney)
# nig.PostDisney.VaR <- qghyp(p, nigfit_post_disney)
nor.PostDisney.VaR <- qnorm(p, mean = mean(post_disney_times, na.rm=TRUE), sd = sd(c(post_disney_times[, 1]), na.rm = TRUE))
emp.PostDisney.VaR <- quantile(x = post_disney_times, probs = p, na.rm=TRUE)
mean(t.PostDisney.VaR)

# Plot of VaR for post disney
plot(emp.PostDisney.VaR, type = "l", xlab = "", ylab = "VaR",main="Post Disney", axes = FALSE,
     ylim = range(c(t.PostDisney.VaR, nor.PostDisney.VaR, emp.PostDisney.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.PostDisney.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.PostDisney.VaR,t.PostDisney.VaR, nor.PostDisney.VaR)))
lines(seq(along = p), t.PostDisney.VaR, col = "red")
lines(seq(along = p), nor.PostDisney.VaR, col = "blue")
legend("bottomright",
       legend = c("Empirical", "T", "Normal"),
       col = col.def, lty = 1)


####### Netflix #######

# Ghd fit models

# Pre Netflix
pre_netflix_times <- timeSeries(pre_netflix_2016$netflix_ret)# change the values to timeseries format

# Post Netflix
post_netflix_times <- timeSeries(post_netflix_2016$netflix_ret)

## Diagnostics to check which model works best
AIC_pre_netflix <- stepAIC.ghyp(pre_netflix_times, control = list(maxit = 1000))

AIC_pre_netflix$fit.table #t-test (aic=8360.938, true symmetry)
#prenetflixLLH <- lik.ratio.test(ghdfit_pre_netflix, nigfit_pre_netflix)
# null hyp=false, pick ghdfit over nigfit

# Fit chosen distribution model
tfit_pre_netflix <- fit.tuv(pre_netflix_times, control = list(maxit = 1000), na.rm = TRUE)

# Plot the ghdfit data for netflix
### Pre Netflix
qqghyp(tfit_pre_netflix, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "Pre Netflix Student-t Distribution", cex = 0.8)

## Diagnostics to check which model works best
AIC_post_netflix <- stepAIC.ghyp(post_netflix_times,control = list(maxit = 1000))

AIC_post_netflix$fit.table #nigfit (aic=3745.120, true symmetry)
#postnetflixLLH <- lik.ratio.test(ghdfit_post_netflix, nigfit_post_netflix)
# null hyp = True, pick nigfit over ghdfit

# Estimate the correct distribution model
nigfit_post_netflix<- fit.NIGuv(post_netflix_times, control = list(maxit = 1000), na.rm = TRUE)

### Post Netflix
qqghyp(nigfit_post_netflix, line = TRUE, ghyp.col = "red", plot.legend = FALSE,
       gaussian = FALSE, main = "Post Netflix NIG Distribution", cex = 0.8)
# qqghyp(hypfit_post_netflix, add = TRUE, ghyp.pch = 2, ghyp.col = "blue",
#        gaussian = FALSE, line = FALSE, cex = 0.8)
# qqghyp(nigfit_post_netflix, add = TRUE, ghyp.pch = 3, ghyp.col = "green",
#        gaussian = FALSE, line = FALSE, cex = 0.8)
# legend("topleft", legend = c("GHD", "HYP", "NIG"),
#        col = col.def[-c(1,5)], pch = 1:3)


## Calculate VaR for Pre Netflix
t.PreNetflix.VaR <- qghyp(p, tfit_pre_netflix)
nor.PreNetflix.VaR <- qnorm(p, mean = mean(pre_netflix_times, na.rm=TRUE), sd = sd(c(pre_netflix_times[, 1]), na.rm = TRUE))
emp.PreNetflix.VaR <- quantile(x = pre_netflix_times, probs = p, na.rm=TRUE)

par(mfrow=c(1,2))
# Plot of VaR for Pre Netflix
plot(emp.PreNetflix.VaR, type = "l", xlab = "", ylab = "VaR",main="Pre Netflix VaR", axes = FALSE,
     ylim = range(c(t.PreNetflix.VaR, nor.PreNetflix.VaR, emp.PreNetflix.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.PreNetflix.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.PreNetflix.VaR, t.PreNetflix.VaR, nor.PreNetflix.VaR)))
lines(seq(along = p), t.PreNetflix.VaR, col = "red")
lines(seq(along = p), nor.PreNetflix.VaR, col = "blue")
legend("bottomright",
       legend = c("Empirical", "T", "Normal"),
       col = col.def, lty = 1)

### Post Netflix VaR

## Calculate VaR for Post Netflix

nig.PostNetflix.VaR <- qghyp(p, nigfit_post_netflix)
nor.PostNetflix.VaR <- qnorm(p, mean = mean(post_netflix_times, na.rm=TRUE), sd = sd(c(post_netflix_times[, 1]), na.rm = TRUE))
emp.PostNetflix.VaR <- quantile(x = post_netflix_times, probs = p, na.rm=TRUE)
mean(nig.PostNetflix.VaR)
## Plot of VaR for Post Netflix
plot(emp.PostNetflix.VaR, type = "l", xlab = "", ylab = "VaR",main="Post Netflix VaR", axes = FALSE,
     ylim = range(c(nig.PostNetflix.VaR, nor.PostNetflix.VaR, emp.PostNetflix.VaR)))
box()
axis(1, at = seq(along = p), labels = names(emp.PostNetflix.VaR), tick = FALSE)
axis(2, at = pretty(range(emp.PostNetflix.VaR, nig.PostNetflix.VaR, nor.PostNetflix.VaR)))
lines(seq(along = p), nig.PostNetflix.VaR, col = "red")
lines(seq(along = p), nor.PostNetflix.VaR, col = "blue")
legend("bottomright",
       legend = c("Empirical", "NIG", "Normal"),
       col = col.def, lty = 1)

################# GARCH model to predict returns for Netflix and Disney ###############

# Pre Disney pre_disney_2016
mean(pre_disney_2016$disney_ret)
par(mfrow=c(1,2))
acf(pre_disney_2016$disney_ret, na.action = na.pass)
pacf(pre_disney_2016$disney_ret, na.action = na.pass)

# From the acf and pacf, the order for ARIMA model for pre disney should be 1, 0,0

# par(mfrow=c(1,2))
# acf(post_disney_2016$disney_ret, na.action = na.pass)
# pacf(post_disney_2016$disney_ret, na.action = na.pass)
# 
# # ARMA model order : 1, 0, 0

# for Netflix
par(mfrow=c(1,2))
acf(pre_netflix_2016$netflix_ret, na.action = na.pass)
pacf(pre_netflix_2016$netflix_ret, na.action = na.pass)
# ARMA model order: 1,0,0

# par(mfrow=c(1,2))
# acf(post_netflix_2016$netflix_ret, na.action = na.pass)
# pacf(post_netflix_2016$netflix_ret, na.action = na.pass)
# 
# # ARMA model order : 1,1,0

# Build ARIMA model for pre events DISNEY data 
arima_pre_disney <- arima(pre_disney_2016$disney_ret, order = c(0,0,0))
arima_pre_disney

predict(arima_pre_disney, n.ahead = 31*1)
# -0.06 possible loss in the returns over the next period

# require(graphics)
# ts.plot(as.ts(pre_disney_2016$disney_ret),predisney_pred_arima$pred, col=1:2, main="ARIMA for Pre Disney")


# Build ARIMA model for pre events NETFLIX data
arima_pre_netflix <- arima(pre_netflix_2016$netflix_ret, order = c(1,0,0))
arima_pre_netflix
predict(arima_pre_netflix, n.ahead=31*1)
# Use arma+garch model to predict risk and future values of the returns
preDisney_garch <- garchFit(formula = ~arma(0,0)+garch(1,1), data=pre_disney_2016$disney_ret)
summary(preDisney_garch)

disney_pred = predict(preDisney_garch, n.ahead = 365*2)
summary(disney_pred)

preNetflix_garch <- garchFit(formula = ~arma(1,0)+garch(1,1), data = pre_netflix_2016$netflix_ret)
summary(preNetflix_garch)

netflix_pred = predict(preNetflix_garch, n.ahead=365*2)
summary(netflix_pred)

# split pre-post, VaR calculated
# use pre data for nflx, dis and use that  for arma+garch model (next month or next quarter) to forecast future risks. 
# Compare the risk measurements (VaR) from post data to arma+garch for pre data.class
# market behavior: buy or not buy. with the announcement, 
# disney either increase or decrease Var compared
# with netflix using the above comparison. 
# The above forecast could be better for other companies
# 
# 
# for pre netflix, arima model produced lower AIC value compared to ARMA+garch model. THis means
# using just the ARIMA model will be beneficial for investors to measure risk.



