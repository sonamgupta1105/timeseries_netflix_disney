# timeseries_netflix_disney
Aim: Assess risk involved in stock prices for Netflix and Disney, based on certain major event by either of the company. 
Hypothesis: A potential major event by either of the company affects the stock prices for both the companies. 
Method: Calculate value at risk, build ARMA and Garch models on pre and post event stock prices for both the companies. Compare which models should the investors use to calculate the risk. 
Conclusion: Netflix seemed to have a loss of 25% on 99% confidence interval, before the event whereas Disney had less risk, in general regardless of the events. Another point to consider was that there could be other factors that may have affected the risk for both the companies during that time period. From the forecasted values of ARMA+Garch model, even if some of the parameters/lags were not statistically significant, it satisfied the conditions for 95% confidence, regardless of any autocorrelations present in the lags. 

Language: R
Software: R Studio
Packages: arima, timeSeries, fgarch, fBasics, ghyp
