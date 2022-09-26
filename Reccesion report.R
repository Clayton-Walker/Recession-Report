# libraries ----
library(ggplot2)
library(quantmod)
library(dplyr)
library(PortfolioAnalytics)
library(tidyverse)
library(ggthemes)
# Assigning ----
FastFood <- c("mcd", "yum", "dpz", "dri", "cmg") 
DiscountRetail <- c("wmt", "cost", "dltr", "big", "tgt")
Healthcare <- c("cvs", "unh", "elv", "cnc", "mck")
Hotel <- c("mar","lvs", "mgm", "hst", "wynn")
Airline <- c("luv", "dal", "ual", "aal", "alk")
Furniture <- c("wsm", "mlkn", "hni", "lzb", "snbr")
tickers <- c(FastFood, DiscountRetail, Healthcare, Hotel, Airline, Furniture)
startDate <- "2007-12-01"
endDate <- "2009-06-01"

# pulling data ----
  #gets data from Yahoo Finance for our FastFood vector
fastFood_prices <- NULL
for (ticker in FastFood) {
  fastFood_prices <- cbind(fastFood_prices,
                            getSymbols.yahoo(ticker, from = startDate, to = endDate,
                                     periodicity = "monthly", auto.assign = F)[,4])
}
fastFood_prices

  #gets data from Yahoo Finance for our DiscountRetail vector
discountRetail_prices <- NULL
for (ticker in DiscountRetail) {
  discountRetail_prices <- cbind(discountRetail_prices,
                           getSymbols.yahoo(ticker, from = startDate, to = endDate,
                                            periodicity = "monthly", auto.assign = F)[,4])
}
discountRetail_prices

  #gets data from Yahoo Finance for our Healthcare vector
healthcare_prices <- NULL
for (ticker in Healthcare) {
  healthcare_prices <- cbind(healthcare_prices,
                           getSymbols.yahoo(ticker, from = startDate, to = endDate,
                                            periodicity = "monthly", auto.assign = F)[,4])
}
healthcare_prices

  #gets data from Yahoo Finance for our Hotel vector
hotel_prices <- NULL
for (ticker in Hotel) {
  hotel_prices <- cbind(hotel_prices,
                           getSymbols.yahoo(ticker, from = startDate, to = endDate,
                                            periodicity = "monthly", auto.assign = F)[,4])
}
hotel_prices

#gets data from Yahoo Finance for our Airline vector
airline_prices <- NULL
for (ticker in Airline) {
  airline_prices <- cbind(airline_prices,
                        getSymbols.yahoo(ticker, from = startDate, to = endDate,
                                         periodicity = "monthly", auto.assign = F)[,4])
}
airline_prices

#gets data from Yahoo Finance for our Furniture vector
furniture_prices <- NULL
for (ticker in Furniture) {
  furniture_prices <- cbind(furniture_prices,
                          getSymbols.yahoo(ticker, from = startDate, to = endDate,
                                           periodicity = "monthly", auto.assign = F)[,4])
}
furniture_prices

portfolio_prices <- NULL 
  #this gets all data in our date range from the tickers on Yahoo Finance that is in the fourth column,which is the end day price
for (ticker in tickers) {
  portfolio_prices <- cbind(portfolio_prices,
                            getSymbols.yahoo(ticker, from = startDate, to = endDate,
                                             periodicity = "monthly", auto.assign = F)[,4])
}
  # this gets the data on the performance of the SPDR S&P500 ETF to use as a benchmark performance
portfolio_prices
spy_prices <- getSymbols.yahoo('SPY', from = startDate, to = endDate, 
                                periodicity = "monthly", auto.assign = F)[, 4]
spy_prices

# returns for fast food ----
yum_return <- 
  (as.numeric(fastFood_prices$YUM.Close[18] - as.numeric(fastFood_prices$YUM.Close[1]))
   / as.numeric(fastFood_prices$YUM.Close[1]))
yum_return

mcd_return <- 
  (as.numeric(fastFood_prices$MCD.Close[18] - as.numeric(fastFood_prices$MCD.Close[1]))
   / as.numeric(fastFood_prices$MCD.Close[1]))
mcd_return

dpz_return <- 
  (as.numeric(fastFood_prices$DPZ.Close[18] - as.numeric(fastFood_prices$DPZ.Close[1]))
   / as.numeric(fastFood_prices$DPZ.Close[1]))
dpz_return

dri_return <- 
  (as.numeric(fastFood_prices$DRI.Close[18] - as.numeric(fastFood_prices$DRI.Close[1]))
   / as.numeric(fastFood_prices$DRI.Close[1])) 
dri_return

cmg_return <- 
  (as.numeric(fastFood_prices$CMG.Close[18] - as.numeric(fastFood_prices$CMG.Close[1]))
   / as.numeric(fastFood_prices$CMG.Close[1])) 
cmg_return

fastFood_Avg_return <- (yum_return + mcd_return + dpz_return + dri_return + cmg_return) / 5
fastFood_Avg_return
fastFood_Avg_return <-  round(fastFood_Avg_return, digits = 3)
fastFood_Avg_return

# returns for discount retail ----
wmt_return <- 
  (as.numeric(discountRetail_prices$WMT.Close[18] - as.numeric(discountRetail_prices$WMT.Close[1]))
   / as.numeric(discountRetail_prices$WMT.Close[1]))
wmt_return

cost_return <- 
  (as.numeric(discountRetail_prices$COST.Close[18] - as.numeric(discountRetail_prices$COST.Close[1]))
   / as.numeric(discountRetail_prices$COST.Close[1]))
cost_return

dltr_return <- 
  (as.numeric(discountRetail_prices$DLTR.Close[18] - as.numeric(discountRetail_prices$DLTR.Close[1]))
   / as.numeric(discountRetail_prices$DLTR.Close[1]))
dltr_return

big_return <- 
  (as.numeric(discountRetail_prices$BIG.Close[18] - as.numeric(discountRetail_prices$BIG.Close[1]))
   / as.numeric(discountRetail_prices$BIG.Close[1]))
big_return

tgt_return <- 
  (as.numeric(discountRetail_prices$TGT.Close[18] - as.numeric(discountRetail_prices$TGT.Close[1]))
   / as.numeric(discountRetail_prices$TGT.Close[1]))
tgt_return

DiscRetail_Avg_return <- (wmt_return + cost_return + dltr_return + big_return + tgt_return) / 5
DiscRetail_Avg_return
DiscRetail_Avg_return <-  round(DiscRetail_Avg_return, digits = 3)
DiscRetail_Avg_return

# returns for healthcare ----
cvs_return <- 
  (as.numeric(healthcare_prices$CVS.Close[18] - as.numeric(healthcare_prices$CVS.Close[1]))
   / as.numeric(healthcare_prices$CVS.Close[1]))
cvs_return

unh_return <- 
  (as.numeric(healthcare_prices$UNH.Close[18] - as.numeric(healthcare_prices$UNH.Close[1]))
   / as.numeric(healthcare_prices$UNH.Close[1]))
unh_return

elv_return <- 
  (as.numeric(healthcare_prices$ELV.Close[18] - as.numeric(healthcare_prices$ELV.Close[1]))
   / as.numeric(healthcare_prices$ELV.Close[1]))
elv_return

cnc_return <- 
  (as.numeric(healthcare_prices$CNC.Close[18] - as.numeric(healthcare_prices$CNC.Close[1]))
   / as.numeric(healthcare_prices$CNC.Close[1]))
cnc_return

mck_return <- 
  (as.numeric(healthcare_prices$MCK.Close[18] - as.numeric(healthcare_prices$MCK.Close[1]))
   / as.numeric(healthcare_prices$MCK.Close[1]))
mck_return

healthcare_Avg_return <-  ((cvs_return + unh_return + elv_return + cnc_return + mck_return) / 5)  
healthcare_Avg_return
healthcare_Avg_return <-  round(healthcare_Avg_return, digits = 3)
healthcare_Avg_return
# returns for hotels ----
mar_return <- 
  (as.numeric(hotel_prices$MAR.Close[18] - as.numeric(hotel_prices$MAR.Close[1]))
   / as.numeric(hotel_prices$MAR.Close[1]))
mar_return

lvs_return <- 
  (as.numeric(hotel_prices$LVS.Close[18] - as.numeric(hotel_prices$LVS.Close[1]))
   / as.numeric(hotel_prices$LVS.Close[1]))
lvs_return

mgm_return <- 
  (as.numeric(hotel_prices$MGM.Close[18] - as.numeric(hotel_prices$MGM.Close[1]))
   / as.numeric(hotel_prices$MGM.Close[1]))
mgm_return

hst_return <- 
  (as.numeric(hotel_prices$HST.Close[18] - as.numeric(hotel_prices$HST.Close[1]))
   / as.numeric(hotel_prices$HST.Close[1])) 
hst_return

wynn_return <- 
  (as.numeric(hotel_prices$WYNN.Close[18] - as.numeric(hotel_prices$WYNN.Close[1]))
   / as.numeric(hotel_prices$WYNN.Close[1])) 
wynn_return

hotels_Avg_return <- (mar_return + lvs_return + mgm_return + hst_return + wynn_return) / 5
hotels_Avg_return
hotels_Avg_return <-  round(hotels_Avg_return, digits = 3)
hotels_Avg_return

# returns for airlines ----
luv_return <- 
  (as.numeric(airline_prices$LUV.Close[18] - as.numeric(airline_prices$LUV.Close[1]))
   / as.numeric(airline_prices$LUV.Close[1]))
luv_return

dal_return <- 
  (as.numeric(airline_prices$DAL.Close[18] - as.numeric(airline_prices$DAL.Close[1]))
   / as.numeric(airline_prices$DAL.Close[1]))
dal_return

ual_return <- 
  (as.numeric(airline_prices$UAL.Close[18] - as.numeric(airline_prices$UAL.Close[1]))
   / as.numeric(airline_prices$UAL.Close[1]))
ual_return

aal_return <- 
  (as.numeric(airline_prices$AAL.Close[18] - as.numeric(airline_prices$AAL.Close[1]))
   / as.numeric(airline_prices$AAL.Close[1])) 
aal_return

alk_return <- 
  (as.numeric(airline_prices$ALK.Close[18] - as.numeric(airline_prices$ALK.Close[1]))
   / as.numeric(airline_prices$ALK.Close[1])) 
alk_return

airline_Avg_return <- (luv_return + dal_return + ual_return + aal_return + alk_return) / 5
airline_Avg_return
airline_Avg_return <-  round(airline_Avg_return, digits = 3)
airline_Avg_return

# returns for furniture ----
wsm_return <- 
  (as.numeric(furniture_prices$WSM.Close[18] - as.numeric(furniture_prices$WSM.Close[1]))
   / as.numeric(furniture_prices$WSM.Close[1]))
wsm_return

mlkn_return <- 
  (as.numeric(furniture_prices$MLKN.Close[18] - as.numeric(furniture_prices$MLKN.Close[1]))
   / as.numeric(furniture_prices$MLKN.Close[1]))
mlkn_return

hni_return <- 
  (as.numeric(furniture_prices$HNI.Close[18] - as.numeric(furniture_prices$HNI.Close[1]))
   / as.numeric(furniture_prices$HNI.Close[1]))
hni_return

lzb_return <- 
  (as.numeric(furniture_prices$LZB.Close[18] - as.numeric(furniture_prices$LZB.Close[1]))
   / as.numeric(furniture_prices$LZB.Close[1])) 
lzb_return

snbr_return <- 
  (as.numeric(furniture_prices$SNBR.Close[18] - as.numeric(furniture_prices$SNBR.Close[1]))
   / as.numeric(furniture_prices$SNBR.Close[1])) 
snbr_return

furniture_Avg_return <- (wsm_return + mlkn_return + hni_return + lzb_return + snbr_return) / 5
furniture_Avg_return
furniture_Avg_return <-  round(furniture_Avg_return, digits = 3)
furniture_Avg_return

# return for our benchmark ----
spy_return <- 
  (as.numeric(spy_prices$SPY.Close[18] - as.numeric(spy_prices$SPY.Close[1]))
   / as.numeric(spy_prices$SPY.Close[1]))
spy_return
spy_return <-  round(spy_return, digits = 3)
spy_return
# bar graph of our returns ----

portfolio_returns <- data.frame(Investment <-  c("Fast_food","Discount_Retail","Healthcare",
                                                 "Hotel", "Airline", "Furniture", "SPY"),
                 Performance <- c(fastFood_Avg_return,DiscRetail_Avg_return,
                                  healthcare_Avg_return, hotels_Avg_return,
                                  airline_Avg_return, furniture_Avg_return, spy_return))
portfolio_returns
returns_barGraph <- ggplot(data = portfolio_returns, aes(x = Investment, y = Performance)) + 
  geom_bar(stat = "identity", width = .5, fill = "black") +
  geom_text(aes(label = Performance), 
            vjust = -1, hjust = -.74, color = "blue",
            size = 3.5) + theme_economist() + labs(title = "Investment Returns") 
returns_barGraph 
# stock value through the recession  ----
plot(portfolio_prices) 
# Calculating Beta, Alpha, and Sharpe Ratio for fast food----
weights <- c(rep(.2, times = 5))
daysTraded <-  375
RiskFree <- .0366   # this was the average 10yr treasury rate in 2008
spy_ROC <- na.omit(ROC(spy_prices))
spy_ROC

fastFood_ROC <- na.omit(ROC(fastFood_prices))
fastFood_ROC

fastFood_returns_monthly <- Return.portfolio(fastFood_ROC)
fastFood_returns_monthly
fastFood_Beta <- CAPM.beta(fastFood_returns_monthly, spy_ROC, RiskFree/daysTraded) 
fastFood_Alpha <- CAPM.jensenAlpha(fastFood_returns_monthly,spy_ROC, RiskFree/daysTraded)
fastFood_SharpeRatio <- SharpeRatio(fastFood_returns_monthly, RiskFree/daysTraded)
# Calculating Beta, Alpha, and Sharpe Ratio for discount retail----
DiscRetail_ROC <- na.omit(ROC(discountRetail_prices))
DiscRetail_ROC
DiscRetail_returns_monthly <- Return.portfolio(DiscRetail_ROC)
DiscRetail_returns_monthly
DsicRetail_Beta <- CAPM.beta(DiscRetail_returns_monthly, spy_ROC, RiskFree/daysTraded)
DsicRetail_Beta
DsicRetail_Alpha <- CAPM.jensenAlpha(DiscRetail_returns_monthly,spy_ROC, RiskFree/daysTraded)
DsicRetail_Alpha
DsicRetail_SharpeRatio <- SharpeRatio(DiscRetail_returns_monthly, RiskFree/daysTraded)
DsicRetail_SharpeRatio
# Calculating Beta, Alpha, and Sharpe Ratio for healthcare----
healthcare_ROC <- na.omit(ROC(healthcare_prices))
healthcare_ROC
healthcarel_returns_monthly <- Return.portfolio(healthcare_ROC)
healthcarel_returns_monthly
healthcare_Beta <- CAPM.beta(healthcarel_returns_monthly, spy_ROC, RiskFree/daysTraded) 
healthcare_Beta
healthcare_Alpha <- CAPM.jensenAlpha(healthcarel_returns_monthly,spy_ROC, RiskFree/daysTraded)
healthcare_Alpha
healthcare_SharpeRatio <- SharpeRatio(healthcarel_returns_monthly, RiskFree/daysTraded)
healthcare_SharpeRatio
# Calculating Beta, Alpha, and Sharpe Ratio for hotels----
hotels_ROC <- na.omit(ROC(hotel_prices))
hotels_ROC
hotel_returns_monthly <- Return.portfolio(hotels_ROC)
hotel_returns_monthly
hotels_Beta <- CAPM.beta(hotel_returns_monthly, spy_ROC, RiskFree/daysTraded)
hotels_Beta
hotels_Alpha <- CAPM.jensenAlpha(hotel_returns_monthly,spy_ROC, RiskFree/daysTraded)
hotels_Alpha
hotels_SharpeRatio <- SharpeRatio(hotel_returns_monthly, RiskFree/daysTraded)
hotels_SharpeRatio
# Calculating Beta, Alpha, and Sharpe Ratio for airlines----
airline_ROC <- na.omit(ROC(airline_prices))
airline_ROC
airline_returns_monthly <- Return.portfolio(airline_ROC)
airline_returns_monthly
airline_Beta <- CAPM.beta(airline_returns_monthly, spy_ROC, RiskFree/daysTraded)
airline_Beta
airline_Alpha <- CAPM.jensenAlpha(airline_returns_monthly,spy_ROC, RiskFree/daysTraded)
airline_Alpha
airline_SharpeRatio <- SharpeRatio(airline_returns_monthly, RiskFree/daysTraded)
airline_SharpeRatio
# Calculating Beta, Alpha, and Sharpe Ratio for furniture companies----
furniture_ROC <- na.omit(ROC(furniture_prices))
furniture_ROC
furniture_returns_monthly <- Return.portfolio(furniture_ROC)
furniture_returns_monthly
furniture_Beta <- CAPM.beta(furniture_returns_monthly, spy_ROC, RiskFree/daysTraded)
furniture_Beta
furniture_Alpha <- CAPM.jensenAlpha(furniture_returns_monthly,spy_ROC, RiskFree/daysTraded)
furniture_Alpha
furniture_SharpeRatio <- SharpeRatio(furniture_returns_monthly, RiskFree/daysTraded)
furniture_SharpeRatio
# graphing rate of change ----
fastfood_ROC_Graph <- plot(fastFood_ROC)
DiscRetail_ROC_Graph <- plot(DiscRetail_ROC)
healthcare_ROC_Graph <- plot(healthcare_ROC)
hotel_ROC_Graph <- plot(hotels_ROC)
airline_ROC_Graph <- plot(airline_ROC)
furniture_ROC_Graph <- plot(furniture_ROC)
spy_ROC_Graph <- plot(spy_ROC)
par(mfrow = c(2,4))
fastfood_ROC_Graph
DiscRetail_ROC_Graph
healthcare_ROC_Graph
hotel_ROC_Graph
airline_ROC_Graph
furniture_ROC_Graph
spy_ROC_Graph
# Df comparing Alpha and Beta  ----
Industry <- c("Fast Food", "Disc. Retail", "Healthcare", "Hotel", "Airline", "Furniture")
Alpha <- c(fastFood_Alpha, DsicRetail_Alpha, healthcare_Alpha,
           hotels_Alpha, airline_Alpha, furniture_Alpha)
Beta <- c(fastFood_Beta, DsicRetail_Beta, healthcare_Beta, 
          hotels_Beta, airline_Beta, furniture_Beta)
Sharpe_Ratio <- c(fastFood_SharpeRatio, DsicRetail_SharpeRatio, healthcare_SharpeRatio,
                  hotels_Beta, airline_Beta, furniture_Beta)
ratio_df <-  data.frame(Alpha, Beta) 
row.names(ratio_df) <- Industry
ratio_df
# scatter plot comparing industries betas and alphas ----
ratio_df_ggplot <- ggplot(ratio_df,
                              aes( x = Alpha,
                                   y = Beta,
                                   color = Industry)) + 
  geom_point(size = 5) +
  geom_smooth(method=lm, color = "black") +
  labs(title = "Idustries average Beta and Alpha",
       x = "Alpha", 
       y = "Beta",
       color = "Industry") + 
  theme_economist() 
ratio_df_ggplot
# Correlograms  ----
par(mfrow = c(1,2))
acf(spy_prices, main = "ACF") # auto covariance and correlation 
pacf(spy_prices, main = "PACF") # partial auto correlation 

# ARIMA Forecast ----
spy_arima <- auto.arima(spy_prices[,1], seasonal = F)
spy_arima
plot(spy_arima) # this shows the forecast is very unreliable

spy_arima_fit <- residuals(spy_arima)
spy_arima_fit
Box.test(spy_arima_fit, lag = 4, type = "Ljung-Box") #low p value not a good fit
spy_forecast <- forecast(spy_arima_fit, h = 8) # h is lag
spy_forecast
par(mfrow = c(1,1))
plot(spy_forecast) # I dont understand why its flat
forecast_spy_accuracy <- accuracy(spy_forecast)
forecast_spy_accuracy
spy_forecast

# Auto Dickey Fuller Test ----
spy_adf <- (adf.test(spy_prices)) # negative dickey fuller is very unreliable
spy_adf

# Each sectors average return ----
fastFood_returns <- c(yum_return, mcd_return, dpz_return, dri_return, cmg_return)
DiscRetail_returns <- c(wmt_return, cost_return, dltr_return, big_return, tgt_return)
healthcare_returns <- c(cvs_return, unh_return, elv_return, cnc_return, mck_return)
hotel_returns <- c(mar_return, lvs_return, mgm_return, hst_return, wynn_return)
airline_returns <- c(luv_return, dal_return, ual_return, aal_return, alk_return)
furniture_returns <- c(wsm_return, mlkn_return, hni_return, lzb_return, snbr_return)
stock_return_Df <- data.frame(fastFood_returns, DiscRetail_returns, healthcare_returns,
                              hotel_returns, airline_returns, furniture_returns)
stock_return_Df
# Reset plots ----
dev.off()



