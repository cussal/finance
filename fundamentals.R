
setwd("~/Google\ Drive/Oeconomica/2017-8/finance/Winter")

#indicate data file
location = "fundamentals.csv"
location2 = "prices.csv"
data <- read.csv(location)
prices_full <- read.csv(location2)

#Microsoft -- Technology
#Macy's -- Consumer Discretionary
#JPMorgan -- Banking
#Merck & Co -- Pharma
#McDonalds -- Fast Food
#Marathon Oil Corp -- Oil
stock_set = c("MSFT","M","JPM","MRK","MCD","MRO")

#fundamentals data
fund <- subset(data, Ticker.Symbol %in% stock_set)

#price data
prices <- subset(prices_full, symbol %in% stock_set)

#aggregate tickers by their symbol, finding mean fundamentals data
#create year column
fund$year <- as.numeric(substring(fund$Period.Ending,1,4))
#aggregate by ticker and year
by_ticker <- aggregate(x = fund, by=list(fund$Ticker.Symbol, fund$year), mean)

#average price of each year
prices$year <- as.numeric(substring(prices$date,1,4))
#take the mean close price per year and per company
avg_close_price <- aggregate(x = prices, by=list(prices$symbol,prices$year), mean)[,c("Group.1","close","year")]

#merge two data frames by ID
#total <- merge(fund,prices, by.x=c("Ticker.Symbol"), by.y=c("symbol"))
#total1 <- merge(by_ticker,prices, by.x=c("Group.1"), by.y=c("symbol"))
#total1[1,]$close * total1[1,]$Estimated.Shares.Outstanding

#### FACTOR MODEL ####
years = c(2011,2012,2013,2014,2015)

#Our Factors:
returns_sp = c(0.15,-0.05,0.02,0.04,0.08)
returns_SMB = c(0.30,0.01,0.03,0.05,-0.02)
returns_HML = c(-0.01,0.03,0.05,0.04,0.01)

#Asset
returns_x = c(0.21,-0.13,0.08,0.10,0.18)

#create returns dataframe
returns = data.frame(years,returns_sp,returns_SMB,returns_HML,returns_x)
colnames(returns) <- c("Year","S&P","SMB","HML","X")

#Find Covariance 
cov(returns_sp ,returns_x)
var(returns_sp )
cov(returns_sp ,returns_x) / var(returns_sp )

#Check results with linear model
linear_returns <- lm(returns_x~returns_sp, data = returns)
print(linear_returns)

#!! The covariance and Coefficient of returns_sp should be exactly equal !!#

#Extend to multiple regression
linear_returns <- lm(returns_x~returns_sp+returns_SMB+returns_HML, data = returns)
print(linear_returns)

############

#Back to the Data

#contains information on outstanding shares, total assets, and price... all the ingredients we need
info <- merge(avg_close_price,by_ticker, by=c("Group.1","year"))[,c("Group.1","year","close","Estimated.Shares.Outstanding","Total.Assets")]
#info <- info[c("Group.1", "year", "close")]
#colnames(info)

# market capitalization: estimated shares outstanding * stock price
info$mkt_cap <- info$close * info$Estimated.Shares.Outstanding

# book-to-market: total book value / market capitalization
info$book_to_mkt <- info$Total.Assets / info$mkt_cap

info[order(info[,6],-rank(info[,7])),]

# beta: cov(rm,ra) / var(rm)
# run into missing data problem...
info_w_na <- merge(avg_close_price,by_ticker, by=c("Group.1","year"),all.x=TRUE)[,c("Group.1","year","close","Estimated.Shares.Outstanding","Total.Assets")]
#transform(info_w_na, Estimated.Shares.Outstanding.New = na.aggregate(Estimated.Shares.Outstanding, by = max))
#tot_mkt_returns <- aggregate(x = info[,c("year","mkt_cap")], by=list(info$year), sum)
