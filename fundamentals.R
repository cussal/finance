# R functions for data analysis
# Lucas Cusimano
# 01/26/2018

# The below code gives some idea about ways on how to manipuate data, specifically:
  # reading in data, 
  # subsetting the data on rows,
  # creating new columns,
  # aggregating data,
  # subsetting the data on columns

# In terms of finance, it demonstrates:
  # calculating betas


# Setting the working directory
setwd("~/Google\ Drive/Oeconomica/2017-8/finance/Winter")

# Choosing relevant datafiles
fundamental_location = "fundamentals.csv"
price_location = "prices.csv"

# Reading the datafiles as variables (read.csv for csv format, see read.table for general formats)
fundamentals_full <- read.csv(fundamental_location)
prices_full <- read.csv(price_location)


# Creating a list of the above companies' stock tickers, where syntax is: c(item1, item2,...)
stock_set = c("MSFT","M","JPM","MRK","MCD","MRO")
  #Microsoft -- Technology
  #Macy's -- Consumer Discretionary
  #JPMorgan -- Banking
  #Merck & Co -- Pharma
  #McDonalds -- Fast Food
  #Marathon Oil Corp -- Oil

# Taking a subset of the data we read in before on stock fundamentals
# subset(dataset, condition)
  # condition is a true/false condition for each row
  # testing inclusion of a given value for the Ticker.Symbol column in the list of stocks we created before
fund <- subset(fundamentals_full, Ticker.Symbol %in% stock_set)

# Taking a subset of price data
prices <- subset(prices_full, symbol %in% stock_set)


# Create a new column, year, and fill it with the first four characters in the string Period.Ending
fund$year <- as.numeric(substring(fund$Period.Ending,1,4))

# Aggregate:
  # by their symbol (by=list(...)) and by year
  # using the mean function to find mean fundamentals data for each company in a year
by_ticker <- aggregate(x = fund, by=list(fund$Ticker.Symbol, fund$year), mean)

# Create year column in the same way as above
prices$year <- as.numeric(substring(prices$date,1,4))
# Aggregate: 
  # by by their symbol and by year 
  # using the mean function to find mean price in a year
avg_close_price <- aggregate(x = prices, by=list(prices$symbol,prices$year), mean)

# Selecting certain columns: mean close price per year and per company
avg_close_price <- avg_close_price[,c("Group.1","close","year")]

#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#
# A brief tangent to using the above data, here we demonstrate 
# the CAPM model using hypothetical data

# Create list of years
years = c(2011,2012,2013,2014,2015)

#S&P 500 returns
returns_sp = c(0.15,-0.05,0.02,0.04,0.08) 

# Returns of the Asset
returns_x = c(0.21,-0.13,0.08,0.10,0.18)

# Put all data together in a dataframe
returns = data.frame(years,returns_x,returns_sp)
# Name columns
colnames(returns) <- c("Year","X","S&P")

# Find Covariance (ideally you need to subtract the risk-free rate when doing this calculation)
cov(returns_sp ,returns_x)
# Find Variance
var(returns_sp )
# Find Cov / Var = Beta
cov(returns_sp ,returns_x) / var(returns_sp )

# Check results with linear model
linear_returns <- lm(returns_x~returns_sp, data = returns)
print(linear_returns)

#The covariance and coefficient of returns_sp should be exactly equal
#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#~#

# Back to the Data

# Grab information on outstanding shares, total assets, and price... all the ingredients we need for the three factor model
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
