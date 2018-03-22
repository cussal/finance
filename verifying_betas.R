# Estimating Beta from CAPM model
# Lucas Cusimano
# 02/15/2018

#####################################
# Note for future updates:          #
#   technically we need to subtract #
#   the risk free rate to have an   #
#   accurate measurement for beta   #
#####################################

# The below code demonstrates merging data, calculating betas from start to finish

setwd("~/...")

# Indicate data files
# Prices taken from Yahoo Finance (yearly) and the NASDAQ website
spx <- read.csv("spx_price.csv")
ms1 <- read.csv("msft_nas.csv")
ms2 <- read.csv("msft_yah.csv")

# Specify a data
from_date = as.Date("2015/01/01")

# Format the data column as a date type
spx$date <- as.Date(spx$date, "%Y/%m/%d")

# Format the open price column in a numeric format
spx$open <- as.numeric(as.character(spx$open))

# Rename columns bu adding "_spx" to the end of each header
colnames(spx) <- paste(colnames(spx), "spx", sep = "_")

# Subset data that occurs after the specified data above
spx <- subset(spx, date_spx > from_date )

# Similar to above
ms1$date <- as.Date(ms1$date, "%Y/%m/%d")
ms1$open <- as.numeric(as.character(ms1$open))
colnames(ms1) <- paste(colnames(ms1), "nas", sep = "_")

# In this case with Yahoo data, the price column was already in the numeric format
ms2$Date <- as.Date(ms2$Date, "%Y-%m-%d")
colnames(ms2) <- paste(tolower(colnames(ms2)), "yah", sep = "_")

# Merging spx and ms1 by their date (by.x specifies the column in the first dataframe)
first <- merge(spx, ms1, by.x = c("date_spx"), by.y = c("date_nas"))
all <- merge(first, ms2, by.x = c("date_spx"), by.y = c("date_yah"))

# Create a new dataframe with the same data as above, except missing one row
changes <- all[-1,]
# Calculate the adjacent difference between each row of open_spx
# Divide it by the previous row (to get percent change)
changes$spx_pc <- diff(all$open_spx) / all$open_spx[-1]
changes$nas_pc <- diff(all$open_nas) / all$open_nas[-1]
changes$yah_pc <- diff(all$open_yah) / all$open_yah[-1]


# Find Betas, these should be almost identical
var_spx = var(changes$spx_pc)
cov(changes$spx_pc,changes$nas_pc)/var_spx
cov(changes$spx_pc,changes$yah_pc)/var_spx
