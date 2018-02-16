# Estimating Beta from CAPM model
# Lucas Cusimano
# 02/15/2018

#####################################
# Note for future updates:          #
#   technically we need to subtract #
#   the risk free rate to have an   #
#   accurate measurement for beta   #
#####################################

setwd("~/...")

#indicate data files
#prices taken from Yahoo Finance
spx <- read.csv("spx_price.csv")
ms1 <- read.csv("msft_nas.csv")
ms2 <- read.csv("msft_yah.csv")

from_date = as.Date("2015/01/01")

spx$date <- as.Date(spx$date, "%Y/%m/%d")
spx$open <- as.numeric(as.character(spx$open))
colnames(spx) <- paste(colnames(spx), "spx", sep = "_")

spx <- subset(spx, date_spx > from_date )

ms1$date <- as.Date(ms1$date, "%Y/%m/%d")
ms1$open <- as.numeric(as.character(ms1$open))
colnames(ms1) <- paste(colnames(ms1), "nas", sep = "_")

ms2$Date <- as.Date(ms2$Date, "%Y-%m-%d")
colnames(ms2) <- paste(tolower(colnames(ms2)), "yah", sep = "_")

first <- merge(spx, ms1, by.x = c("date_spx"), by.y = c("date_nas"))
all <- merge(first, ms2, by.x = c("date_spx"), by.y = c("date_yah"))

#by.y = [,c("date_spx","close_spx","close_nas","close_yah")]
changes <- all[-1,]
changes$spx_pc <- diff(all$open_spx) / all$open_spx[-1]
changes$nas_pc <- diff(all$open_nas) / all$open_nas[-1]
changes$yah_pc <- diff(all$open_yah) / all$open_yah[-1]


## Find Betas (see note above)
var_spx = var(changes$spx_pc)
cov(changes$spx_pc,changes$nas_pc)/var_spx
cov(changes$spx_pc,changes$yah_pc)/var_spx
