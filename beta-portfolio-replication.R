# Lucas Cusimano
# OEconomica Finance Cohort
# March 2018
# This script replicates table 7 from Reinganum 

#  Set working directory
setwd("~/Google Drive/Oeconomica/finance/Winter/replication/")

# Import the data, rename columns, drop unimportant columns, add risk-free rate
r3 <- read.csv("data/r3000_16-17.csv")
colnames(r3) <- c("unique","date","ticker","fac1","fac2",
                     "price","return","shares.outstanding",
                     "return.r","return.s")
r3 <- r3[c("unique","date","ticker","price", "shares.outstanding",
                 "return","return.r","return.s")]

r3$return <- as.numeric(as.character(r3$return)) #convert to numeric
r3 <- r3[!is.na(r3$return),] #eliminate null values

r3$rf<-0
r3[substr(as.character(r3$date),1,4) == "2016",]$rf <- 0.02
r3[substr(as.character(r3$date),1,4) == "2017",]$rf <- 0.06

r3$return <- r3$return - r3$rf
r3$return.r <- r3$return.r - r3$rf
r3$return.s <- r3$return.s - r3$rf

# Step 3: Caclulate betas for each unique stock

betf <- function(a,b){
  cov(a,b)/var(b)
}

# Takes the unique list of identifiers
stocks = unique(r3$unique)

# This creates a matrix to record our betas
record = matrix(0,length(stocks),2)

for(i in c(1:length(stocks))){
  # Take the desired subset of the specific stock we want and for 2016
  a <- subset(r3, (unique == stocks[i]) & date < 20170000)
  
  # If less than 12 observations in 2016, then we can't accurately determine beta, so we drop it
  if(length(a$unique) < 12){
    # beta value should be NA
    b = NA
  } else{
    # otherwise we calculate the beta using the formula (you will need to write the betf function or calculate directly)
    b <- betf(a$return,a$return.r)
  }
  
  # now we keep a record of the ticker and the associated beta for 2016
  record[,1][i] = stocks[i]
  record[,2][i] = b
  
}

betas <- data.frame(record)
betas[is.na(betas$X2),]
betas <- betas[!is.na(betas$X2),] #eliminate null values
colnames(betas)<-c("id","beta")
# betas <- betas[order(betas$beta),]

num = 10

quants = quantile(betas$beta,probs = seq(0,1,by=1/(num+1)))

q1 = betas[betas$beta < quants[[2]],]$id

qs = matrix(0,length(q1),num)
bs = matrix(0,length(q1),num)

qs[,1] = q1

for(i in c(1:num)){
  
  save = betas[betas$beta <= quants[[i+1]] & betas$beta >= quants[[i]],]
  
  while(length(save$id) < length(qs[,i])){
    save <- rbind(save,c(NA,NA))
  }
  qs[,i] = save$id
  bs[,i] = save$beta
}

portfolio <- data.frame(qs)
# summary(portfolio)

rec <- function(ids,r3){
  sub <- subset(r3, (unique %in% ids) & (date > 20170000))
  sub$value <- sub$price * sub$shares.outstanding
  
  sub$return <- sub$return + sub$rf + 1
  
  weights <- aggregate(sub$value, by=list(sub$unique), mean)
  weights$x <- weights$x / sum(weights$x)
  colnames(weights) <- c("unique","weight")
  
  agg_ret <- aggregate(sub$return, by = list(sub$unique), prod)
  agg_ret$x <- agg_ret$x - 1
  colnames(agg_ret) <- c("unique","return")
  
  total <- merge(agg_ret,weights, by = "unique")
  
  total$wret <- total$return * total$weight
  
  #approximation 
  return (100*(prod(total$wret + 1) - 1)/12)
  
}

chart = matrix(0,num,2)
chart[,2]=apply(bs,2,mean)

for(i in c(1:num)){
  chart[i,][1] = rec(portfolio[,i],r3)
}

# Using portfolios
summary(lm(X1 ~ X2 ,data = data.frame(chart)))

# Using all

betas$rets <- NA
for(i in c(1:length(betas$id))){
  betas[3][i,] = rec(betas$id[i],r3)
}

summary(lm(rets ~ beta ,data = data.frame(betas)))

#chart <- df(chart)
colnames(chart) <- c("returns","beta")
