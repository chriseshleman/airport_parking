
# Airport Parking 

rm(list = ls()) # clear global environment 
cat("\014") # clear the console 

library(leaps) 
library(MASS) 
library(olsrr) 
library(data.table) 
library(ggplot2) 
library(zoo) 
library(dplyr) 
library(plyr) 
library(biglm) 
library(datasets) 
library(glmnet) 
library(ISLR) 
library(broom) 
#library(XLConnect) 

#setwd("C:/Users/ceshleman/OneDrive - The Port Authority of New York & New Jersey/MAIN WORK ESHLEMANC/Forecasting/Port Exports 2019") 
setwd("~/OneDrive - The Port Authority of New York & New Jersey/MAIN WORK ESHLEMANC/Airport parking") 
list.files() 
a = read.csv("./Avg_Domestic_Ticket_Prices.csv") 
  a$Year = as.numeric(as.character(a$Year)) 
a$quarter = paste0("0", a$Quarter)
a$quarter = as.Date(paste(a$Year,paste0("0", a$Quarter),"01",sep="-")) 
a = subset(a,a$quarter>="2009-01-01") 
b = read.csv("./EWR_Data.csv") 
c = read.csv("./JFK_Data.csv") 
d = read.csv("./Consumer_Spending.csv") 
#a$quarter = as.Date(paste(a$Year,paste0("0", a$Quarter),"01",sep="-")) 
c$month = as.Date(c$month, format="%m/%d/%y") 
#c = ts(data=c, start=c(2006,1), end=c(2019,3), frequency=12)
d$month = as.Date(d$month, format="%m/%d/%y") 
#d = ts(data=d, start=c(2009,1), end=c(2018,10), frequency=12) 
cd = merge(c,d, by="month",all.x=TRUE) 
names(cd) 
cd$Year = NULL 
cd$Month = NULL 
cd$m = month(cd$month) 
cd$quarter 
cd$quarter = substr(quarters(cd$month), 2, 2)
head(cd$quarter,13) 
cd$yr = year(cd$month) 
head(cd$yr) 
cd$quarter = paste(year(cd$month),cd$quarter,sep="-") 
head(cd$quarter) 
head(cd) 
cd$m = NULL 
cd$q = NULL 
cd$yr = NULL 
#cd2 = aggregate(. ~ quarter, cd, mean)
cd2 = aggregate(. ~ quarter, cd, FUN = mean, na.action=na.pass) 



cd2 = aggregate(cd, nfrequency = 4, FUN = mean) 
cd = as.data.frame(aggregate(cd, nfrequency=4, FUN=mean)/3) 
#c$quarter = seq(as.Date("2006/1/1"), as.Date("2019/1/1"), "quarters") 
# 
head(c2) 
tail(c2) 
c2 = as.quarterly(c, na.rm=TRUE)


### Variable selection 
# When running remember to remove 
  # id column, response column, factors, anything with too many NAs 


### 1. Kitchen sink 
sink = lm(PriceCH ~ ., data=oj[,-c(1:3,6:9,14,18)]) 
sink.tidy = tidy(sink) 
sink.tidy2 = tidy(glance(sink)) 


### 2. Lasso 
# lasso needs a matrix (not a data frame) - y is a vector and X is a matrix 
y = oj[,4] 
X = data.matrix(oj[,-c(1:4,6:9,14,18)], rownames.force = NA) 
fit.lasso = glmnet(x = X, y = y, alpha = 1, standardize=F) # alpha = 1 makes it a lasso model 
lasso.tidy = tidy(fit.lasso) 
lasso.tidy2 = tidy(glance(fit.lasso)) 
plot(fit.lasso, xvar = "lambda") 
coef(fit.lasso, s = .005) # s is the value of lambda 


stp.tidy = tidy(fit.stp) 
stp.tidy2 = tidy(glance(fit.stp)) 
plot(fit.stp) 




#e = data.frame(replicate(4,sample(5:27,1000,rep=TRUE)))
#f = data.frame(replicate(4,sample(0:1,1000,rep=TRUE )))
#e = cbind(e,f) 
#e$qtr = seq(as.Date("2000/1/1"), as.Date("2024/12/30"), by = "quarter")
#names(e) = c("X1","X2","X3","X4","X5","X6","X7","X8","qtr") 

#e$qtr = NULL 

# X. Stepwise 
#fit.stp = regsubsets(PriceCH ~ SalePriceMM + PriceDiff + PctDiscMM,
#                     data = oj, nvmax = 4)
#summary(fit.stp) 
#fit.stp = regsubsets(PriceCH ~ oj[,-c(1:3,6:9,14,18)], data=oj, method = "seqrep",  nvmax = 3) #Can only include 3 variables 
#fit.stp = regsubsets(PriceCH ~ ., data=oj[,-c(1:3,6:9,14,18)] , method = "seqrep",  nvmax = 3) #Can only include 3 variables 
##intercept=TRUE, really.big=FALSE) #"exhaustive", "backward", "forward" 
#summary(fit.stp) 
#fit.stp = regsubsets(x = PriceCH, data=oj[,-c(1:4,6:9,14,18)], weights=NULL, nbest=1, nvmax=8, method = "seqrep")

