
library(leaps) 
library(zoo) 
library(olsrr) 
library(glmnet) 
library(lubridate) 
library(ISLR) 
library(broom) 

oldw = getOption("warn")
options(warn = -1)

#setwd("C:/Users/ceshleman/OneDrive - The Port Authority of New York & New Jersey/MAIN WORK ESHLEMANC/Forecasting/Port Exports 2019") 
setwd("~/OneDrive - The Port Authority of New York & New Jersey/MAIN WORK ESHLEMANC/Airport parking") 

#Load data on airport parking activity, ticket prices and consumer spending. 

a = read.csv("./Avg_Domestic_Ticket_Prices.csv") 
a$Year = as.numeric(as.character(a$Year)) 
a$quarter = paste0("0", a$Quarter)
a$quarter = as.Date(paste(a$Year,paste0("0", a$Quarter),"01",sep="-")) 
a = subset(a,a$quarter>="2009-01-01") 
b = read.csv("./EWR_Data.csv") 
c = read.csv("./JFK_Data.csv") 
d = read.csv("./Consumer_Spending.csv") 
dd = read.csv("./Consumer_Spending_short.csv") 
c$month = as.Date(c$month, format="%m/%d/%y") 
d$month = as.Date(d$month, format="%m/%d/%y") 
cd = merge(c,d, by="month",all.x=TRUE) 
cd$Year = NULL 
cd$Month = NULL 
cd$m = month(cd$month) 
cd$quarter = substr(quarters(cd$month), 2, 2)
cd$yr = year(cd$month) 
cd$quarter = paste(year(cd$month),cd$quarter,sep="-") 
cd$m = NULL 
cd$q = NULL 
cd$yr = NULL 

cd = aggregate(. ~ quarter, cd, FUN = mean, na.action=na.pass) 
cd$q = seq(as.Date("2006/1/1"), as.Date("2019/1/1"), by = "quarter")
cd = subset(cd,cd$q<"2018-04-01") 

names(dd) = tolower(names(dd)) 
dd$month = as.Date(dd$month, format="%m/%d/%y") 
cdd = merge(c,dd,by="month") 

cdd$m = month(cdd$month) 
cdd$quarter = substr(quarters(cdd$month), 2, 2)
cdd$yr = year(cdd$month) 
cdd$quarter = paste(year(cdd$month),cdd$quarter,sep="-") 
cdd$m = NULL 
cdd$q = NULL 
cdd$yr = NULL 
cdd = aggregate(. ~ quarter, cdd, FUN = mean, na.action=na.pass) 
cdd$q = seq(as.Date("2006/1/1"), as.Date("2019/1/1"), by = "quarter")
cdd = subset(cdd,cdd$q<"2018-04-01") 

#Remove things with too many blanks (NAs) and factor variables. 

cdd2 = cdd[ , colSums(is.na(cdd)) == 0] 
cdd2 = cdd2[, !sapply(cdd2, is.character)] 
cdd2$Month = NULL 
cdd2$Year = NULL 
cdd2$month = NULL 
cdd2$q = NULL 
cdd2$Total.Revenue = NULL 
cdd2$Avg..Parking.Paid = NULL 

## Variable selection 
#When running remember to remove id column, response column, factors, anything with too many NAs. 

#First, check for strong correlations.

#plot(cdd2)
#Redo this with some ggplot correlation matrix or something. 

# 1. Kitchen sink (less useful) 

sink = lm(Paid.Parking.Activity ~ ., data=cdd2) 
sink.tidy = tidy(sink) 
sink.tidy2 = tidy(glance(sink)) 
sink.tidy 
# So the results suggest (looking at p-values) that we'd dump cell service, 
# Airtrain, personal spending, spending on all goods, spending on durable goods, 
# spending on non-durable goods, spending on all services and 
# spending on household services. 
# Best are domestic enplanements and international enplanements. 
# ... But we know this isn't a good way to do this. Can I articulate it? 
# It's the omitted variable bias issue re: kitchen sink analysis. 

# 2. Stepwise 

# 2a. p-values 
sink = lm(Paid.Parking.Activity ~ ., data=cdd2) 
sink_p = ols_step_backward_p(sink) 
sink_p 

# 2b. AIC 
sink_aic = ols_step_backward_aic(sink) 
sink_aic 


# 3. Lasso 
y = cdd2[,1] 
X = data.matrix(cdd2[,-c(1)], rownames.force = NA) 
fit.lasso = glmnet(x = X, y = y, alpha = 1, standardize=T) # alpha = 1 is lasso 
lasso.tidy = tidy(fit.lasso) 
lasso.tidy2 = tidy(glance(fit.lasso)) 
plot(fit.lasso, xvar = "lambda") 
coef(fit.lasso, s = .6) # s is the value of lambda 

#glmnet(x, y, family=c("gaussian","binomial","poisson","multinomial","cox",
#      "mgaussian"),
#       weights, offset=NULL, alpha = 1, nlambda = 100,
#       lambda.min.ratio = ifelse(nobs

lasso.tidy 
options(warn = oldw)



#Stepwise guidance suggests sticking with international emplanements and goods, and personal consumption is close.

#time = auto.arima(ts(Put Y here),xreg=(put matrix of covariates here), ic="aic", trace=TRUE, allowdrift=FALSE)#,lambda=0, seasonal=TRUE)# 
#summary(fit) 

# OK, document the outputs from these three things. Then try and dig deeper. 
# Then clean up all outputs, clean up the results, do it in Markup (clean), 
# and write the accompanying narrative. 
