#PREDICTING LOAN REPAYMENT


loans = read.csv("loans.csv")
str(loans)

table(loans$not.fully.paid)
8045/(8045+1533)
# or
prop.table(table(loans$not.fully.paid))

# handle with missing data
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))

summary(missing)

#impute tha dataset
# Imputation predicts missing variable values for a given observation using the variable values that are reported

library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans),"not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
summary(loans)

loans_imputed = read.csv("loans_imputed.csv")                                                                                          
sum(loans != loans_imputed)

#build model
set.seed(144)
library(caTools)
split = sample.split(loans$not.fully.paid,SplitRatio=0.7)
train = subset(loans,split == TRUE)
test = subset(loans,split == FALSE)
LoansLog = glm(not.fully.paid ~.,data=train,family="binomial")
summary(LoansLog)

#profit calculation
(700-710)*-9.406e-03
exp((700-710)*-9.406e-03)

predicted.risk = predict(LoansLog,type="response")

#confusion matrix & find accuracy
table(test$not.fully.paid,predicted.riskd >= 0.5)

(2400+3)/(2400+3+13+457)

# accuracy of baseline model
table(test$not.fully.paid)

2413/(2413+460)

#add prediction risk to test dataset
test$predicted.risk = predicted.risk

#Find the AUC (area under curve)
library(ROCR)
ROCRpred = prediction(predicted.risk, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)


#build bivariate model using one independent variable  (interest rate is  highly significant in the bivariate model) 

LoansBiLog = glm(not.fully.paid ~ int.rate, data=train,family="binomial")

summary(LoansBiLog)

# interest rate is correlated with other risk-related variables, and therefore does not incrementally improve the model when those other variables are included

#find the correlation matrix on a data frame holding discrete values(or factors) 
cor(loans[,unlist(lapply(loans, is.numeric))])

#find the prediction in the bivariate model

predicted.bi = predict(LoansBiLog, newdata=test, type="response")
max(predicted.bi)

#find AUC in the bivariate model
library(ROCR)
ROCRpred = prediction(predicted.bi, test$not.fully.paid)
as.numeric(performance(ROCRpred, "auc")@y.values)

#an investor who invested c dollars in a loan with interest rate r for t years makes c * (exp(rt) - 1) dollars of profit if the loan is paid back in full and -c dollars of profit if the loan is not paid back in full (pessimistically).

#profit in this model
10*exp(0.06*3)

# add profit to the test dataset
test$profit = exp(test$int.rate*3) -1
test$profit[test$not.fully.paid == 1] = -1

str(test)
max(test$profit)*10


#the investor only purchases loans with a high interest rate (a rate of at least 15%)
# build the data frame highInterest 
highInterest = subset(test,int.rate >=0.15)
str(highInterest)

mean(highInterest$profit)

table(highInterest$not.fully.paid)

#proportion of the high-interest loans were not paid back in full
110/(110+327)

#determine the 100th smallest predicted probability of not paying in full

cutoff = sort(highInterest$predicted.risk, decreasing=FALSE)[100]

highInterest100 = subset(test,int.rate >=0.15 & predicted.risk <= cutoff)

str(highInterest100)

#the profit of the investor, who invested $1 in each of these 100 loans 
sum(highInterest100$profit)

# the numbers of 100 selected loans were not paid back in full
table(highInterest100$not.fully.paid)

