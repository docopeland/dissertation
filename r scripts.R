## upload databasecsv
set <- 
  read.csv("/Users/danielle/Documents/UManchester/dissertation/Data/databasecsv.csv")
## change the names of the columns
names(set)[c(4,5)] <- c("GDP","CPI")
## create a dummy variables
set$stab <- ifelse(set$CPI <= 3 & set$CPI >= 1, 1, 0) ## stability called 'stab'
set$time <- ifelse(set$Year >= 2004, "1", "0") ## treatment period called 'time'
set$time <- as.numeric(set$time) ## changing it to a numberic class
## view dataset
View(set)

## do descriptive analysis
library(pastecs)
library(moments)
## time dummy
stat.desc(set$time)
summary(set$time)
skewness(set$time, na.rm=TRUE)
kurtosis(set$time, na.rm=TRUE)
## EU dummy
stat.desc(set$EU)
summary(set$EU)
skewness(set$EU, na.rm=TRUE)
kurtosis(set$EU, na.rm=TRUE)
## GDP
stat.desc(set$GDP)
summary(set$GDP)
skewness(set$GDP, na.rm=TRUE)
kurtosis(set$GDP, na.rm=TRUE)
## stability dummy
stat.desc(set$stab)
summary(set$stab)
skewness(set$stab, na.rm=TRUE)
kurtosis(set$stab, na.rm=TRUE)

## plots based on GDP
library(MASS)
library(car)
scatterplot(GDP~Year, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=set, 
            main="GDP growth rate by Year")
scatterplot(GDP~EU, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=set, xlab="EU Status", 
            main="GDP growth rate by EU Status")
scatterplot(GDP~Year|EU, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=set)
histogram(set$GDP, data=set, xlab="GDP", ylab="Percent of Total",
          main="Histogram of GDP growth rate") ## histogram
## densityplot(set$GDP, data=set) ## kernel density plots

## scatterplots based on stability
scatterplot(stab~Year, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=set, main="Stability
            by Year")
scatterplot(stab~EU, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=set, xlab="EU Status", 
            main="Stability by EU Status")
scatterplot(stab~Year|EU, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=set)
histogram(set$stab, xlab = "Stability Status", ylab="Percent of Total",
          main="Histogram of Stability") ## histogram
##densityplot(set$stab, data=set) ## kernel density plots

## coplots of GDP and stability
scatterplot(GDP~stab, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=set)
scatterplot(stab~GDP, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=set)

## linear models
GDP.lm <- lm(GDP~EU*time, data=set)
summary(GDP.lm)
stargazer(GDP.lm, type="html", dep.var.labels=c("Gross Domestic Product growth rates"), 
          covariate.labels=c("Gamma", "Lambda", "Delta G", "Alpha"), out="GDP.lm.htm")
ncvTest(GDP.lm) ##Non-constant Variance Score Test; test for homoskedasticity; need p<.10
plot(fitted(GDP.lm), rstandard(GDP.lm))
stab.prob <- glm (stab~EU*time, family=binomial(link="probit"), data=set)
summary(stab.prob)
stargazer(stab.prob, type="html", dep.var.labels=c("Stability"), 
          covariate.labels=c("Gamma", "Lambda", "Delta G", "Alpha"), out="stab.prob.htm")
null <- glm(stab~1, family=binomial(link="probit"), data=set)
plot(fitted(stab.prob), rstandard(stab.prob))
library(rcompanion)
nagelkerke(stab.prob) ## Pseudo R-Squareds, Chi-Squared, p-value