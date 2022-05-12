#' Exponential, WLS, and FGLS Models of Wage Data
#' Data: Wages.csv (1379 x 6)

setwd("C:/Users/abhatt/Desktop/SDM/Data")
df <- read.csv("Wages.csv")
View(df)
str(df)

# Data Cleaning

which(! complete.cases(df))                       # Checking for missing values
df <- na.omit(df)
df$height <- NULL
df$sex  <- factor(df$sex)
df$sex  <- relevel(df$sex, "female")              # Set baseline for factor variables
df$race <- factor(df$race)
df$race <- relevel(df$race, "white")
str(df)


#' Data Visualization

hist(df$wage, breaks=20, prob=T, main="Histogram of Wage") 
den <- density(df$wage)                    
lines(den, col="red")

df$logwage <- log(df$wage)
which(! complete.cases(df))                                
df <- na.omit(df)
#' There were some negative values in wage! While converting them to log, these
#' values became NaN. So we have to clean out the dataset by removing these rows. 
#' A better approach would have been to remove negative wages in the data cleaning 
#' stage as incorrect data.

hist(df$logwage, breaks=20, prob=T, main="Histogram of log(Wage)")
den <- density(df$logwage)                    
lines(den, col="red")

library(corrplot)
m <- cbind(df$wage, df$age, df$education)
cor(m)
corrplot(cor(m), method="circle")                # Correlations

pairs(df)                                        # Pairs plot

library("PerformanceAnalytics")
chart.Correlation(m)


# Visualizations with ggplot

plot(df$wage ~ df$education)

library(ggplot2)
ggplot(df, aes(x=education, y=wage)) +  
  geom_point(color= "steelblue") + 
  ggtitle("Plot: Wage vs Education") +
  geom_smooth(color="red")                       # Loess plot

ggplot(df, aes(x=education, y=wage)) +  
  geom_point(color= "steelblue") + 
  ggtitle("Linear Plot: Wage vs Education") +
  geom_smooth(method="lm", color="red")

ggplot(df, aes(x=education, y=logwage)) +  
  geom_point(color= "steelblue") + 
  ggtitle("Plot: Log(Wage) vs Education") +
  geom_smooth(color="red")

plot(df$logwage ~ df$age)

ggplot(df, aes(x=age, y=wage)) +  
  geom_point(color= "steelblue") + 
  ggtitle("Plot: Wage vs Age") +
  geom_smooth( color="red")

ggplot(df, aes(x=age, y=logwage)) +  
  geom_point(color= "steelblue") + 
  ggtitle("Plot: Log(Wage) vs Age") +
  geom_smooth(color="red")

ggplot(df, aes(x=logwage, fill=sex)) +
  geom_density(alpha = 0.6) +
  ggtitle("Logwage Distributions by Sex") 

ggplot(df, aes(logwage, fill=race)) +
  geom_density(alpha = 0.6) +
  ggtitle("LogWage Distribution by Race") 


#' OLS  Models

m1 <- lm(wage ~ education + age + sex + race, data=df)
summary(m1)                                              
confint(m1)
plot(m1)

m2 <- lm(logwage ~ education + age + sex + race, data=df)
summary(m2)  
confint(m2)
plot(m2)

m3 <- lm(logwage ~ education + age + I(age*age) + sex + race, data=df)
summary(m3)   
confint(m3)
plot(m3)

library(stargazer)
stargazer(m1, m2, m3, type="text", single.row=TRUE)


#' Heteroscedastic robust standard errors

# Original model (containing heteroscedastic residuals)
m3 <- lm(logwage ~ education + age + I(age*age) + sex + race, data=df)

library(lmtest)                                  # Bresush-Pagan test for heteroscedasticity
bptest(m3)                                       # p<0.05 indicates heteroscedasticity     
summary(m3, cor=T)                               # cor=T displays correlation among betas

vcov(m3)                                         # Variance-covariance matrix
round(sqrt(diag(vcov(m3))), 3)

library("dplyr")                                 # Piping
m3 %>% vcov %>% diag() %>% sqrt() %>% round(3)   # Standard errors from vcov matrix

# HCSE computation (using sandwich package)
# install.packages("sandwich")
library(sandwich) 
vc <- vcovHC(m3)                                 # Heteroscedastic consistent vcov matrix
coeftest(m3, vc)                                 # Heteroscedastic consistent t-tests
m3 %>% vcovHC %>% diag() %>% sqrt() %>% round(3) # Heteroscedastic consistent standard errors

# Heteroscedastic consistent regression
#install.packages("robustbase")
library(robustbase) 
hc <- lmrob(logwage ~ education + age + I(age*age) + sex + race, data=df)
summary(hc)                                            
plot(hc)

stargazer(m3, hc, type="text", single.row=TRUE)


#' Weighted Least Squares
#' Weight are often 1/e or 1/e^2 (for bivariate models) or 1/yhat or 1/yhat^2 (for multivariate models)
#' to compensate for fanning. But you can experiment with different weights.

wt = 1/m3$fitted^2 
wls <- lm(logwage ~ education + age + I(age*age) + sex + race, data=df, weights=wt)
stargazer(m3, hc, wls, type="text", single.row=TRUE)
plot(wls)
vcov(wls)                                        # Variance-covariance matrix


#' Feasible Generalized Least Squares
#' Also check out fgls() and RFGLS package in R

temp <- lm(abs(m3$residuals) ~ education + age + I(age*age) + sex + race, data=df)
summary(temp)

wt <- 1/temp$fitted^2
fgls1 <- lm(logwage ~ education + age + I(age*age) + sex + race, data=df, weights=wt)
summary(fgls1)
plot(fgls1)


temp <- lm(abs(m3$residuals) ~ m3$fitted)
summary(temp)

wt <- 1/temp$fitted^2
fgls2 <- lm(logwage ~ education + age + I(age*age) + sex + race, data=df, weights=wt)
summary(fgls2)
plot(fgls2)
vcov(fgls2)

stargazer(m3, hc, fgls1, fgls2, type="text", single.row=TRUE)
