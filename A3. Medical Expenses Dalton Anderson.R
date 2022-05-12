rm(list=ls())

library(dplyr)
library(ggplot2)
library(readxl)
library(corrplot)
library(stargazer)
library(PerformanceAnalytics)
#preprocessing

#Load in dataset
#Import data
df_master <- read_excel("Healthinsurance.xlsx")
colnames(df_master)=tolower(make.names(colnames(df_master)))
#drop excluded variables
df =select (df_master,-c(black,hisp))
#check for na
df = df_master
colSums(is.na(df))
#no nas

#histogram of y1

ggplot(df_master, aes(medexpense)) + 
  geom_histogram()
#going to use log to normalize the distribution
df_master$medexpenselog = log(df_master$medexpense)

ggplot(data=df_master, aes(df_master$medexpenselog)) +
labs(title = 'Histogram for Med Expense (Log)', subtitle = 'A more normalized view of out-of-pocket med expenses.') + 
  geom_histogram() 
#much better



#feature engineering

#collapse race and health and earners

#health
df$greathealth <- ifelse(df_master$verygood==1 | df_master$good==1, 1, 0)
df$badhealth <- ifelse(df_master$fair==1 | df_master$poor==1, 1, 0)

#income
df$incomelevel <- ifelse(df$lowincome==1 | df$midincome==0, 'low', 'med')

df$healthins<- relevel(df$healthins, 0)
#education
#df %>% 
#  mutate(df$edlevel = case_when(educyr >= 11 ~ 'no high school'
#                                ,educyr >= 15 ~ 'no college'
#                                ,educyr >= 18 ~ 'college'
#                                ,educyr >= 19 ~ 'upper college'
#                                ,TRUE ~ 'unknown'
#  )
#  )


#remove old columns
df =select (df_master,-c(verygood,good,fair,poor,lowincome,midincome,firmsize,firmlocation))



#data visualizations

hist(df$income)
#going to use log to normalize the distribution
hist(log(df$income))
hist(df$age)

hist(df$educyr)
#sampling basis ?

hist(log(df$adom))

dfplot =select (df,c(medexpense,medexpenselog,income,healthins,private,illnesses,age,married,educyr,blackhisp,ssiratio,poverty,prioritylist))
temp <- dfplot[, c(1:10)]
library(PerformanceAnalytics)
chart.Correlation(temp)

corrplot(cor(dfplot),method = 'number')

#' Regression models

m1 <- lm(medexpense ~ income + healthins + private + illnesses + age, data=df)
summary(m1)

m2 <- lm(medexpenselog ~ income + healthins + private + illnesses + age + blackhisp, data=df)
summary(m2)
m3 <- lm(log(medexpense) ~ income + healthins + private + illnesses + age + blackhisp+greathealth+badhealth, data=df)         # Why is this model interesting?
summary(m3)

m4 <- lm(log(medexpense) ~ illnesses , data=df)         # Why is this model interesting?
summary(m4)

m5 <- lm(log(medexpense) ~ female , data=df)         # Why is this model interesting?
summary(m5)

m6 <- lm(log(medexpense) ~ blackhisp , data=df)         # Why is this model interesting?
summary(m6)

m7 <- lm(log(medexpense) ~ age , data=df)         # Why is this model interesting?
summary(m7)



library(stargazer)
stargazer(m3, m1, m4,  type='text', single.row=TRUE)


#' Test for assumptions

plot(m2)
plot(m3)


shapiro.test(m3$res)                        # Shapiro-Wilk's test of multivariate normality
shapiro.test(m5$res)

bartlett.test(list(m3$res, m3$fit))         # Bartlett's test of homoskedasticity
bartlett.test(list(m5$res, m5$fit))

library("car")                              # Multicollinearity test
vif(m3)
vif(m4)

library(lmtest)
dwtest(m3)                                  # Durbin-Watson test of autocorrelation
dwtest(m4)

