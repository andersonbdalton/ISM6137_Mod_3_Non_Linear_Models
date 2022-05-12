#' Generalized Linear Models & Poison Regression
#' Data: Police Stop & Frisk data from New York
#' Question: Is there systemic racism in police stop and frisk?
 
df = read.table("http://www.stat.columbia.edu/~gelman/arm/examples/police/frisk_with_noise.dat", skip=6, header=TRUE)
dim(df)
str(df)
View(df)                                           # Data looks clean

#' Recode factor variables

library(dplyr)
df$eth = recode_factor(df$eth, "1"="Black", "2"="Hispanic", "3"="White")
df$eth = relevel(df$eth, "White")
df$crime = recode_factor(df$crime, "1"="Violent", "2"="Weapons", "3"="Property", "4"="Drug")


#' Data visualization

hist(df$stops)                                     # Not normally distributed
hist(log(df$stops))                                # Much better

den <- density(df$stops)                           # Density function
hist(df$stops, breaks=20, prob=T, main="Histogram of Stops")
lines(den, col="red")

hist(log(df$stops), breaks=20, prob=T, main="Histogram of Stops")
lines(density(log(df$stops)), col="red")

pairs(df)                                          # Pair plots
cor(df[1:3])                                       # Correlation matrix

plot(stops ~ past.arrests, data=df)                # Huge fanning
plot(log(stops) ~ past.arrests, data=df)           # Concavity + fanning
plot(log(stops) ~ log(past.arrests), data=df)      # Good

library(ggplot2)
ggplot(df, aes(x=past.arrests, y=log(stops))) +
  geom_point(color= "steelblue") +
  geom_smooth(method="lm", color="red")            # Fit linear plot

ggplot(df, aes(x=past.arrests, y=log(stops))) +
  geom_point(color= "steelblue") +                 # Fit polynomial plot
  geom_smooth(method="lm", formula = y ~ poly(x, 2), color="red")

#' Question: What analysis should you do?
#' Note: Observations are not independent; why?


#' We want to analyze data across ALL crimes. So we must group data by precincts and 
#' ethnic groups (combine crimes). This will give us 75*2 = 225 rows.

df.sum = df %>%
  group_by(precinct, eth) %>%
  summarise(stops=sum(stops), past_arrests=sum(past.arrests), pop=round(mean(pop)))
View(df.sum)

ggplot(df.sum, aes(x=stops, color=eth, fill=eth)) +
  geom_histogram(breaks=seq(0, 2800, 50)) + facet_wrap(~eth, ncol=1)

ggplot(df.sum, aes(x=past_arrests, y=stops, color=eth)) + 
  geom_point()

ggplot(df.sum, aes(x=log(past_arrests), y=log(stops), color=eth)) + 
  geom_point()

ggplot(df.sum, aes(x=log(past_arrests), y=log(stops), group=eth, color=eth)) + 
  geom_point() + geom_smooth(method.args=list(degree=1))


#' OLS: What will be a good OLS model?

ols1 <- lm (stops ~ past_arrests + eth, data=df.sum)
summary(ols1)
plot(ols1)

ols2 <- lm (log(stops) ~ past_arrests + eth, data=df.sum)
summary(ols2)

ols3 <- lm (log(stops) ~ log(past_arrests) + eth, data=df.sum)
summary(ols3)

# Generalized Linear Model (Maximum Likelihood Estimation)

mle <- glm(log(stops) ~ log(past_arrests) + eth, data=df.sum, family=gaussian)
summary(mle)

library(stargazer)
stargazer(ols1, ols2, ols3, mle, type="text", title="Regression Models of Stop & Frisk Data")
AIC(ols3)


#' Poisson regression
#' stops is count data, and therefore has Poisson distribution

poisson1 <- glm(stops ~ 1, family=poisson (link=log), data=df.sum)   # Intercept only model
summary(poisson1)

poisson2 <- glm(stops ~ log(past_arrests), family=poisson (link=log), data=df.sum)
summary(poisson2)

poisson3 <- glm(stops ~ log(past_arrests) + eth, family=poisson (link=log), data=df.sum)
summary(poisson3)

stargazer(ols3, mle, poisson3, type="text", title="Model Comparison of Stop & Frisk Data")

#' Note: Poisson models have over-dispersion since residual deviance > degrees of freedom.
#' This means that estimates are correct, but standard errors are wrong and unaccounted for
#' by the model. In this case, a quasi-possion model or negative binomial model is appropriate. 

qpoisson <- glm(stops ~ log(past_arrests) + eth, family=quasipoisson (link=log), data=df.sum)
summary(qpoisson)

#' Given that the dispersion parameter (lambda) is 169 and quite far from 1, negative binomial 
#' regression may be more appropriate.

library(MASS)
nbinom <- glm.nb(stops ~ log(past_arrests) + eth, data=df.sum)
summary(nbinom)

stargazer(mle, poisson3, qpoisson, nbinom, type="text", title="Comparison of Stop & Frisk Models")

#' Which model is the best?

#' Extract coefficients and standard errors 

coef <- coef(nbinom)
multipliers <- exp(c(coef[1], coef[1] + coef[2], coef[1] + coef[3]))
print(multipliers)


#' Question: How can we estimate discrimination in specific precincts?

mle <- glm(log(stops) ~ log(past_arrests) + eth + factor(precinct), data=df.sum, family=gaussian)
summary(mle)

poisson = glm(stops ~ log(past_arrests) + eth + factor(precinct), data=df.sum, family=poisson (link=log))
summary(poisson)

qpoisson <- glm(stops ~ log(past_arrests) + eth + factor(precinct), data=df.sum, family=quasipoisson (link=log))
summary(qpoisson)

nbinom <- glm.nb(stops ~ log(past_arrests) + eth + factor(precinct), data=df.sum)
summary(nbinom)

#' Visualizing findings using jtools (jtools also requires broom and ggstance packages)

# install.packages("jtools")
# install.packages("broom")
# install.packages("ggstance")

library(jtools)
plot_summs(qpoisson, scale=TRUE, exp=TRUE)
plot_summs(poisson, qpoisson, scale=TRUE, exp=TRUE)


#' Poisson regression with rate data, e.g., modeling stops/pop
#' In this case, we should use the X variable as an offset

mle <- glm(log(stops) ~ log(past_arrests) + eth + offset(log(pop)), data=df.sum, family=gaussian)
summary(mle)

poisson = glm(stops ~ log(past_arrests) + eth + offset(log(pop)), data=df.sum, family=poisson (link=log))
summary(poisson)

qpoisson = glm(stops ~ eth + log(past_arrests) + offset(log(pop)), data=df.sum, family=quasipoisson (link=log))
summary(qpoisson)

nbinom <- glm.nb(stops ~ log(past_arrests) + eth + + offset(log(pop)), data=df.sum)
summary(nbinom)

stargazer(mle, poisson, qpoisson, nbinom, type="text", title="Model Comparison of Stop & Frisk Rate Data")

