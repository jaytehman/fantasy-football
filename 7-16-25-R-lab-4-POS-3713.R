#R Lab 4
#POS 3713
#Summer B
#7-16-25

#First, let's load in tidyverse

library(tidyverse)

#We'll need three more packages for this today: broom, stargazer, and ggrepel
#We won't load them just yet, but let's make sure we've downloaded them

#install.packages("broom")
#install,packages("stargazer")
#install.packages("ggrepel")

#load in our data

data <- read_rds("2024-fantasy-football.RDS")

#Let's get a feel for the data first!

glimpse(data)

data %>% 
  ggplot(aes(x=FantasyPoints))+
  geom_histogram()+
  theme_minimal()+
  labs(x="Fantasy Points",
       y="")

#Let's get some basic summary statistics

summary(data$FantasyPoints)


#Let's make a bar graph of player positions

data %>% 
  ggplot(aes(x=Pos))+
  geom_bar()+
  theme_minimal()+
  labs(x="Positions",
       y="")


#Let's make a basic scatterplot (How does Turnovers v Points Sound?)

#We're going to use ggrepel here

library(ggrepel)

?geom_text_repel

data%>%
  #filter(Pos=="QB") %>% 
  ggplot(aes(x=Turnovers, y=FantasyPoints, label=Player))+
  geom_text_repel()+
  geom_smooth(method = "lm")+
  labs(caption ="",
       x="Turnovers",
       title="2024 Fantasy Points vs Turnovers")+
  theme_minimal()


cor(data$Turnovers, data$FantasyPoints)

#That's a positive correlation?
#Turnovers are bad! Why would fantasy football reward turnovers?

#Let's try and run a regression, and see what that says

#We'll use the lm() function

OLS_1 <-lm(FantasyPoints ~ Turnovers, data=data)

#We can check out the object OLS_1 using view()
View(OLS_1)

#That gives us a lot of information, but not really in a useful way.

#What about summary?

summary(OLS_1)

#We'll use broom to get a tidier version of this

broom::tidy(OLS_1)

#Now, let's try and calculate the 95% confidence interval around our estimate
#of beta hat

#Let's create an object for both beta_hat and it's standard error

beta_hat <- as.numeric(broom::tidy(OLS_1)[2,2])

se_beta_hat <- as.numeric(broom::tidy(OLS_1)[2,3])

beta_hat+ (1.96*se_beta_hat) #That's our upper bound

beta_hat-(1.96*se_beta_hat) #That's our lower bound

#Let's try and get a regression table

#We'll use stargazer

stargazer::stargazer(OLS_1)

#I'm assuming y'all don't write in LaTeX, so this is probably meaningless

stargazer::stargazer(OLS_1, type = "text")

#how about we add confidence intervals

stargazer::stargazer(OLS_1, type = "text", ci=TRUE, ci.level=0.95)

#Generally, we prefer having the standard errors instead of CIs

#Let's plot our residuals versus our predicted values

rvf <-data.frame(cbind(OLS_1$fitted.values, OLS_1$residuals, OLS_1$model$Turnovers))

rvf <- rvf %>% 
  rename(fitted = X1) %>% 
  rename(residuals = X2) %>%
  rename(turnovers=X3) %>% 
  glimpse()

rvf %>% 
  ggplot(aes(x=fitted, y=residuals))+
  geom_point()+
  theme_minimal()


rvf %>% 
  ggplot(aes(x=turnovers, y=residuals))+
  geom_point()+
  theme_minimal()


#Is there a clear pattern?

#Let's try something

cor(rvf$residuals, rvf$turnovers) #that's tiny

#What's the substantive meaning of our terms?
#Let's graph this!

data %>% 
  ggplot(aes(x=Turnovers, y=FantasyPoints))+
  geom_point(alpha=.15)+
  geom_abline(slope = OLS_1[["coefficients"]][["Turnovers"]], intercept = OLS_1[["coefficients"]][["(Intercept)"]])+
  theme_minimal()+
  labs(x="Turnovers",
       y="Fantasy Points")

#So, we don't have heteroskedacisity, our p-values are really tiny,
#and we have a p-value that leads us to reject the null
#Does this mean that Turnovers cause fantasy points?


#We're clearly missing some items that cause points,
#like touchdowns, recieving yards, etc.

#Let's run another model, but with something that probably doesn't have
#a (direct) causal relationship with fantasy points

OLS_2 <- lm(FantasyPoints ~ Pos, data=data)
summary(OLS_2)
broom::tidy(OLS_2)

#When running an OLS model with categorical variables,
#you have to have a reference category (in this case, QB)

#Let's go back to stargazer

stargazer::stargazer(OLS_1, OLS_2, type = "text")

#Now, we know how Fantasy points are actually calculated
#We know that Touchdowns are worth 6 points each
#Passing yards are worth 0.04 points each
#Rushing and receiving yards are worth 0.1 points each
#A reception is worth 1 point
#A turnover is worth -1 point

#Note: We do have some measurement error, so We won't recover those numbers exactly
#https://youtu.be/xVCeJ9dJua4?si=soIOxzWWKvYwsPYa
#https://www.cbssports.com/fantasy/football/stats/QB/2024/season/stats/ppr/
OLS_truish_model <-lm(FantasyPoints ~ PassTD+PassYD+RushTD+RushYD+RecTD+RecYD+Rec+Turnovers, data = data)
broom::tidy(OLS_truish_model)
summary(OLS_truish_model)

#the error comes from how we got the data.
#CBS didn't report Recieving numbers for QBs
#or passing numbers for WRs/TEs/RBs


stargazer::stargazer(OLS_1, OLS_2, OLS_truish_model, type="text")


