#This R-script contains the code for the analysis presented in our 
#Master Thesis from NHH during the fall of 2022 -
#   The NordLink Effect: Estimating NordLink's causal effect on the German/
#   Norwegian electricity price spread: a difference-in-difference approach

# Main analyis: ----------------------------------------------------------------

#Dependencies: 
library(tidyverse)
library(zoo)
library(stargazer)
library(lubridate)
library(purrr)
library(boot)
library(leaps)
library(lmtest)
library(sandwich)
library(corrplot)

#Setting the working directory: 
setwd("C:/Users/larsa/OneDrive - Norges Handelshøyskole/9. Semester/BANTHE/Appendix")

#Loading in the necessary data: 
load("Data/did_data.Rdata")
load("Data/DA_spread_abs.Rdata")

#Sourcing some functions from another script:
source("Functions/plot_prices_functions.R")


#Difference-in-Difference approach: ----
#We want to study if the NordLink-cable made the Norwegian and German 
#electricity market converge. We use the spread as a proxy for market 
#convergence. Below we have a proof-of-concept of the empirical strategy:

#Here we can see the spread for Germany and Belgium and when the NordLink-cable
#was introduced. Before the introduction of NordLink the spreads look similar.
#After NordLink the spread is much higher due to many geopolitical factors, but we 
#want to know if being in the treatment group affects the spread. 
plot_two_spread_abs("2020-01-01", spread1 = "BE", spread2 = "DE") + 
  geom_vline(xintercept = as.Date("2020-12-09"), linetype = "dotted", color = "steelblue", size = 1.5)


#To do this we rely on the "parallel trend assumption", which in our case, 
#states that Germany would follow the same trend as Belgium in the abscence 
#of the treatment (NordLink)... We need to convince the readers of our thesis
#that this assumption is valid. The common approach here is to discuss the
#pre- trend spread and potential policies that might differ in the different 
#countries after the introduction of NordLink:
plot_two_spread_abs("2020-01-01", "2020-12-09", "BE", "DE")
plot_two_prices("2020-01-01", "2020-12-09", "BE", "DE")


#Creating and looking at the models:--------------------------------------------

#We need to illustrate the importance of the difference in difference setup:
#We can create a naive model where we just include a dummy variable for the
#introduction of NordLink, using only the German spread.

#This result will tell us that the introduction of NordLink lead to a serious
#increase in the spread, but this is obviously due to other reason's.
naive_mod <- lm(Spread ~ NordLink, data = did_data %>% filter(Area == "DE"))
stargazer(naive_mod, type = "text")


#When looking at the outcomes of the simple regression we see: 
#   There is no statistical evidence for a difference between German/Belgian spread
#   On average the spread after the introduction of NordLink is 19 euros higher
#   On average NordLink reduced the German spread by 6.9 euros!

#Assuming the development of the Belgium prices is a good proxy of
#the German electricity prices in the absence of NordLink, this is
#good evidence that the German and Norwegian electricity market 
#converged due to NordLink!
simple_mod <- lm(Spread ~ NordLink + Group + Post, data = did_data)
stargazer(simple_mod, type = "text", out = "Results/Regression outputs/did_reg.txt")

#We also want to includes some control variables which hopefully increases the
#accuracy of the causal effect analysis. We can add year fixed effects, peak
#hours fixed effects and the natural gas price to hopefully control some of the
#variation not related to the introduction of NordLink:
contr_mod <- lm(Spread ~ NordLink + Group + Post + 
                  Area:Wind + Area:Solar +
                  Congestion_Norway + Congestion_Germany + 
                  water_reservs +
                  NO_Wind +
                  Area:NO_load_for + 
                  Season +
                  Hour +
                  Day, 
                data = did_data)


stargazer(contr_mod, type = "text")
stargazer(naive_mod, simple_mod, contr_mod, type = "text")

#Robustness of the models: -----------------------------------------------------

#Bootstrapped standard errors: 
#As normally distributed error terms are unlikely to hold, when working with
#day-ahead electricity prices we should get the standard deviation using 
#a bootstrapping approach:

#We need to start by creating a function that samples some data and
#estimates the effect of NordLink
calc_DID_simple <- function(data, indices){
  sample_df <- data[indices, ]
  
  model <- lm(Spread ~ NordLink + Group + Post,
              data = sample_df)
  
  return(
    model$coefficients
  )
}
calc_DID_contr  <- function(data, indices){
  sample_df <- data[indices, ]
  
  model <- lm(Spread ~ NordLink + Group + Post + 
                Area:Wind + Area:Solar +
                Congestion_Norway + Congestion_Germany + 
                water_reservs +
                NO_Wind +
                Area:NO_load_for + 
                #Area:load_for +
                Season +
                Hour +
                Day,
              data = sample_df)
  
  return(
    model$coefficients[c(1,2,3,4,7,8,41,42,43,44,45,46)]
  )
  
}

#Running the bootstrap with 5000 replications:
#This is quite computationally heavy and takes a few minutes to run...
simple_mod_boot <- boot(did_data, calc_DID_simple, R = 5000)
contr_mod_boot  <- boot(did_data, calc_DID_contr, R = 5000)

#Taking a look at the results:
#We see that the bootstrapped estimation of the effect of NordLink, is pretty
#much identical to the results used for estimating the entire data set...
#The bootstrap yields a slightly lower standard error, which should be more
#robust...
simple_mod_boot
contr_mod_boot


