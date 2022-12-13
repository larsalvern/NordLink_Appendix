#This script contains all the plots for the day ahead prices: 

#Dependencies: 
library(tidyverse)

#Loading the data:
load("Data/DA_prices.Rdata")
load("Data/DA_spread.Rdata")
load("Data/DA_prices_daily.Rdata") 
load("Data/cbf_no_de_daily.Rdata")


#functions for plotting the different prices:  
plot_two_prices <- function(startdate, enddate, price1, price2){
  DA_prices_daily %>% 
    filter(Date >= as.Date(startdate)) %>% 
    filter(Date <= as.Date(enddate)) %>% 
    filter(Area == price1 | Area == price2) %>% 
    ggplot() +
    geom_line(aes(x = Date, y = DayAhead_Price, color = Area)) + 
    scale_x_date(date_breaks = "3 months") +
    xlab("") + 
    ylab("Day Ahead Prices (EUR/Mwh)") +
    #ggtitle("Day Ahead Prices in Euro/Mwh", ) + 
    theme_bw()
}  
plot_three_prices <- function(startdate, price1, price2, price3){
  DA_prices_daily %>% 
    filter(Date >= as.Date(startdate)) %>% 
    filter(Area == price1 | Area == price2 | Area == price3) %>% 
    ggplot() +
    geom_line(aes(x = Date, y = DayAhead_Price, color = Area)) + 
    scale_x_date(date_breaks = "3 months") +
    xlab("") + 
    ylab("Day Ahead Prices (EUR/Mwh)") +
    #ggtitle("Day Ahead Prices in Euro/Mwh", ) + 
    theme_bw()
}  

#function for plotting smoothed prices:
plot_two_smooth_prices <- function(startdate, price1, price2){
  DA_prices_daily %>% 
    filter(Date >= as.Date(startdate)) %>% 
    filter(Area == price1 | Area == price2) %>% 
    ggplot() +
    geom_line(aes(x = Date, y = DayAhead_Price_Smooth, color = Area)) + 
    scale_x_date(date_breaks = "3 months") +
    xlab("") + 
    ylab("Day Ahead Prices (EUR/Mwh)") +
    #ggtitle("Day Ahead Prices in Euro/Mwh", ) + 
    theme_bw()
}
plot_three_smooth_prices <- function(startdate, price1, price2, price3){
  DA_prices_daily %>% 
    filter(Date >= as.Date(startdate)) %>% 
    filter(Area == price1 | Area == price2 | Area == price3) %>% 
    ggplot() +
    geom_line(aes(x = Date, y = DayAhead_Price_Smooth, color = Area)) + 
    scale_x_date(date_breaks = "3 months") +
    xlab("") + 
    ylab("Day Ahead Prices (EUR/Mwh)") +
    #ggtitle("Day Ahead Prices in Euro/Mwh", ) + 
    theme_bw()
}

#function for plotting spread:
plot_spread <- function(startdate, spread1){
  DA_spread %>%
    filter(Date >= as.Date(startdate)) %>% 
    filter(Area == spread1) %>% 
    group_by(Date, Area) %>% 
    summarise(Spread = Spread %>% mean()) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = Spread, color = Area)) +
    scale_x_date(date_breaks = "3 months") +
    xlab("") + 
    ylab("Spread (Absolute Difference from NO2 price - Euro / Mvh)") +
    #ggtitle("Spread from NO2 prices") + 
    theme_bw()
}
plot_two_spread <- function(startdate, enddate = "2022-09-05", spread1, spread2){
  DA_spread %>% 
    filter(Date >= as.Date(startdate)) %>% 
    filter(Date <= as.Date(enddate)) %>% 
    filter(Area == spread1 | Area == spread2) %>% 
    group_by(Date, Area) %>% 
    summarise(Spread = Spread %>% mean()) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = Spread, color = Area)) +
    scale_x_date(date_breaks = "3 months") +
    xlab("") + 
    ylab("Spread (Absolute Difference from NO2 price - Euro / Mvh)") +
    #ggtitle("Spread from NO2 prices") + 
    theme_bw()
}

plot_spread_abs <- function(startdate, spread1){
  DA_spread_abs %>%
    filter(Date >= as.Date(startdate)) %>% 
    filter(Area == spread1) %>% 
    group_by(Date, Area) %>% 
    summarise(Spread = Spread %>% mean()) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = Spread, color = Area)) +
    scale_x_date(date_breaks = "3 months") +
    xlab("") + 
    ylab("Spread (Absolute Difference from NO2 price - Euro / Mvh)") +
    #ggtitle("Spread from NO2 prices") + 
    theme_bw()
}
plot_two_spread_abs <- function(startdate, enddate = "2022-09-05", spread1, spread2){
  DA_spread_abs %>% 
    filter(Date >= as.Date(startdate)) %>% 
    filter(Date <= as.Date(enddate)) %>% 
    filter(Area == spread1 | Area == spread2) %>% 
    group_by(Date, Area) %>% 
    summarise(Spread = Spread %>% mean()) %>% 
    ggplot() + 
    geom_line(aes(x = Date, y = Spread, color = Area)) +
    scale_x_date(date_breaks = "3 months") +
    xlab("") + 
    ylab("Spread (Absolute Difference from NO2 price - Euro/Mvh)") +
    #ggtitle("Spread from NO2 prices") + 
    theme_bw()
}



plot_spread("2020-01-01", "DE_spread")
plot_two_spread("2020-01-01", "IT_spread", "DE_spread")

