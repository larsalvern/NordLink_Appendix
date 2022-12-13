#This R-script contains the code for the Appendix presented in our 
#Master Thesis from NHH during the fall of 2022 -
#   The NordLink Effect: Estimating NordLink's causal effect on the German/
#   Norwegian electricity price spread: a difference-in-difference approach

#Dependencies: 
library(tidyverse)
library(stargazer)
library(sandwich)
library(lmtest)
library(ggpubr)

#Loading some data: 
load("Data/did_data.Rdata")
load("Data/power_prod.Rdata")
load("Data/cbf_data.Rdata")
load("Data/renewable_forecast.Rdata")
load("Data/contr_mod.Rdata")
load("Data/simple_mod.Rdata")
load("Data/naive_mod.Rdata")
load("Data/DA_prices.Rdata")
load("Data/net_mw_df.Rdata")

#A2 Average Merit Orders - 2022 ------------------------------------------------
marginal_cost <- 
  power_prod %>%
  ungroup() %>% 
  select(Production_Type) %>% 
  distinct() %>% 
  mutate(Marginal_Cost = c(14, 14, 180, 12, 200, 390, 25, 40, 14, 6, 15, 5))

colors <- c("Waste" = "#F2F7FC", "Biomass" = "#E3EEF9", "Hydro" = "#D6E6F6",
            "Solar" = "#B7D3EF", "Wind" = "#6FA7DF", "Nuclear" = "#5094D8",
            "Coal" = "#1F5489", "Gas" = "#163A5E")
colors_no <- c("Hydro" = "#D6E6F6", "Wind" = "#6FA7DF", "Gas" = "#163A5E")


NO_meritorder_plot <- 
  power_prod %>% 
  rename(Area = Area_agg) %>%
  filter(Date >= as.Date("2022-01-01")) %>% 
  group_by(Area, Production_Type) %>% 
  summarise(MW = MW %>% sum()) %>% 
  mutate(Percent = MW / sum(MW)) %>% 
  left_join(marginal_cost) %>% 
  filter(Area == "NO") %>% 
  mutate(Production_Type = Production_Type %>% fct_reorder(Marginal_Cost)) %>% 
  filter(Production_Type != "Other") %>% 
  ggplot(aes(x = 1, y = Marginal_Cost, width = Percent)) + 
  geom_bar(aes(fill = Production_Type), color = "black", stat = "identity") +
  facet_grid(~Production_Type, scales = 'free_x', space = 'free') +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = colors_no, name = "Production Type") + 
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  labs(x = "Production Type", y = "Marginal Production Cost") + 
  ggtitle("")

DE_meritorder_plot <- 
  power_prod %>% 
  rename(Area = Area_agg) %>%
  filter(Date >= as.Date("2022-01-01")) %>% 
  group_by(Area, Production_Type) %>% 
  summarise(MW = MW %>% sum()) %>% 
  mutate(Percent = MW / sum(MW)) %>% 
  filter(Percent >= 0.005) %>% 
  left_join(marginal_cost) %>% 
  filter(Area == "DE") %>% 
  mutate(Production_Type = Production_Type %>% fct_reorder(Marginal_Cost)) %>% 
  filter(Production_Type != "Other" & Production_Type != "Oil") %>% 
  ggplot(aes(x = 1, y = Marginal_Cost, width = Percent)) + 
  geom_bar(aes(fill = Production_Type), color = "black", stat = "identity") +
  facet_grid(~Production_Type, scales = 'free_x', space = 'free') +
  scale_x_continuous(expand = c(0, 0)) +
  scale_fill_manual(values = colors, name = "Production Type") + 
  theme_classic()+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        panel.spacing = unit(0, "lines"),
        strip.background = element_blank(),
        strip.text.x = element_blank()) + 
  labs(x = "Production Type", y = "Marginal Production Cost") + 
  ggtitle("")

DE_meritorder_plot
NO_meritorder_plot

#A5 Norwegian Wind Production: -------------------------------------------------
#Checking the relation between CBF and Norwegian Wind
flow_wind_df <- 
  cbf_data %>% filter(Border == "NO-DE") %>% 
  left_join(renewable_forecast %>% 
              filter(Area == "DE" | Area == "NO") %>% 
              select(Datetime, Area, Wind) %>% 
              pivot_wider(names_from = Area, values_from = Wind)) %>% 
  na.omit()

#We can confirm that Norwegian Wind increases Norwegian exports! Or atleast pushes
#for more, by decreasing imports!
stargazer(lm(Flow ~ DE + NO, data = flow_wind_df), type = "text")

#Now we want to check if it leads to Congestion?
stargazer(lm(Congestion_Germany ~ NO_Wind, data = did_data %>% filter(Area == "DE")), type = "text")

#Combine:
flow_wind_mod <- lm(Flow ~ NO + DE, data = flow_wind_df)
cong_wind_mod <- lm(Congestion_Germany ~ NO, data = did_data %>% filter(Area == "DE") %>% rename(NO = NO_Wind))

stargazer(flow_wind_mod, cong_wind_mod, type = "text")

#A6 Recurring Effect Dummy Variables - Results ---------------------------------
stargazer(contr_mod, type = "text")

#A7 Robustness Check: ----------------------------------------------------------

#Here we present the three models used for checking the robustness of the result:
log_did_data <-
  DA_prices %>% 
  filter(Date >= as.Date("2020-01-01")) %>% 
  filter(Area == "NO2" | Area == "DE" | Area == "BE") %>% 
  pivot_wider(names_from = Area, values_from = DayAhead_Price) %>% 
  filter(DE > 0 & NO2 > 0 & BE > 0) %>% 
  mutate(NO2 = NO2 %>% log(),
         DE  = DE  %>% log(),
         BE  = BE  %>% log()) %>% 
  mutate(DE_spread = DE - NO2,
         BE_spread = BE - NO2) %>% 
  select(-NO2, -DE, -BE, -Currency, -Time) %>% 
  pivot_longer(cols = c(DE_spread, BE_spread), names_to = "Area", values_to = "log_Spread") %>% 
  mutate(Area = str_split_fixed(Area, n = 2, pattern = "_")[,1]) %>% 
  left_join(did_data %>% select(-Spread))

#Another version of the log transformation, not quite sure is more correct:
log_did_data2 <-
  DA_prices %>% 
  filter(Date >= as.Date("2020-01-01")) %>% 
  filter(Area == "NO2" | Area == "DE" | Area == "BE") %>% 
  pivot_wider(names_from = Area, values_from = DayAhead_Price) %>% 
  mutate(DE_spread = log(abs(DE - NO2)),
         BE_spread = log(abs(BE - NO2))) %>% 
  select(-NO2, -DE, -BE, -Currency, -Time) %>% 
  pivot_longer(cols = c(DE_spread, BE_spread), names_to = "Area", values_to = "log_Spread2") %>% 
  mutate(Area = str_split_fixed(Area, n = 2, pattern = "_")[,1]) %>% 
  left_join(did_data %>% select(-Spread)) %>% 
  mutate(log_Spread2 = ifelse(is.finite(log_Spread2), log_Spread2, NA))


#Doing the same analysis for percentage difference in prices:
percent_did_data <- 
  DA_prices %>% 
  filter(Date >= as.Date("2020-01-01")) %>% 
  filter(Area == "NO2" | Area == "DE" | Area == "BE") %>% 
  pivot_wider(names_from = Area, values_from = DayAhead_Price) %>% 
  filter(DE > 0 & NO2 > 0 & BE > 0) %>% 
  mutate(DE_spread = (DE-NO2)/NO2,
         BE_spread = (BE-NO2)/NO2) %>% 
  select(-NO2, -DE, -BE, -Currency, -Time) %>% 
  pivot_longer(cols = c(DE_spread, BE_spread), names_to = "Area", values_to = "percent_Spread") %>% 
  mutate(Area = str_split_fixed(Area, n = 2, pattern = "_")[,1]) %>% 
  filter(percent_Spread <= 50) %>% 
  left_join(did_data %>% select(-Spread))

#Creating the models:
log_did_mod <- lm(log_Spread ~ NordLink + Group + Post + 
                    Area:Wind + Area:Solar +
                    Congestion_Norway + Congestion_Germany + 
                    water_reservs +
                    NO_Wind + 
                    Area:NO_load_for + 
                    Season + 
                    Hour + 
                    Day,
                  data = log_did_data)

log_did_mod2 <- lm(log_Spread2 ~ NordLink + Group + Post + 
                     Area:Wind + Area:Solar +
                     Congestion_Norway + Congestion_Germany + 
                     water_reservs +
                     NO_Wind + 
                     Area:NO_load_for + 
                     Season + 
                     Hour + 
                     Day,
                   data = log_did_data2)

percent_mod <- lm(percent_Spread ~ NordLink + Group + Post +
                    Area:Wind + Area:Solar +
                    Congestion_Norway + Congestion_Germany + 
                    water_reservs +
                    NO_Wind +
                    Area:NO_load_for + 
                    Season +
                    Hour +
                    Day,
                  data = percent_did_data)

#Plotting the transformed data:
log_did_data  %>% ggplot()+geom_line(aes(x = Datetime, y = log_Spread,  color = Area)) + theme_bw()
log_did_data2 %>% ggplot()+geom_line(aes(x = Datetime, y = log_Spread2, color = Area)) + theme_bw()
percent_did_data %>% ggplot() + geom_line(aes(x = Datetime, y = percent_Spread, color = Area)) + theme_bw()


stargazer(log_did_mod, log_did_mod2, percent_mod, type = "text")

#A 7.1 Removing Control Variables:
#We also want to present evidence that the model does not change significantly 
#when removing different control variables: 
model1 <- lm(Spread ~ NordLink + Group + Post + Area:Solar + Congestion_Norway + Congestion_Germany + water_reservs + NO_Wind + Area:NO_load_for + Season + Hour + Day, data = did_data)
model2 <- lm(Spread ~ NordLink + Group + Post + Area:Wind + Congestion_Norway + Congestion_Germany + water_reservs + NO_Wind + Area:NO_load_for + Season + Hour + Day, data = did_data)
model3 <- lm(Spread ~ NordLink + Group + Post + Area:Wind + Area:Solar + Congestion_Norway + Congestion_Germany + NO_Wind + Area:NO_load_for + Season + Hour + Day, data = did_data)
model4 <- lm(Spread ~ NordLink + Group + Post + Area:Wind + Area:Solar + Congestion_Norway + Congestion_Germany + water_reservs + Area:NO_load_for + Season + Hour + Day, data = did_data)
model5 <- lm(Spread ~ NordLink + Group + Post + Area:Wind + Area:Solar + Congestion_Norway + Congestion_Germany + water_reservs + NO_Wind + Season + Hour + Day, data = did_data)
model6 <- lm(Spread ~ NordLink + Group + Post + Area:Wind + Area:Solar + Congestion_Norway + Congestion_Germany + water_reservs + NO_Wind + Area:NO_load_for + Season + Hour + Day, data = did_data)

stargazer(model1, model2, model3, model4, model5, type = "text")

#A 7.2 Correlation Matrix - Control Variables: 
correlation <- cor(did_data %>% select(Wind, 
                                       Solar, 
                                       Congestion_Norway, 
                                       Congestion_Germany, 
                                       water_reservs, 
                                       NO_Wind,
                                       NO_load_for)) %>% round(2)


correlation %>% stargazer(type = "text")

#A 7.3 Newey White standard Errors: 
#Robust standard errors: 
#We first present Newey-White Standard errors for all the models:

#Takes a while to run...
naive_mod_robust  <- coeftest(naive_mod, vcovHAC(naive_mod, type = "HC0"))
simple_mod_robust <- coeftest(simple_mod, vcovHAC(simple_mod, type = "HC0"))
contr_mod_robust  <- coeftest(contr_mod, vcovHAC(contr_mod, type = "HC0"))

naive_mod_robust
simple_mod_robust
contr_mod_robust

#A8 Non Energy Crisis Results: -------------------------------------------------

naive_mod_non_crisis <- lm(Spread ~ NordLink, data = did_data %>% filter(Area == "DE", Date <= as.Date("2021-10-01"))) 
stargazer(naive_mod_non_crisis, type = "text")

#With the basic difference in difference setup we see the effect of NordLink is 
#substantially less when looking at the data before the energy crisis!
simple_mod_non_crisis <- lm(Spread ~ NordLink + Group + Post, data = did_data %>% filter(Date <= as.Date("2021-10-01")))
stargazer(simple_mod_non_crisis, type = "text")

#We see similar results when including control variables, the effect of NordLink
#seems to increase when controlling for different factors that might affect the 
#spread. 
contr_mod_non_crisis <- lm(Spread ~ NordLink + Group + Post + 
                             Area:Wind + Area:Solar +
                             Congestion_Norway + Congestion_Germany + 
                             water_reservs +
                             NO_Wind +
                             Area:NO_load_for + 
                             Season +
                             Hour +
                             Day, 
                           data = did_data %>% filter(Date <= as.Date("2021-10-01")))

stargazer(contr_mod_non_crisis, type = "text")


#All models together:
stargazer(naive_mod_non_crisis, 
          simple_mod_non_crisis,
          contr_mod_non_crisis, type = "text")

#A9 Load and Power Production: -------------------------------------------------
day_colors <- c("Monday" = "#B7D3EF", "Tuesday" = "#B7D3EF", "Wednesday" = "#B7D3EF",
                "Thursday" = "#B7D3EF", "Friday" = "#B7D3EF", "Saturday" = "#2D7BC9", "Sunday" = "#163A5E")
load_NO_plot <- 
  net_mw_df %>%
  ggplot() + 
  geom_line(aes(x = Time, y = Load_NO, color = Weekday), size = 1) + 
  ggtitle("Norway") + 
  ylab("Load Forecsat [MW]") + 
  scale_y_continuous(breaks = c(9000, 10000, 11000)) +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  xlab("Hour of Day") + 
  scale_color_manual(values = day_colors) +
  theme_bw()

load_DE_plot <- 
  net_mw_df %>%
  ggplot() + 
  geom_line(aes(x = Time, y = Load_DE, color = Weekday), size = 1) + 
  ggtitle("Germany") + 
  ylab("Load Forecsat [MW]") + 
  xlab("Hour of Day") + 
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_y_continuous(breaks = c(45000, 55000, 65000)) +
  scale_color_manual(values = day_colors) +
  theme_bw()

NO_power_plot <- net_mw_df %>% 
  ggplot() + 
  geom_line(aes(x = Time, y = NO_power_prod, color = Weekday), size = 1) + 
  ylab("Power Production [MW]") + 
  xlab("Hour of Day") + 
  ggtitle("Norway") + 
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_y_continuous(breaks = c(9000, 11000, 13000)) +
  scale_color_manual(values = day_colors) +
  theme_bw()

DE_power_plot <- net_mw_df %>% 
  ggplot() + 
  geom_line(aes(x = Time, y = DE_power_prod, color = Weekday), size = 1) + 
  ylab("Power Production [MW]") + 
  xlab("Hour of Day") + 
  ggtitle("Germany") + 
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_color_manual(values = day_colors) +
  theme_bw()


ggarrange(load_NO_plot, load_DE_plot, common.legend = TRUE, legend = "right", nrow = 2)
ggarrange(NO_power_plot, DE_power_plot, common.legend = TRUE, legend = "right", nrow = 2)



