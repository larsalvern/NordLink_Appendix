#This R-script contains the code for plots presented in our 
#Master Thesis from NHH during the fall of 2022 -
#   The NordLink Effect: Estimating NordLink's causal effect on the German/
#   Norwegian electricity price spread: a difference-in-difference approach

#Dependencies:
library(tidyverse)
library(lubridate)
library(gridExtra)
library(ggpubr)
library(readxl)

#Setting the working directory:
setwd("C:/Users/larsa/OneDrive - Norges Handelshøyskole/9. Semester/BANTHE/Appendix")

#Loading necessary data: 
load("Data/did_data.Rdata")
load("Data/fyllingsgrad.Rdata")
load("Data/power_prod.Rdata")
load("Data/DA_spread_abs.Rdata")
load("Data/contr_mod.Rdata")
load("Data/load_data.Rdata")
load("Data/net_mw_df.Rdata")
load("Data/cbf_data.Rdata")

#Sourcing some function from another script:
source("Functions/plot_prices_functions.R")

#Chapter 3: Background ---------------------------------------------------------

#3.2.2 Weather Induced Challenges: 
filling_degree_plot <- 
  fyllingsgrad %>%
  rename('2015-2020' = '2015-2020 avg') %>%
  rename(Week = ...1) %>% 
  slice(-1) %>% 
  slice(-53) %>% 
  pivot_longer(cols = c('2015-2020', '2022', '2021'), names_to = "Year") %>% 
  mutate(Year = factor(Year, levels = c('2015-2020', '2021', '2022'))) %>% 
  filter(value > 0) %>% 
  mutate(value = value * 100) %>% 
  ggplot() + 
  geom_line(aes(x = Week, y = value, color = Year), size = 1) + 
  ylab("Filling degree [%]") + 
  scale_x_continuous(breaks = seq(0, 50, by = 5)) + 
  scale_color_manual(values = c("steelblue", "orangered3", "darkgreen")) +
  ylim(0, 100) + 
  theme_bw()

filling_degree_plot

#3.3.1 Supply and Demand in the Power Market:
marginal_cost <- 
  power_prod %>%
  ungroup() %>% 
  select(Production_Type) %>% 
  distinct() %>% 
  mutate(Marginal_Cost = c(14, 14, 180, 12, 200, 390, 25, 40, 14, 6, 15, 5))

colors <- c("Waste" = "#F2F7FC", "Biomass" = "#E3EEF9", "Hydro" = "#D6E6F6",
            "Solar" = "#B7D3EF", "Wind" = "#6FA7DF", "Nuclear" = "#5094D8",
            "Coal" = "#1F5489", "Gas" = "#163A5E")

high_RES_meritorder_plot <- 
  power_prod %>% 
  rename(Area = Area_agg) %>%
  filter(Date == as.Date("2021-03-12") & Time == "12:00") %>% 
  group_by(Area, Production_Type) %>% 
  summarise(MW = MW %>% sum()) %>% 
  mutate(Percent = MW / sum(MW)) %>% 
  filter(Percent >= 0.005) %>% 
  left_join(marginal_cost) %>% 
  filter(Area == "DE") %>% 
  mutate(Production_Type = Production_Type %>% fct_reorder(Marginal_Cost)) %>%
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
  ggtitle("High Renewable Production")

low_RES_meritorder_plot <- 
  power_prod %>% 
  rename(Area = Area_agg) %>%
  filter(Date == as.Date("2022-09-04") & Time == "23:00") %>% 
  group_by(Area, Production_Type) %>% 
  summarise(MW = MW %>% sum()) %>% 
  mutate(Percent = MW / sum(MW)) %>% 
  filter(Percent >= 0.005) %>% 
  left_join(marginal_cost) %>% 
  filter(Area == "DE") %>% 
  mutate(Production_Type = Production_Type %>% fct_reorder(Marginal_Cost)) %>% 
  filter(Production_Type != "Other") %>% 
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
  labs(x = "Production Type", y = "") + 
  ggtitle("Low Renewable Production")

ggarrange(high_RES_meritorder_plot, low_RES_meritorder_plot, common.legend = TRUE, legend = "right", ncol = 2)

#Chapter 4: Data ---------------------------------------------------------------

#4.3 Descriptive Spread Statistics
DE_spread_plot <- 
  plot_spread_abs("2020-01-01", "DE") +
  ylab("Absolute Difference from NO2 price") + 
  geom_vline(xintercept = as.Date("2020-12-09"), linetype = "dotted", color = "steelblue", size = 1.5) +
  ylab("Absolute difference from NO2 price") + 
  scale_x_date(date_breaks = "6 months") +
  scale_color_manual(values = c("steelblue"))

DA_prices_plot <- 
  plot_two_prices("2020-01-01", "2022-12-31", "DE", "NO2") +
  geom_vline(xintercept = as.Date("2020-12-09"), linetype = "dotted", color = "steelblue", size = 1.5) +
  scale_x_date(date_breaks = "6 months") +
  ylab("Day-ahead prices (EUR per MWh)") +
  scale_color_manual(values = c("steelblue", "orangered3"))

DE_spread_plot
DA_prices_plot

#4.3.1 Distribution:
spread_dist_plot <-
  DA_spread_abs %>% 
  filter(Date >= as.Date("2020-01-01")) %>% 
  filter(Area == "DE") %>% 
  ggplot() + 
  geom_histogram(aes(x = Spread), color = "black", fill = "steelblue", binwidth = 10) +
  ylab("") + 
  theme_bw()

spread_dist_plot

#4.3.2 Seasonality:
Average_prices_plot <- 
  DA_prices %>%
  filter(Area == "DE" | Area == "NO2") %>% 
  group_by(Time, Area) %>% 
  summarise(Price = DayAhead_Price %>% mean(na.rm = T)) %>% 
  mutate(Time = str_split_fixed(Time, pattern = ":", n = 2)[,1] %>% as.numeric()) %>% 
  ggplot() + 
  geom_line(aes(x = Time, y = Price, color = Area), size = 1) + 
  ggtitle("Average day-ahead prices") +
  ylab("Price [EUR per MWh]") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) + 
  scale_color_manual(values = c("steelblue", "orangered3")) +
  ylim(0, 120) + 
  theme_bw()

Average_spread_plot <- 
  DA_spread_abs %>% 
  filter(Area == "DE") %>%
  group_by(Time) %>% 
  summarise(Spread = Spread %>% mean(na.rm = T)) %>% 
  mutate(Time = str_split_fixed(Time, pattern = ":", n = 2)[,1] %>% as.numeric()) %>% 
  ggplot() + 
  geom_line(aes(x = Time, y = Spread), size = 1, color = "steelblue") + 
  ggtitle("Average spread") +
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  ylab("Spread [EUR per MWh]") + 
  scale_color_brewer(palette = "Dark2") +
  ylim(0, 120) + 
  theme_bw()

grid.arrange(Average_prices_plot, Average_spread_plot, ncol = 2)


weekday_end_plot <- 
  DA_spread %>% 
  filter(Area == "DE") %>% 
  mutate(Day = wday(Datetime),
         Day = Day %>% as.character(),
         Day = recode(Day, "1" = "Sun", "2" = "Mon", "3" = "Tue",
                      "4" = "Wed", "5" = "Thu", "6" = "Fri",
                      "7" = "Sat")) %>% 
  mutate(Day = factor(Day, levels = c("Mon", "Tue", "Wed", "Thu",
                                      "Fri", "Sat", "Sun"))) %>% 
  mutate(Weekend = ifelse(Day == "Sat" | Day == "Sun", "Weekend", "Weekday")) %>% 
  filter(Weekend == "Weekend") %>% 
  bind_rows(DA_spread %>% 
              filter(Area == "DE") %>% 
              mutate(Day = wday(Datetime),
                     Day = Day %>% as.character(),
                     Day = recode(Day, "1" = "Sun", "2" = "Mon", "3" = "Tue",
                                  "4" = "Wed", "5" = "Thu", "6" = "Fri",
                                  "7" = "Sat")) %>% 
              mutate(Day = factor(Day, levels = c("Mon", "Tue", "Wed", "Thu",
                                                  "Fri", "Sat", "Sun"))) %>% 
              mutate(Weekend = ifelse(Day == "Sat" | Day == "Sun", "Weekend", "Weekday")) %>% 
              filter(Weekend == "Weekday") %>% 
              slice_sample(n = 9260)) %>% 
  filter(Spread <= 200 & Spread >= -200) %>% 
  ggplot() + 
  geom_histogram(aes(x = Spread), color = "black", fill = "steelblue", bins = 50) + 
  facet_grid(cols = vars(Weekend)) +
  ylab("") + 
  theme_bw()

weekday_end_plot

#Chapter 5: Methodology: -------------------------------------------------------

#5.3 Parallel Trend Assumption:
parallel_prices_plot <- 
  DA_prices_daily %>% 
  filter(Date >= as.Date("2020-01-01")) %>% 
  filter(Date <= as.Date("2020-12-09")) %>% 
  filter(Area == "DE" | Area == "BE") %>% 
  ggplot() +
  geom_line(aes(x = Date, y = DayAhead_Price, color = Area)) + 
  scale_color_manual(values = c("orangered3", "steelblue")) +
  scale_x_date(date_breaks = "3 months") +
  xlab("") + 
  ylab("Day-ahead prices (EUR per Mwh)") +
  theme_bw()

parallel_prices_plot

#Chapter 6: Results ------------------------------------------------------------

#6.2.2 Standard Error Issues:
residuals_plot <- 
  did_data %>%
  select(Datetime) %>% 
  mutate(residuals = contr_mod$residuals) %>% 
  ggplot(aes(x = Datetime, y = residuals)) +
  geom_point(size = 0.3, alpha = 0.2, color = "steelblue") + 
  ylab("Residual") + 
  xlab("Date") +
  scale_x_datetime(date_breaks = "6 months") +
  theme_bw()

residuals_plot

#Chapter 7: Discussion ---------------------------------------------------------

#7.3.2 Net Export Need:
day_colors <- c("Monday" = "#B7D3EF", "Tuesday" = "#B7D3EF", "Wednesday" = "#B7D3EF",
                "Thursday" = "#B7D3EF", "Friday" = "#B7D3EF", "Saturday" = "#2D7BC9", "Sunday" = "#163A5E")

net_mw_NO <- 
  net_mw_df %>% 
  ggplot() + 
  geom_line(aes(x = Time, y = Net_MW_NO, color = Weekday), size = 1) + 
  ylab("Net MW") + 
  xlab("Hour of Day") + 
  ggtitle("Norway") + 
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_color_manual(values = day_colors) +
  geom_hline(yintercept = 0) + 
  theme_bw()

net_mw_DE <- 
  net_mw_df %>% 
  ggplot() + 
  geom_line(aes(x = Time, y = Net_MW_DE, color = Weekday), size = 1) + 
  ylab("Net MW") + 
  xlab("Hour of Day") + 
  ggtitle("Germany") + 
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  scale_y_continuous(breaks = c(-5000, 0, 5000)) +
  scale_color_manual(values = day_colors) + 
  geom_hline(yintercept = 0) +
  theme_bw()

ggarrange(net_mw_NO, net_mw_DE, common.legend = TRUE, legend = "right", nrow = 2)

#7.2.4 NordLink Flow and Market Convergence: 

#Here we include the creatin of the data to make it easier to understand 
#how we construct the plot:
coef_vector <- c()

#We loop over the 24 hours in the day and use only those hours in the model 
#estimation. That way we get 24 coefficients for each hour of the day:
for (hour in unique(did_data$Hour)) {
  model <- lm(Spread ~ NordLink + Group + Post + 
                Area:Wind + Area:Solar +
                Congestion_Norway + Congestion_Germany + 
                water_reservs +
                NO_Wind +
                Area:NO_load_for + 
                Season,
              data = did_data %>% filter(Hour == hour))
  
  coef_vector <- append(coef_vector, model$coefficients[2] %>% as.numeric())
}

coef_data <- 
  data.frame(Hour = unique(did_data$Hour), Coef = coef_vector) %>% 
  mutate(Hour = str_split_fixed(Hour, pattern = ":", n = 2)[,1],
         Hour = Hour %>% as.numeric()) %>% 
  mutate(Coef = Coef %>% abs())

coef_plot <- 
  coef_data %>% 
  ggplot() + 
  geom_line(aes(x = Hour, y = Coef), size = 1, color = "steelblue") +
  ylim(0, 20) +
  ylab("Spread reduction [EUR per MWh]") + 
  scale_x_continuous(breaks = seq(0, 23, by = 2)) +
  theme_bw()

coef_plot

#7.2.5 Electricity Markets Imperfections:
flow_spread_plot <- 
  DA_spread %>% 
  filter(Area == "DE") %>% 
  select(-Currency, Area) %>% 
  left_join(cbf_data %>% filter(Border == "NO-DE") %>% select(-Export, -Import)) %>% 
  na.omit() %>% 
  ggplot(aes(x = Flow, y = Spread)) +
  geom_point(size = 0.3, alpha = 0.3, color = "steelblue") + 
  scale_x_continuous(breaks = c(-1400, - 700, 0, 700, 1400)) + 
  #ggtitle("Relationship between flow in NordLink and spread") + 
  ylab("Raw spread [EUR per MWh]") +
  xlab("Flow in NordLink [MW]") +
  theme_bw()

flow_spread_plot
