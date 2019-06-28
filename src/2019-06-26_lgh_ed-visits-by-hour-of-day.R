
#'--- 
#' title: "LGH ED visits by hour of day"
#' author: "Nayef Ahmad"
#' date: "2019-06-26"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---
#' 

#+ libraries, message = FALSE 
library(tidyverse)
library(here)
library(odbc)
library(DBI)
library(lubridate)
library(ggbeeswarm)
library(DT)
library(broom)
library(caret)
library(kableExtra)
library(scales)
library(plotly)

#+ knitr
knitr::opts_chunk$set(dev = "png",
                      cache = TRUE)


#+ analysis 
# 1) set up database connections and import functions: -----------
source(here::here("src", 
                  "setup-denodo_function.R"))
source(here::here("src", 
                  "ed-visits-by-hour-denodo_function.R"))


setup_denodo()

#' ## Data 

# 2) pull ed data: -----------
df1.ed_visits <- extract_ed_visits_by_hour("20170101",  # todo: earlier start?
                                           "20190617")

df2.ed_visits_cleaned <- 
  df1.ed_visits %>% 
  mutate(weekday = weekdays(date), 
         month = month(date), 
         year = year(date), 
         years_from_2017 = year - 2017) %>% 
  
  # fiddle with factors: 
  mutate(weekday = fct_relevel(weekday, 
                               levels = c("Monday", 
                                          "Tuesday", 
                                          "Wednesday", 
                                          "Thursday", 
                                          "Friday",
                                          "Saturday", 
                                          "Sunday")), 
         # years_from_2017 = as.factor(years_from_2017), 
         year = as.factor(year), 
         month = as.factor(month), 
         interval_1_hour_at_start_date = as.factor(interval_1_hour_at_start_date)) %>% 
  
  rename(ed_visits = value, 
         hour = interval_1_hour_at_start_date) %>% 
  
  mutate(lag_ed_visits = lag(ed_visits), 
         lag2_ed_visits = lag(ed_visits, 2), 
         lag3_ed_visits = lag(ed_visits, 3), 
         lag4_ed_visits = lag(ed_visits, 4), 
         lag5_ed_visits = lag(ed_visits, 5),
         lag6_ed_visits = lag(ed_visits, 6)) %>% 
  
  select(date, 
         hour, 
         years_from_2017, 
         month, 
         year, 
         weekday, 
         ed_visits, 
         lag_ed_visits, 
         lag2_ed_visits, 
         lag3_ed_visits, 
         lag4_ed_visits, 
         lag5_ed_visits, 
         lag6_ed_visits)

str(df2.ed_visits_cleaned)

df2.ed_visits_cleaned %>% datatable()

# mean and sd: 
df3.mean_and_sd <- 
  df2.ed_visits_cleaned %>% 
  group_by(hour) %>% 
  summarise(mean_visits = mean(ed_visits, 
                               na.rm = TRUE), 
            sd_visits = sd(ed_visits, 
                           na.rm = TRUE))

df3.mean_and_sd %>% 
  datatable() %>% 
  formatRound(2:3, 1)

#' \  
#' \  
#' \  

#' 
#' ## Exploratory plots

# 3) plots: ------------
# facet by year
df2.ed_visits_cleaned %>% 
  ggplot(aes(x = hour, 
             y = ed_visits)) + 
  geom_boxplot() + 
  facet_wrap(~year) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))

# facet by weekday
df2.ed_visits_cleaned %>% 
  ggplot(aes(x = hour, 
             y = ed_visits)) + 
  geom_boxplot() + 
  facet_wrap(~weekday) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


df2.ed_visits_cleaned %>% 
  ggplot(aes(x = year, 
             y = ed_visits)) + 
  geom_boxplot() + 
  facet_wrap(~weekday) + 
  labs(title = "hourly arrival rate by year") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


# density by year: 
df2.ed_visits_cleaned %>% 
  ggplot(aes(x = ed_visits)) + 
  geom_density() + 
  facet_wrap(~year) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))



#' ## Notes - distribution of response variable 
#'
#' Unfortunately, this is very non-normal.
#'
#' **Hypothesis**: this is a mixture of two normal distributions: one for
#' "late-night" rates, one for rest of the day (todo:). 
#' 








#' ## Variation by day of week
#' 
#' 

# set colours for days of week 
x <- seq(0, 1, length.out = 7)
cols <- seq_gradient_pal(low = "blue", 
                         high = "red")(x)

# hour of day, split by day of week 
p <- 
  df2.ed_visits_cleaned %>% 
  group_by(hour, 
           weekday) %>% 
  summarize(mean_visits = mean(ed_visits, na.rm = T)) %>% 
  ggplot(aes(x = hour, 
             y = mean_visits, 
             group = weekday, 
             col = weekday)) + 
  geom_line() + 
  scale_color_manual(values = cols) +
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1)); p

# ggplotly(p)

df2.ed_visits_cleaned %>% 
  filter(year %in% c("2018")) %>% 
  ggplot(aes(x = hour, 
             y = ed_visits)) + 
  geom_boxplot() + 
  facet_wrap(~weekday) + 
  labs(title = "2018") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))
  

# simple average by hour of day 
df2.ed_visits_cleaned %>% 
  group_by(hour) %>% 
  summarise(mean_visits = mean(ed_visits, 
                               na.rm = TRUE)) %>% 
  
  ggplot(aes(x = hour, 
             y = mean_visits)) + 
  geom_point(size = 5, 
             col = "dodgerblue4") + 
  labs(title = "2018") + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1)
        )
      





# avg ED visits by weekday AND month
# this is the type of plot that I am arguing against - it's all noise

df2.ed_visits_cleaned %>% 
  filter(year == "2018") %>% 
  group_by(hour, 
           weekday) %>% 
  summarise(mean_visits = mean(ed_visits, 
                               na.rm = TRUE)) %>% 
  
  ggplot(aes(x = hour, 
             y = mean_visits)) + 
  geom_col(fill = "dodgerblue4") + 
  facet_wrap(~weekday) + 
  
  labs(title = "Is there really any point in looking at graphs like this?") + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95"), 
      axis.text.x = element_text(angle = 45, 
                                 hjust = 1))
    
  




# 4) regression model 1: ----

#' ## Regression models 

#+ models

