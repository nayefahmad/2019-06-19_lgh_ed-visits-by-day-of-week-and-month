
#'--- 
#' title: "LGH ED visits by day of week"
#' author: "Nayef Ahmad"
#' date: "2019-06-19"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
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

#+ knitr
knitr::opts_chunk$set(dev = "png",
                      cache = TRUE)


#+ analysis 
# 1) set up database connections and import functions: -----------
source(here::here("src", 
                  "setup-denodo_function.R"))
source(here::here("src", 
                  "ed-visits-denodo_function.R"))


setup_denodo()


# 2) pull ed data: -----------
df1.ed_visits <- extract_ed_visits("20170101",  # todo: earlier start? 
                                   "201906017")

df2.ed_visits_cleaned <- 
  df1.ed_visits %>% 
  mutate(date = ymd(date_id), 
         weekday = weekdays(date), 
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
         years_from_2017 = as.factor(years_from_2017), 
         year = as.factor(year)) %>% 
  
  rename(ed_visits = value) %>% 
  
  select(date, 
         years_from_2017, 
         month, 
         year, 
         weekday, 
         ed_visits)

# str(df2.ed_visits_cleaned)

df2.ed_visits_cleaned %>% datatable()

# mean and sd: 
df3.mean_and_sd <- 
  df2.ed_visits_cleaned %>% 
  group_by(year) %>% 
  summarise(mean_visits = mean(ed_visits), 
            sd_visits = sd(ed_visits))

df3.mean_and_sd %>% datatable()


# 3) plots: ------------
# time series 
df2.ed_visits_cleaned %>% 
  ggplot(aes(x = date, 
             y = ed_visits)) + 
  geom_line() + 
  geom_smooth() + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95"))
      
# facet by year
df2.ed_visits_cleaned %>% 
  ggplot(aes(x = weekday, 
             y = ed_visits)) + 
  geom_beeswarm(alpha = .4) + 
  facet_wrap(~year) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


df2.ed_visits_cleaned %>% 
  ggplot(aes(x = weekday, 
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
  ggplot(aes(x = year, 
             y = ed_visits)) + 
  geom_beeswarm() + 
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

# checking normality: 
for (i in 0:2) {
x <- df2.ed_visits_cleaned %>% 
  filter(years_from_2017 == i) %>% 
  pull(ed_visits) 

qqnorm(x, main = paste("years from 2017 = ", i))
qqline(x, col = "red")
}


