
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

df2.ed_visits_cleaned %>% 
  datatable()


# 3) plots: ------------
df2.ed_visits_cleaned %>% 
  ggplot(aes(x = date, 
             y = ed_visits)) + 
  geom_line() + 
  geom_smooth() + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95"))
      

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



            
