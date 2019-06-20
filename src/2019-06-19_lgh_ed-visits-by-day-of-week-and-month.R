
#'--- 
#' title: "LGH ED visits by day of week"
#' author: "Nayef Ahmad"
#' date: "2019-06-19"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
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

#' ## Data 

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
         year = as.factor(year), 
         month = as.factor(month)) %>% 
  
  rename(ed_visits = value) %>% 
  
  mutate(lag_ed_visits = lag(ed_visits)) %>% 
  
  select(date, 
         years_from_2017, 
         month, 
         year, 
         weekday, 
         ed_visits, 
         lag_ed_visits)

# str(df2.ed_visits_cleaned)

df2.ed_visits_cleaned %>% datatable()

# mean and sd: 
df3.mean_and_sd <- 
  df2.ed_visits_cleaned %>% 
  group_by(year) %>% 
  summarise(mean_visits = mean(ed_visits), 
            sd_visits = sd(ed_visits))

df3.mean_and_sd %>% 
  datatable() %>% 
  formatRound(2:3, 2)

#' \  
#' \  
#' \  

#' ## Exploratory plots

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


# variation by month: 
df2.ed_visits_cleaned %>% 
  filter(weekday == "Monday", 
         year %in% c("2017")) %>% 
  ggplot(aes(x = weekday, 
             y = ed_visits)) + 
  geom_beeswarm() + 
  facet_wrap(~month) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))
  


# 4) regression model 1: ----

#' ## Regression models 

#+ models

#' ### With interaction between month and weekday 
#' 
set.seed(121)
v1_train_index <- createDataPartition(df2.ed_visits_cleaned$ed_visits, 
                                      p = 0.8, 
                                      list = FALSE)

m1 <- lm(ed_visits ~ years_from_2017 + weekday + month + lag_ed_visits + weekday:month, 
         data = df2.ed_visits_cleaned[v1_train_index, ])

summary(m1)


par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))


# glance(m1) 
# tidy(m1)
# augment(m1) # %>% names
# predict(m1, interval = "prediction")

m1.train_rmse <- sqrt(mean(resid(m1)^2))



# test set performance: 
df4.predictions <- 
  data.frame(ed_visits = df2.ed_visits_cleaned[-v1_train_index, 6], 
             predicted = predict(m1, 
                                 newdata = df2.ed_visits_cleaned[-v1_train_index, ])) 

m1.test_rmse <- sqrt(mean((df4.predictions$predicted - df4.predictions$ed_visits)^2, 
                          na.rm = TRUE))



# 5) regression model 2: ----

#' ### Without interaction between month and weekday 
#' 
set.seed(121)
v1_train_index <- createDataPartition(df2.ed_visits_cleaned$ed_visits, 
                                      p = 0.8, 
                                      list = FALSE)

m2 <- lm(ed_visits ~ years_from_2017 + weekday + month + lag_ed_visits, 
         data = df2.ed_visits_cleaned[v1_train_index, ])

summary(m2)


par(mfrow = c(2,2))
plot(m2)
par(mfrow = c(1,1))


# glance(m2) 
# tidy(m2)
# augment(m2) # %>% names
# predict(m2, interval = "prediction")

m2.train_rmse <- sqrt(mean(resid(m2)^2))



# test set performance: 
df4.predictions <- 
  data.frame(ed_visits = df2.ed_visits_cleaned[-v1_train_index, 6], 
             predicted = predict(m2, 
                                 newdata = df2.ed_visits_cleaned[-v1_train_index, ])) 

m2.test_rmse <- sqrt(mean((df4.predictions$predicted - df4.predictions$ed_visits)^2, 
                          na.rm = TRUE))


#' ## Summary of models

df5.model.performance <- 
  data.frame(model = c("year + month + weekday + month:weekday", 
                       "year + month + weekday + month:weekday", 
                       "year + month + weekday", 
                       "year + month + weekday"), 
             metric = rep(c("Train RMSE", 
                            "Test RMSE"), 2), 
             value = c(m1.train_rmse, 
                       m1.test_rmse, 
                       m2.train_rmse, 
                       m2.test_rmse)) 


df5.model.performance %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
              "condensed", 
              "responsive"))

#' \  
#' \  
#' \                


#' ## Model selection notes

#' Including month, weekday *and* year is very likely to overfit - there's just
#' 4 data points per cell!!
#'
#' The general strategy to prevent overfitting is, of course, cross-validation
#' or a train/test split


#' \  
#' \  
#' \  


# 6) train model 2 on full dataset: -----------

#' ## Train selected model on full dataset

m3.full_dataset <- lm(ed_visits ~ years_from_2017 + weekday + month + lag_ed_visits, 
                      data = df2.ed_visits_cleaned)

summary(m3.full_dataset)

glance(m3.full_dataset) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
              "condensed", 
              "responsive"))

tidy(m3.full_dataset) %>% 
  mutate(lower_ci = estimate - 1.96 * std.error, 
         upper_ci = estimate + 1.96 * std.error) %>% 
  
  select(term, 
         lower_ci, 
         estimate, 
         upper_ci, 
         everything()) %>% 
  
  datatable() %>% 
  formatRound(2:7, 2)
