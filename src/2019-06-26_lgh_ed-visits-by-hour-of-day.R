
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
  
  replace(is.na(.), 0) %>% 
  
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

# add hour_int column: 
days <- df2.ed_visits_cleaned$date %>% unique() %>% length()

df2.ed_visits_cleaned <- 
  df2.ed_visits_cleaned %>% 
  mutate(hour_int = rep(1:24, 
                        days))


# result: 
str(df2.ed_visits_cleaned)

df2.ed_visits_cleaned %>%
  head(100) %>% 
  datatable()

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


#' ## Notes - Poisson?
#'
#' Mean and variance aren't really close for most hours. Does this mean that
#' arrival rate isn't really Poisson-distributed?

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
#' "late-night" rates, one for rest of the day.
#'
#' Let's start by analyzing visits over the period 7 AM to midnight. This seems
#' to give a reasonable distribution.
#'
#'
#' 


# density for "busy" hours: 
df2.ed_visits_cleaned %>% 
  filter(hour_int >= 8, 
         hour_int <= 24) %>% 
  ggplot(aes(x = ed_visits)) + 
  geom_density() + 
  facet_wrap(~year) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"),
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1))


# compare with poisson curves: 
# mean_visits_from_5am <- 
#   df2.ed_visits_cleaned %>% 
#   filter(year == "2018", 
#          hour_int >= 5, 
#          hour_int <= 24) %>% 
#   pull(ed_visits) %>% 
#   mean(na.rm = TRUE)
# 
# x <- rpois(1000, mean_visits_from_5am)
# x %>% density() %>% plot(main = paste("Poisson with mean", round(mean_visits_from_5am, 2)))


#'
#' ### Plots for each hour of day in 2018 
#' 

df4.nest_by_hour <- 
  df2.ed_visits_cleaned %>% 
  filter(year == "2018") %>% 
  group_by(hour_int) %>% 
  nest %>% 
  
  mutate(density = map2(data, 
                        hour_int, 
                        function(x, y){
                         ggplot(x, 
                                aes(x = ed_visits)) + 
                           geom_density() + 
                           labs(title = y %>% as.character())
                       }))

df4.nest_by_hour$density
 

#' ## Data - subset for 7 AM to midnight 
#' 

df5.ed_visits_busy_hours <- 
  df2.ed_visits_cleaned %>% 
  filter(hour_int >= 8)

df5.ed_visits_busy_hours %>% 
  head(50) %>% 
  datatable()


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
  filter(hour_int >= 8, 
         hour_int <= 24) %>% 
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
      





# avg ED visits by weekday AND hour
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
#' 
#' ### m1: With interaction between hour and weekday

#+ models

set.seed(11)
v1_train_index <- createDataPartition(df5.ed_visits_busy_hours$ed_visits, 
                                      p = 0.8, 
                                      list = FALSE)

# fit model: 
m1 <- lm(ed_visits ~ hour + weekday + years_from_2017 + 
           lag_ed_visits + lag2_ed_visits + hour:weekday, 
         data = df5.ed_visits_busy_hours[v1_train_index, ])

summary(m1)

# diagnostics
resid(m1) %>% hist

par(mfrow = c(2,2))
plot(m1)
par(mfrow = c(1,1))


glance(m1) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
              "condensed", 
              "responsive"))
              

# tidy(m1)

augment(m1) %>% 
  ggplot(aes(x = .fitted, 
             y = ed_visits)) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 25)) + 
  scale_y_continuous(limits = c(0, 25)) + 
  
  geom_smooth() + 
  geom_abline(slope = 1, 
              intercept = 0) + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95"))
      

# predict(m1, interval = "prediction")

m1.train_rmse <- sqrt(mean(resid(m1)^2))


# test set performance: 
df6.predictions_m1 <- 
  data.frame(ed_visits = df5.ed_visits_busy_hours[-v1_train_index, 7], 
             predicted = predict(m1, 
                                 newdata = df5.ed_visits_busy_hours[-v1_train_index, ])) 

m1.test_rmse <- sqrt(mean((df6.predictions_m1$predicted - df6.predictions_m1$ed_visits)^2, 
                          na.rm = TRUE))









#' ### m2: Without interaction between hour and weekday
#' 

set.seed(11)
v1_train_index <- createDataPartition(df5.ed_visits_busy_hours$ed_visits, 
                                      p = 0.8, 
                                      list = FALSE)

# fit model: 
m2 <- lm(ed_visits ~ hour + weekday + years_from_2017 + 
           lag_ed_visits + lag2_ed_visits,  
         data = df5.ed_visits_busy_hours[v1_train_index, ])

summary(m2)


# diagnostics
resid(m2) %>% hist

par(mfrow = c(2,2))
plot(m2)
par(mfrow = c(1,1))


glance(m2) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "condensed", 
                                      "responsive"))


# tidy(m2)

# actual vs predicted values: 
augment(m2) %>% 
  ggplot(aes(x = .fitted, 
             y = ed_visits)) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 25)) + 
  scale_y_continuous(limits = c(0, 25)) + 
  
  geom_smooth() + 
  geom_abline(slope = 1, 
              intercept = 0) + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


# predict(m2, interval = "prediction")

m2.train_rmse <- sqrt(mean(resid(m2)^2))


# test set performance: 
df7.predictions_m2 <- 
  data.frame(ed_visits = df5.ed_visits_busy_hours[-v1_train_index, 7], 
             predicted = predict(m2, 
                                 newdata = df5.ed_visits_busy_hours[-v1_train_index, ])) 

m2.test_rmse <- sqrt(mean((df7.predictions_m2$predicted - df7.predictions_m2$ed_visits)^2, 
                          na.rm = TRUE))






#' ### m3: Without interaction between hour and weekday: POISSON model 
#' 

set.seed(11)
v1_train_index <- createDataPartition(df5.ed_visits_busy_hours$ed_visits, 
                                      p = 0.8, 
                                      list = FALSE)

m3 <- glm(ed_visits ~ hour + weekday + years_from_2017 + 
           lag_ed_visits + lag2_ed_visits, 
         data = df5.ed_visits_busy_hours[v1_train_index, ], 
         family = "poisson")

summary(m3)



# diagnostics
resid(m3) %>% hist

par(mfrow = c(2,2))
plot(m3)
par(mfrow = c(1,1))



glance(m3) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "condensed", 
                                      "responsive"))

# Actual vs predicted vals 
augment(m3) %>% 
  ggplot(aes(x = .fitted, 
             y = ed_visits)) + 
  geom_point() + 
  scale_x_continuous(limits = c(0, 5)) + 
  scale_y_continuous(limits = c(0, 25)) + 
  
  geom_smooth() + 
  geom_abline(slope = 1, 
              intercept = 0) + 
  
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))


# 5) summary of models --------------------------------------

#' ## Summary of models
#'
#' * Poisson model is clearly wildly inappropriate
#'
#' * Between m1 and m2, m2 is simpler with similar adj-Rsqrd, and similar test
#' RMSE
#'
#' * However, m1 includes several significant interactions, and studying
#' interactions is one of the key goals in this analysis. To look at it another
#' way, m1 is more complex, but is probably not overfitting much.
#' 
#' * Let's go with m1 here. 

#' \  
#'

df8.model.performance <- 
  data.frame(model = c("year + weekday + hour + hour:weekday + lag + lag2", 
                       "year + weekday + hour + hour:weekday + lag + lag2", 
                       "year + weekday + hour + lag + lag2", 
                       "year + weekday + hour + lag + lag2"), 
             metric = rep(c("Train RMSE", 
                            "Test RMSE"), 2), 
             value = c(m1.train_rmse, 
                       m1.test_rmse, 
                       m2.train_rmse, 
                       m2.test_rmse)) 


df8.model.performance %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "condensed", 
                                      "responsive"))




#' ## Train selected model on full dataset 
#' 

m4.full_dataset <- lm(ed_visits ~ hour + weekday + years_from_2017 + 
                        lag_ed_visits + lag2_ed_visits + hour:weekday, 
                      data = df5.ed_visits_busy_hours)


glance(m4.full_dataset) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "condensed", 
                                      "responsive"))


df9.coeffs <- 
  tidy(m4.full_dataset) %>% 
  mutate(lower_ci = estimate - 1.96 * std.error, 
         upper_ci = estimate + 1.96 * std.error, 
         is_signif_0.10 = ifelse(p.value < 0.10, 
                                 1, 0)) %>% 
  
  dplyr::select(term, 
                lower_ci, 
                estimate, 
                upper_ci, 
                everything()) 

df9.coeffs %>% 
  datatable() %>% 
  formatRound(2:7, 2)

#'
#' \  
#' 


# 6) pre-processing before visualization: -----------

#'
#'## Pre-processing before visualization
#'

df10.sig_interactions <- 
  df9.coeffs %>% 
  filter(grepl(":", term), 
         is_signif_0.10 == 1) %>% 
  mutate(hour = substr(term, 1, 15)) %>% 
  select(term, 
         hour, 
         estimate)


df11.hour_effects <- 
  df9.coeffs %>% 
  filter(!grepl(":", term), 
         grepl("hour", term)) %>% 
  
  left_join(df10.sig_interactions, 
            by = c("term" = "hour")) %>% 
  
  mutate(is_interaction = ifelse(is.na(estimate.y), 
                                       "No", "Yes") %>% 
           as.factor) %>% 
  
  select(term, 
         lower_ci, 
         estimate.x, 
         upper_ci, 
         is_interaction)



#' ## Visual of hour effects 
#' 

# 7) visuals of hour and day of week effects ----------

df11.hour_effects %>% 
  
  mutate(term = substring(term, 5) %>% as.factor()) %>% 
  
  ggplot()  +
  geom_pointrange(aes(x = term, 
                      ymin = lower_ci, 
                      ymax = upper_ci, 
                      y = estimate.x, 
                      col = is_interaction)) + 
  geom_hline(yintercept = 0) + 
  
  scale_y_continuous(limits = c(-5, 10), 
                     breaks = seq(-5, 10, 3)) + 
  
  scale_color_manual(values = c("black", 
                               "red")) + 
  
  labs(x = "Hour of day", 
       y = "Difference in average hourly ED visits" ,
       title = "LGH ED \nImpact of Hour of Day on average hourly ED visits", 
       subtitle = "These estimates control for year and day-of-week, allowing us to \nisolate hourly effects from other factors and from statistical noise \n\nBaseline - 0700 to 0759 on Monday", 
       caption = "\n\nNote: our model accounts for 30% of the variation\nin hourly ED visits between 7 AM and midnight", 
       col = "Varies by weekday?") + 
  
  theme_light(base_size = 12) +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"), 
        axis.text.x = element_text(angle = 45, 
                                   hjust = 1)
        )


#' \  
#' \  
#' \  

#' 
#' ## Prediction intervals for illustrative data
#'
#' Import illustrative data to predict on. Note that all lagged ed_visits values
#' are set to the overall mean for the corresponding hour of day (see
#' `df3.mean_and_sd`)


df12.predict_intervals <- 
  read_csv(here::here("data", 
                      "2019-06-30_illustrative-data-for-prediction-intervals.csv")) %>% 
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
         hour = as.factor(hour)) %>% 
  
  # only MOnday: 
  filter(weekday == "Monday") %>% 
  
  # identify hours with significant weekday effects: 
  left_join(df10.sig_interactions %>% 
              mutate(hour = substr(hour, 5, 15)) %>% 
              select(hour) %>% 
              unique() %>% 
              mutate(is_interaction = 1)) %>% 
  
  replace(is.na(.), 0) %>% 
  mutate(is_interaction = as.factor(is_interaction))



# add predictions from model m1: 
df12.predict_intervals <- 
  df12.predict_intervals %>% 
  bind_cols(predict(m1, 
                    newdata = df12.predict_intervals,
                    interval = "predict") %>% 
              as.data.frame()) %>% 
  
  
  
# group and nest: 
  group_by(weekday) %>% 
  nest() 

# graphs of time of day ed_visits, by weekday: 
df12.predict_intervals <- 
  df12.predict_intervals %>% 
  mutate(hourly_visits = map2(data, 
                              weekday, 
                              function(data, weekday){
                               ggplot(data) + 
                                 geom_pointrange(aes(x = hour, 
                                                     y = fit, 
                                                     ymin = lwr, 
                                                     ymax = upr, 
                                                     col = is_interaction)) + 
                                  labs(title = paste0(as.character(weekday), "s", " - 2019"), 
                                       subtitle = "Predicted ED visits by hour of day, after accounting for \nyear, weekday effects, and intrinsic variability", 
                                       x = "Hour", 
                                       y = "ED visits", 
                                       col = "Varies by weekday?") + 
                                  
                                  scale_y_continuous(limits = c(-2, 20), 
                                                     breaks = seq(-2, 20, 2)) + 
                                  
                                  scale_color_manual(values = c("black", 
                                                                "red")) + 
                                  
                                  theme_light() +
                                  theme(panel.grid.minor = element_line(colour = "grey95"), 
                                      panel.grid.major = element_line(colour = "grey95"), 
                                      axis.text.x = element_text(angle = 45, 
                                                                 hjust = 1))
                                      
                                  
                                 
                             }
                             ))
  
  
df12.predict_intervals$hourly_visits    








# 8) write outputs: -----------------------------
# write_csv(df5.ed_visits_busy_hours,
#           here::here("results",
#                      "dst",
#                      "2019-07-02_lgh_ed-visits-by-hour_data.csv"))


# write_csv(df9.coeffs,
#           here::here("results", 
#                      "dst", 
#                     "2019-06-28_lgh_ed-visits-by-hour_regression-coeffs.csv"))
             
