

#*****************************************************
# Function to extract ED daily visits from denodo 
#*****************************************************

# function definition: 
extract_ed_visits_by_hour <- function(startdate_id, 
                              enddate_id, 
                              site = "Lions Gate Hospital", 
                              denodo_vw = vw_eddata){
  
  # inputs: 
  # > startdate and enddate as integers
  # > denodo_view is the name of the table saved as a connection via dbplyr 
  
  # outputs: 
  # dataframe with dates and ed visit counts
  
  eddata <- 
    denodo_vw %>% 
    filter(facility_name == !!(site),  # !! used to unquote the actual argument "site" 
           start_date_id >= startdate_id, 
           start_date_id <= enddate_id) %>% 
    
    dplyr::select(start_date_id, 
                  interval_1_hour_at_start_date, 
                  patient_id, 
                  facility_name) %>%  # show_query()
    
    collect() %>% 
    
    group_by(start_date_id, 
             interval_1_hour_at_start_date) %>%
    summarise(num_ED_visits = n())  
    
    # long format, matching with census results: 
  eddata <- 
    eddata %>%   
    mutate(nursing_unit_cd = NA) %>% 
    select(start_date_id, 
           interval_1_hour_at_start_date, 
           nursing_unit_cd, 
           num_ED_visits) %>% 
    rename(date_id = start_date_id) %>% 
    mutate(date = ymd(date_id)) %>% 
    
    # display in long format: 
    gather(key = "metric",
           value = "value", 
           -c(date_id, 
              date, 
              nursing_unit_cd, 
              interval_1_hour_at_start_date))
  
  
  # df of all dates and hours - for joining on
  hours <- c("0000 - 0059", "0100 - 0159", "0200 - 0259", "0300 - 0359", "0400 - 0459", "0500 - 0559", "0600 - 0659", "0700 - 0759", "0800 - 0859", "0900 - 0959", "1000 - 1059", "1100 - 1159", "1200 - 1259", "1300 - 1359", "1400 - 1459", "1500 - 1559", "1600 - 1659", "1700 - 1759", "1800 - 1859", "1900 - 1959", "2000 - 2059", "2100 - 2159", "2200 - 2259", "2300 - 2359")
  
  dates <- seq(ymd(startdate_id), 
               ymd(enddate_id), 
               by = "1 day")
  
  dates_df <- 
    data.frame(dates = rep(dates, each = 24), 
               hours = rep(hours, times = length(dates)))
  
  # join ed data on dates df: 
  eddata %>% 
    ungroup() %>% 
    right_join(dates_df, by = c("date" = "dates", 
                                "interval_1_hour_at_start_date" = "hours")) %>% 
    mutate(date_id_complete = gsub("-", 
                                   "", 
                                   as.character(date))) %>% 
    select(date_id_complete, 
           date, 
           interval_1_hour_at_start_date, 
           metric, 
           value)
  
}




# test the function: ------
# library(beepr)
# 
# edvisits_by_hour <- extract_ed_visits_by_hour("20180101",
#                                               "20190105")  # ; beep()
# 
# str(edvisits_by_hour)
