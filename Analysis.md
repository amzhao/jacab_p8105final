Analysis
================
Bing Bing Guo
11/24/2019

``` r
ems_data_clean = read.csv("./data/EMS_Incident_Dispatch_Data.csv") %>%
  select(INITIAL_SEVERITY_LEVEL_CODE, FINAL_SEVERITY_LEVEL_CODE, INITIAL_CALL_TYPE,
         DISPATCH_RESPONSE_SECONDS_QY, INCIDENT_TRAVEL_TM_SECONDS_QY, HELD_INDICATOR, BOROUGH,
         ZIPCODE, INCIDENT_DISPOSITION_CODE, INCIDENT_DATETIME) %>% 
  janitor::clean_names() %>% 
  separate(col = incident_datetime, into= c('date', 'time'), sep = ' ') %>% 
  mutate(arrival_outcome = ifelse(incident_disposition_code == "83", "dead", "alive"),
         initial_severity_level_code = factor(initial_severity_level_code, 
                                              levels = c("1", "2","3", "4", "5", "6", "7", 
                                                         "8", "9"), ordered = TRUE),
         final_severity_level_code = factor(final_severity_level_code, 
                                            levels = c("1", "2", "3", "4", "5", "6", "7", "8"),
                                            ordered = TRUE), 
         held_indicator = recode(held_indicator, "N" = "no", "Y" = "yes")) %>%
  select(-incident_disposition_code)
```

    ## Warning: Expected 2 pieces. Additional pieces discarded in 341372 rows [1,
    ## 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].
