---
title: "Statistical Analyses"
output: 
  html_document:
  code_folding: hide
  toc: true
  toc_float: true
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(viridis)
library(rvest)
library(purrr)
library(broom)
library(modelr)
library(mgcv)
library(patchwork)
library(plotly)
library(maps)
library(leaflet)
library(rgdal)
library(maptools)
library(BAMMtools)
library(spdep)
library(kableExtra)
```


```{r warning=FALSE, echo = FALSE}
###Importing and cleaning EMS data in NYC: 
ems_data_clean = read.csv("./data/EMS_Incident_Dispatch_Data.csv") %>%
  select(INITIAL_SEVERITY_LEVEL_CODE, FINAL_SEVERITY_LEVEL_CODE, INITIAL_CALL_TYPE,
         DISPATCH_RESPONSE_SECONDS_QY, INCIDENT_TRAVEL_TM_SECONDS_QY, HELD_INDICATOR, BOROUGH,
         ZIPCODE, INCIDENT_DISPOSITION_CODE, INCIDENT_DATETIME) %>% 
  janitor::clean_names() %>% 
  separate(col = incident_datetime, into = c('date', 'time'), sep = ' ') %>% 
  separate(col = date, into = c("month","day"), sep = '/') %>% 
  mutate(month = factor(month, levels = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), labels = c("Jan", "Feb", "Mar", "Apr", "May", "June", "Jul", "Aug", "Sept", "Oct", "Nov", "Dec"), ordered = TRUE)) %>% 
  mutate(arrival_outcome = ifelse(incident_disposition_code == "83", "dead", "alive"),
         arrival_outcome = recode_factor(arrival_outcome, `0` = "alive", `1` = "dead"),
         initial_severity_level_code = factor(initial_severity_level_code, 
 levels = c("1", "2","3", "4", "5", "6", "7", "8", "9"), ordered = TRUE),
         final_severity_level_code = factor(final_severity_level_code, 
                                            levels = c("1", "2", "3", "4", "5", "6", "7", "8"), ordered = TRUE), 
         held_indicator = recode(held_indicator, "N" = "no", "Y" = "yes")) %>%
  mutate(neighbourhood = recode(zipcode, "10026" = "central harlem", "10027" = "central harlem", "10030" = "central harlem", "10037" = "central harlem", "10039" = "central harlem", "10001" = "chelsea and clinton", "10001" = "chelsea and clinton", "10011" = "chelsea and clinton", "10018" = "chelsea and clinton", "10019" = "chelsea and clinton", "10020" = "chelsea and clinton", "10036" = "chelsea and clinton",  "10029" = "east harlem", "10035" = "east harlem", "10010" = "gramercy park and murray hill", "10016" = "gramercy park and murray hill", "10017" = "gramercy park and murray hill", "10022" = "gramercy park and murray hill", "10012" = "greenwich village and soho", "10013" = "greenwich village and soho", "10014" = "greenwich village and soho", "10004" = "lower manhattan", "10005" = "lower manhattan", "10006" = "lower manhattan", "10007" = "lower manhattan", "10038" = "lower manhattan", "10280" = "lower manhattan", "10002" = "lower east side", "10003" = "lower east side", "10009" = "lower east side", "10021" = "upper east side", "10028" = "upper east side", "10044" = "upper east side", "10065" = "upper east side", "10075" = "upper east side", "10128" = "upper east side", "10023" = "upper west side", "10024" = "upper west side", "10025" = "upper west side", "10031" = "inwood and washington heights", "10032" = "inwood and washington heights", "10033" = "inwood and washington heights", "10034" = "inwood and washington heights", "10040" = "inwood and washington heights" )
  ) %>%  
  drop_na(neighbourhood) %>% 
    select(-incident_disposition_code) 
```

#
#
  
# Two Sample T-test

The Average response time for those who survived was 8.83 minutes, while the average for those who died was 4.91 minutes.

In order to see if the difference in average response time between those who survived and those who died was significantly different, we performed a two sample t-test. 

```{r warning=FALSE, echo = FALSE}
###t-test
t.test(ems_data_clean$incident_travel_tm_seconds_qy/60 ~ ems_data_clean$arrival_outcome, na.rm = FALSE)
```

The t-test value for the average incident response time between when the patient survived and when they died was 51.493. Given the p-value <.0001 we can conclude that the averages are significantly different, at the 5% level of significance. 


# ANOVA

In order to assess whether the travel time varies significantly between neighborhoods in New York City, we performed an ANOVA test.

```{r warning=FALSE, echo = FALSE}
#ANOVA
res.aov = aov(incident_travel_tm_seconds_qy/60 ~ neighbourhood, data = ems_data_clean)
# Summary of the analysis
summary(res.aov) 
```

Given the p-value of <.0001 is smaller than 0.05, we can reject the null hypotheis and conclude that the mean incident travel time is not zero for at least one of the neighborhoods, and can thus say that there is a statistically significant difference between the mean incident travel time in seconds between neighborhoods, at the 5% level of significance. 

# Logistic Regression


We performed a logistic regression to model the log odds of death on arrival, given incident travel time in seconds and controlling for neighborhood.

The result odds ratios and corresponding p-values of death on arrival (survival) for each of the variables considerd can be found in the table below:

```{r include= FALSE, warning=FALSE, echo = FALSE}
###Logistic Regression Model and Results Table: 
ems_data_clean %>%  
    mutate(
    initial_severity_level_code = fct_relevel(initial_severity_level_code, "1")) %>%
    select(incident_travel_tm_seconds_qy, initial_severity_level_code, neighbourhood)
```

```{r echo = FALSE}
model = glm(arrival_outcome ~ incident_travel_tm_seconds_qy + neighbourhood, data = ems_data_clean, family = binomial())



#create tidy table
model %>% 
  broom::tidy() %>% 
  mutate(OR = exp(estimate)) %>%
  select(term, estimate, OR, p.value) %>% 
  knitr::kable(digits = 3) %>% 
  kable_styling()
```
