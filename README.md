---
title: "README.md"
author: "Ilya Schneider, Juan Reyes"
date: "`r Sys.Date()`"
output: html_document
---

[Source of the data](https://usa.ipums.org/usa/index.shtml)

Set-up
```{r}
#Enabling all the needed libraries
library(tidyverse)
library(ggplot2)
#install.packages('ipumsr')
library(ipumsr)

#Changing the working directory to import the data
setwd("G:/MPP/Fall 2022/PPOL 670/assignment04/data")
ddi <- read_ipums_ddi("usa_00002.xml")
data <- read_ipums_micro(ddi)
```

Making new variables
```{r}
data <- mutate(data, race_groups = 
           case_when(
             RACE == 1 ~ "White",
             RACE == 2 ~ "Black/African American",
             RACE == 3 ~ "American Indian or Alaska Native",
             RACE == 4 | RACE == 5 ~ "Chinese or Japanese",
             RACE == 6 ~ "Other Asian or Pacific Islander",
             TRUE ~ "Other"
           ))
data <- mutate(data, years_educ = 
                 case_when(
                   EDUCD <= 12 ~ 0, 
                   EDUCD == 14 ~ 1, 
                   EDUCD == 15 ~ 2, 
                   EDUCD == 16 ~ 3, 
                   EDUCD == 17 ~ 4, 
                   EDUCD == 22 ~ 5, 
                   EDUCD == 23 ~ 6, 
                   EDUCD == 25 ~ 7, 
                   EDUCD == 26 ~ 8, 
                   EDUCD == 30 ~ 9, 
                   EDUCD == 40 ~ 10, 
                   EDUCD == 50 ~ 11, 
                   EDUC == 06 ~ 12, 
                   EDUCD == 71 ~ 13, 
                   EDUCD == 81 ~ 14, 
                   EDUCD == 101 ~ 16, 
                   EDUCD == 114 ~ 18, 
                   EDUCD >= 115 ~ 21))
```


###Income and Educational Attainment of Working Age Adults, Arizona, 2019
```{r}
options(scipen=999)
data %>%
  filter(!is.na(EDUCD) & AGE >= 18 & INCTOT > 0 & INCTOT < 9999999 & STATEFIP == 04)%>%
  ggplot() +
  geom_point(aes(x = years_educ, y = INCTOT), color = "red", alpha = 0.05) +
  geom_smooth(aes(x = years_educ, y = INCTOT)) +
  scale_y_log10() +
  labs(
    title = "Income and Educational Attainment of Working Age Adults, Arizona, 2019",
    subtitle = "As educational attainment increases, income also grows, at an increasing rate.",
    caption = "Steven Ruggles, et. al. IPUMS USA: Version 10.0 [dataset]. Minneapolis, MN: IPUMS, 2020. https://doi.org/10.18128/D010.V10.0",
    x = "Years of education",
    y = "Log of individual income in USD"
)
```
