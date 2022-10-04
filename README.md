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
setwd("G:/MPP/Fall 2022/PPOL 670/assignment04")
```

Making new variables
```{r}
data <- mutate(data, racial_category = 
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


### Income and Educational Attainment of Working Age Adults, Arizona, 2019
```{r}
options(scipen=999)
data %>%
  filter(!is.na(EDUCD) & AGE >= 18 & INCTOT > 0 & INCTOT < 9999999 & STATEFIP == 04)%>%
  ggplot() +
  geom_point(aes(x = years_educ, y = INCTOT), color = "red", alpha = 0.2) +
  geom_smooth(aes(x = years_educ, y = INCTOT), linetype = "longdash") +
  scale_y_log10() +
  labs(
    title = "Income and Educational Attainment of Working Age Adults, Arizona, 2019",
    subtitle = "As educational attainment increases, income also grows, at an increasing rate.",
    caption = "Steven Ruggles, et. al. IPUMS USA: Version 10.0 [dataset]. Minneapolis,\nMN: IPUMS, 2020. https://doi.org/10.18128/D010.V10.0",
    x = "Years of education",
    y = "Log of individual annual income in USD"
  )
```

### Percentage of Population in Poverty, by Race, Arizona, 2019
```{r}
data %>%
  filter(OFFPOV != 9, STATEFIP == 04) %>%
  group_by(racial_category) %>%
  summarize(prop_poverty = mean(OFFPOV)) %>%
  ggplot(mapping = aes(x = racial_category, y = prop_poverty, fill = racial_category)) +
  geom_col() +
  geom_text(aes(label = round(prop_poverty, digits = 4)), vjust = -.4, size = 3, fontface = "bold") +
  theme(axis.text.x = element_blank()) +
  labs(
    title = "Percentage of Population in Poverty, by Race, Arizona, 2019",
    subtitle = "Whites, Asians, and Pacific Islanders have the lowest poverty rates, while American\nIndians have the highest.",
    caption = "Steven Ruggles, et. al. IPUMS USA: Version 10.0 [dataset].\nMinneapolis, MN: IPUMS, 2020.\nhttps://doi.org/10.18128/D010.V10.0",
    x = "Race",
    y = "Proportion of population in poverty",
    fill = 'Race'
  )
```

### Total Income of Working Age Adults, by Gender, Arizona, 2019
```{r}
data %>%
  filter(INCTOT > 0 & INCTOT < 9999999 & STATEFIP == 04 & AGE >= 18 & years_educ <= 12) %>%
  mutate(
    Gender = if_else(
      condition = SEX == 1,
      true = "Male", false = "Female"
    )
  ) %>%
  ggplot(aes(x = INCTOT, y = factor(1), shape = Gender, color = Gender)) +
  geom_point(alpha = .1, size = 5) +
  scale_x_continuous() +
  scale_y_discrete(labels = NULL) +
  labs(
    title = "Total Income of Working Age Adults, by Gender, Arizona, 2019",
    subtitle = "Most of the population earns below $250,000, and men are more likely than women to\nhave high enough incomes to be outliers.",
    caption = "Steven Ruggles, et. al. IPUMS USA: Version 10.0 [dataset]. Minneapolis,\nMN: IPUMS, 2020. https://doi.org/10.18128/D010.V10.0",
    x = "Total annual individual income in USD",
    y = NULL
    )
```

### Income Distribution of Working Age Adults, by Race, Arizona, 2019
```{r}
labels <- c('American Indian\nor Alaska Native', 'Black/African\nAmerican', 'Chinese or\nJapanese', 'Other', 'Other Asian or\nPacific Islander', 'White')
data %>%
  filter(INCTOT > 0 & INCTOT < 9999999 & STATEFIP == 04 & AGE >= 18) %>%
  ggplot(aes(x = racial_category, y = INCTOT)) +
  geom_boxplot(color = "dark blue", fill = "light blue") +
  scale_x_discrete(labels = labels) +
  labs(
    title = "Income Distribution of Working Age Adults, by Race, Arizona, 2019",
    subtitle = "Asians and Pacific Islanders have the the widest interquartile ranges, while\nAmerican Indians have the narrowest, despite having many outlying high-earners.",
    caption = "Steven Ruggles, et. al. IPUMS USA: Version 10.0 [dataset]. Minneapolis,\nMN: IPUMS, 2020. https://doi.org/10.18128/D010.V10.0",
    x = "Race",
    y = "Individual annual income in USD"
  )
```