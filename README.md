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
options(scipen=999)
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


### Total Income of Working Age Adults, by Gender, Arizona, 2019
```{r}
data %>%
  filter(INCTOT >= 0 & INCTOT < 9999999 & STATEFIP == 04 & AGE >= 18 & years_educ <= 12) %>%
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

This strip chart highlights income distribution by gender of adults in Arizona using 2019 income data. It shows that men are more likely than women to be the highest earners, as well as that the bulk of observations of male income is slightly higher than that of women. The graph also shows that the vast majority of individuals in the population earn below $250,000 annually, and that only singular individuals earn above $400,000. Given this disparity income between male and female individuals, policy can be pursued that encourages women to pursue higher paying jobs, and firms to hire more women for those higher paying jobs. Policy can be made to lower this gap, depending on the reason behind the disparity, such as improved access to education and training for women, or better maternal leave.

### Income Distribution of Working Age Adults, by Race, Arizona, 2019
```{r}
labels <- c('American Indian\nor Alaska Native', 'Black/African\nAmerican', 'Chinese or\nJapanese', 'Other', 'Other Asian or\nPacific Islander', 'White')
data %>%
  filter(INCTOT > 0 & INCTOT < 9999999 & STATEFIP == 04 & AGE >= 18) %>%
  ggplot(aes(x = racial_category, y = INCTOT)) +
  geom_boxplot(color = "dark blue", fill = "light blue") +
  scale_x_discrete(labels = labels) +
  scale_y_log10() +
  labs(
    title = "Income Distribution of Working Age Adults, by Race, Arizona, 2019",
    subtitle = "Asians and Pacific Islanders have the the widest interquartile ranges, while\nAmerican Indians have the narrowest, despite having many outlying high-earners.",
    caption = "Steven Ruggles, et. al. IPUMS USA: Version 10.0 [dataset]. Minneapolis,\nMN: IPUMS, 2020. https://doi.org/10.18128/D010.V10.0",
    x = "Race",
    y = "Log of individual annual income in USD"
  )
```

This boxplot illustrates the income distribution of adults, whose annual income is greater than $0, in Arizona in 2019, this time by race. We can see that Asians and Pacific Islanders have the highest interquartile ranges, with Whites falling close behind. At the same time, the interquartile range of income is lowest for Black/African American and American Indian/Alaska Native workers, as well as those who do not explicitly belong to any of the racial categories. We can also see that Whites have the most extreme outliers in terms of income on both ends of the distribution. When taking the racial disparity into account, we can suggest that policy that aims to increase incomes be targeted directly at Blacks, American Indians, and those of an unclassified race.


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

This bar chart illustrates the poverty rate in Arizona by race in 2019, and provides results similar to those seen in the boxplot above: people identifying as White or Other Asian/Pacific Islander have lowest rates of poverty, with those identifying as Chinese or Japanese close behind. At the same time, those identifying as Black, as an unclassified race, and particularly American Indian/Alaska Native, have much higher poverty rates, with the latter group having a poverty rate of 30.84%. In combination with the boxplot, we can see that anti-poverty measures should be targeted primarily towards Black and American Indian communities. Such measures can be greater investments into public education to decrease long term poverty, or investments into public infrastructure in those particular communities.


### Income and Educational Attainment of Working Age Adults, Arizona, 2019
```{r}
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

This scatter-line plot shows the positive relationship between income and educational attainment for adults in Arizona, using 2019 individual-level income data. The graph demonstrates that particularly after 12 years of schooling - after getting a high school diploma - the certainty and magnitude of the relationship increase, showing that an extra year of schooling has a positive impact on earnings. With particularly low incomes and high poverty rates, we can attempt to provide those who struggle financially with a long term remedy: education. Investing into high school education and graduation, especially in Black and American Indian communities in Arizona, can increase the number of high-earning individuals in the overall population.