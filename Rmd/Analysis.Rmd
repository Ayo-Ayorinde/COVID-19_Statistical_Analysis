---
title: "Analysis of COVID-19 Dataset"
author: "Ayorinde Ayomide David"
date: "`r Sys.Date()`"
output: pdf_document
editor_options: 
  chunk_output_type: console
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE
)

```

### Importing the dependencies
```{r}
if(!require(pacman)) install.packages("pacman")  # Installing the package manager

pacman::p_load(tidyverse, # Metapackage
               here, # R library for coercing Rmarkdown into reading dataset from a seperate folder
               visdat, # R library for graphical inspection of dataset
               inspectdf, # R library for the distribution of variables 
               gtsummary,
               ggplot2
               )
```


### Loading the COVID-19 dataset into R
```{r Data, message=FALSE, warning=FALSE}

covid <- read_csv(here("Data/COVID19_line_list_data.csv"))
```

### Exploring and inspecting the dataset

```{r message=FALSE, warning=FALSE}
# Exploring the dataset

dim(covid)
head(covid, n = 10)
tail(covid, n = 10)

# Inspecting the dataset

vis_dat(covid)

inspect_cat(covid) %>% 
  show_plot()

inspect_num(covid) %>% 
  show_plot()


```


### Selecting, cleaning, transformation and manipulation of the variables of interest
```{r message=FALSE, warning=FALSE}
covid_selected <- covid %>% 
  select(id,
         reporting_date = 'reporting date', # The initial variable name has to go into quote because it has a whitespace which is unconventional for variable name in R
         gender, 
         death,
         age,
         country)


```


```{r}
covid_selected %>% 
  select(death) %>% 
  unique()


covid_selected <- covid_selected %>% 
  mutate(death = as.integer(covid$death != 0)) # This overwrite the initial death column by leaving entries as 0 or converting not zero entries to 1

covid_selected %>% # checking to confirm if the changes has been effected
  select(death) %>% 
  unique()

```


```{r}
covid_selected <- covid_selected %>% 
  mutate(reporting_date = mdy(reporting_date)) # This overwrite the initial reporting date by converting the variable class to Date

```


```{r}
covid_selected <- covid_selected %>% 
  mutate(month = month(reporting_date, label = T),
         month = replace_na(month,"Feb")) # The first mutate chunk create a new column for month and the second replaces na in the month variable with Feb

```

```{r}
covid_selected <- covid_selected %>% # Creating a column for continent
  mutate(continent = case_when(
    country %in% c("USA", "Canada") ~ "North America",
    country %in% c("France", "Germany", "Italy", "Russia", "UK", "Finland", "Spain",
                   "Sweden", "Belgium", "Austria", "Croatia", "Switzerland") ~ "Europe",
    country %in% c("China", "Japan", "Malaysia", "Nepal", "Singapore", "South Korea",
                   "Taiwan", "Thailand", "Vietnam", "Cambodia", "Sri Lanka", "UAE", 
                   "Hong Kong", "India", "Phillipines", "Iran", "Israel", "Lebanon",
                   "Kuwait", "Bahrain", "Afghanistan") ~ "Asia",
    country %in% c("Australia") ~ "Oceania",
    country %in% c("Egypt", "Algeria") ~ "Africa",
    TRUE ~ "Other"))
```


### Visualizing some of the variables of interest
```{r}
ggplot(covid_selected, mapping = aes(x = month)) +
  geom_bar() +
  labs(title = "Distribution of Cases Reported by Month",
       subtitle = "Jan 2020 - Feb, 2020",
       x = "Month",
       caption = "Analyst: Ayorinde Ayomide David")

```



```{r }
ggplot(covid_selected,mapping = aes(x = continent, fill = continent)) +
   geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribution of COVID-19 Cases by Country",
       subtitle = "Jan 2020 - Feb 2020",
       x = "Country",
       caption = "Analyst: Ayorinde Ayomide David",
       fill = "Country")

```


### Analyzing fatality by country
```{r}
covid_selected %>% 
  group_by(country) %>%  
  summarise(number_of_death = sum(death == 1))
  
```



### Statistical Analysis

**Two-sample t-test**

H~0~: There is no significant difference between the age of those alive and dead ($\mu_1 = \mu_2$)

H~1~: There is a significant difference between the age of those alive and dead ($\mu_1 \ne \mu_2$)

```{r}
covid_selected %>% 
  group_by(death) %>% 
  summarise(gender_death_mean = mean(age, na.rm = T))

```

We can see that there is a difference of about 20(in years) between the ages of those that are dead and alive. Now, the question:

Is this really significant?

Let's confirm using **t.test** 

```{r}
dead <- covid_selected %>% 
  filter(death == 1)
alive <- covid_selected %>% 
  filter(death == 0)

t.test(alive$age,
       dead$age,
       conf.level = 0.95,
       alternative = "two.sided")


```

**Decision rule**: If p-value is \< 0.05, we reject null hypothesis, otherwise, we fail to reject null hypothesis

**Conclusion**: Since the p-value is \< 0.05, we reject null hypothesis and conclude that there is a significant difference between the age of those that are dead and those that are alive. In order words, older people are more likely/prone to death if tested positive for COVID-19



**Test of Independence**

```{r}
covid_selected <- covid_selected %>% 
  drop_na(gender) %>% 
  mutate(gender_cov = factor(gender),
         death_cov = factor(case_when(death == 1 ~ "Dead",
                               death == 0 ~ "Alive")))

ggplot(covid_selected, mapping = aes(x = gender_cov,
                            fill = death_cov)) +
  geom_bar() +
  labs(title = "Chi-Square Test of Independence Bar Chart",
       subtitle = "Gender and Death",
       x = "Gender",
       caption = "Analyst: Ayorinde Ayomide David",
       fill = "Life status")

```


Are the proportions of gender independent of life status?

The question above leads us to the hypothesis below

H~0~: The variables are independent i.e There is no relationship between the variables

H~1~: The variables are not independent i.e There is a relationship between the variables

```{r}
covid_selected %>% 
  select(gender_cov,
         death_cov) %>% 
  table() %>% 
  chisq.test()

  
```

**Decision rule**: If p-value is \< 0.05, we reject null hypothesis, otherwise, we fail to reject null hypothesis

**Conclusion**: Since the p-value is \< 0.05, we reject null hypothesis and conclude that there is a relationship between the death and gender variable

