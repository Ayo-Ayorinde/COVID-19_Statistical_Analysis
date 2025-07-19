covid <- read_csv(here("Data/COVID19_line_list_data.csv"))


view(covid)

describe(covid)

covid %>% 
  select(death) %>% 
  unique()


names(covid)
covid <- covid %>% 
  select(!c(...4,...22,...23,...24,...25,...26)) %>%  #removing columns with all observation missing
  
  mutate(death_cleaned = as.integer(covid$death != 0))


covid %>% 
  select(death_cleaned) %>% 
  unique()


covid %>% 
  select(death_cleaned) %>% 
  sum()/ nrow(covid)


covid %>% 
  group_by(death_cleaned) %>% 
  summarise(gender_death_mean = mean(age, na.rm = T))

dead <- covid %>% 
  filter(death_cleaned == 1)
alive <- covid %>% 
  filter(death_cleaned == 0)

t.test(alive$age,
       dead$age,
       conf.level = 0.95,
       alternative = "two.sided")


covid <- covid %>% 
  drop_na(gender) %>% 
  mutate(gender_cov = factor(gender),
         death_cov = factor(case_when(death_cleaned == 1 ~ "Alive",
                                      death_cleaned == 0 ~ "Dead")))

ggplot(covid, mapping = aes(x = gender_cov,
                            fill = death_cov)) +
  geom_bar() +
  labs(title = "Chi-Square Test of Independence Bar Chart",
       subtitle = "Gender and Death",
       x = "Gender",
       caption = "Author: Ayorinde Ayomide David",
       fill = "Life status")



covid %>% 
  select(gender_cov,
         death_cov) %>% 
  table() %>% 
  chisq.test()
