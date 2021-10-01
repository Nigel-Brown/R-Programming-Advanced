library(tidyverse)

students <- read_csv(here::here('Data Cleansing', "data", 'student_scores.csv'))

students

students_tidy  <- students %>% 
  pivot_longer(ends_with('Grade'),
               names_to = 'Level',
               values_to = 'Score')


grvec <- c("A+"=4.3,"A"=4,"A-"=3.7,"B+"=3.3,"B"=3,"B-"=2.7,
              "C+"=2.3,"C"=2,"C-"=1.7,"D+"=1.3,"D"=1,"D-"= 0.7, "F"=0)

students_tidy$Grade <- grvec[as.character(students_tidy$Score)]

students_tidy

students_tidy %>% 
  ggplot() +
  geom_line(aes(Level, Grade, group = `Last Name`, colour = `Last Name`, linetype = `Last Name`)) +
  theme_light() +
  labs(
    title = 'Student score by year',
    x = NULL,
    y = NULL
  ) +
  theme(
    legend.position = 'bottom'
  )
