library(tidyverse)
library(tidytext)


data <- read_csv('classification_analysis.csv')

data <- data %>% 
  janitor::clean_names()

data_unknown <- data %>% 
  filter(category == 'Unknown')


percentage_unknown = nrow(data_unknown) / nrow(data)

num_cat <- nrow(data) - nrow(data_unknown) 



text_df <- tibble(line = 1:nrow(data_unknown), text = data_unknown$name)


words <- text_df %>% 
  unnest_tokens(word, text)

custom_stop_words <- tribble(~word, ~lexicon,
                             "4c", "CUSTOM",
                             "paper", "CUSTOM",
                             "printing", "CUSTOM",
                             "xxx", "CUSTOM",
                             "glossy", "CUSTOM",
                             "cover", "CUSTOM",
                             "pages", "CUSTOM"
                             )

stop_words2 <- stop_words %>% 
  bind_rows(custom_stop_words)

cnt_words <- words %>% 
  anti_join(stop_words2, by = c("word" = "word")) %>% 
  group_by(word) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
  
##View(cnt_words)

test <- data_unknown$name
nonlatin_ind <- grep("[\\p{Han}]", test, value = TRUE, perl = TRUE) 
data_unknown %>% 
 # filter(name %in% nonlatin_ind) %>% 
  group_by(name) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 


data %>% 
  group_by(account_name ) %>% 
  summarise(
    n = n()) %>% 
  ggplot(aes(fct_reorder(account_name, n),n)) +
  geom_col() +
  scale_y_log10() +
  coord_flip()

data_unknown %>% 
  group_by(account_name ) %>% 
  summarise(
    n = n()
  ) %>% 
  ggplot(aes(fct_reorder(account_name, n),n)) +
  geom_col() +
  scale_y_log10() +
  coord_flip()

data_unknown %>% 
  group_by(name) %>%    
  summarise(n = n()) %>% 
  arrange(desc(n))

data %>% 
  select(product_id) %>% 
  distinct() %>% 
  nrow()

