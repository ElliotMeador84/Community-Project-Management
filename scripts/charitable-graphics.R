## ----------------------
## Title:   Chartitable Orgaisations (graphics)
## Purpose: Script for creating plots of chartible
##          data in Scotland
## Date: 28 February 2021
## ----------------------

library(tidyverse)
library(scales)
library(pdftools)
library(fs)
library(ggfittext)
library(flextable)



# Change working directory to Community Project Management home folder in OneDrive

current_wd <- getwd()

comm_proj <-"~/OneDrive/module/community-project-management-2021/"

# setwd(comm_proj)

setwd("plots-figures/scottish-charity")



#setwd(current_wd)

## Section 1 ----

scottish_char <- read_csv("/Users/emeador/OneDrive/Data/charity/scotland/scotland_charity_aug_2020.csv")

## Functions ----
quick_save <- function(.x){

ggsave(.x, 
       filename =   paste0(deparse(substitute(.x)), ".png"), 
       width = 13,
       height = 8, 
       dpi = 350
)  
}

labs_standard <- label_number_si(accuracy = 1)
labs_pound <- label_number_si(accuracy = 0.1)


ggploter <- function(.df){
  .df %>% 
    rename(vary = 1, 
           n = 2, 
           per = 3) %>% 
  ggplot(aes(vary, n, label = per))+
    geom_col(aes(fill = vary), 
             width = 0.725,
             show.legend = F)+
    geom_bar_text(aes(fill = vary),
                  contrast = TRUE, size = 12) +
    scale_x_discrete(labels = function(.x)str_wrap(.x, 55))+
    scale_y_log10(labels = labs_standard) +
    theme(text = element_text(size = 16))
}
## Table information ----


scottish_char %>% 
  summarise(donations_and_legacies_income = 
              sum(donations_and_legacies_income, 
                  na.rm = T), 
            charitable_activities_income = 
              sum(charitable_activities_income, 
                  na.rm = T), 
            other_trading_activities_income = 
              sum(other_trading_activities_income, 
                  na.rm = T), 
            investments_income = sum(
              investments_income, 
              na.rm = T
            ), 
            other_income = sum(other_income, 
                               na.rm = T), 
            raising_funds_spending = sum(raising_funds_spending,
                                         na.rm = T), 
            charitable_activities_spending = sum(charitable_activities_spending, na.rm = T), 
            other_spending = sum(other_spending, 
                                 na.rm = T)) %>% 
  gather(key, value) %>% 
  arrange(-value) %>% 
  mutate(value_c = labs_pound(value), 
         key = str_replace_all(key, "_", " "), 
         key = str_to_sentence(key)) %>% 
  select(-value) %>% 
  set_names("Spending/income category", "Amount") %>% 
  flextable() %>% 
  autofit() %>% 
  set_caption("Financial statistics of Scottish charities (2020)")



## Bar plots 2 ----

## Regulatory type
(scottish_char %>% 
  count(regulatory_type) %>% 
  drop_na() %>% 
  mutate(per = percent(n/sum(n)), 
         regulatory_type = fct_reorder(regulatory_type, n)) %>% 
  ggploter() +
  scale_fill_brewer(palette = "Set1") +
  labs(caption  = "Social housing in Scotland is housing owned and managed by public authorities (mainly councils) and housing associations (registered social landlords or RSLs)." ,
       subtitle = "SOURCE: Scottish Charity Regulator",
       x = NULL, 
       y = NULL) -> regulatory_type)


quick_save(regulatory_type)





(scottish_char %>% 
  count(designated_religious_body) %>% 
  drop_na() %>% 
  mutate(per = percent(n/sum(n)), 
         designated_religious_body = 
           fct_reorder(designated_religious_body, n)
         ) %>% 
  ggploter()+
  labs(x = "Is a designated religous body?", 
       subtitle = "SOURCE: Scottish Charity Regulator",
       y = NULL) -> designated_religious_body)


quick_save(designated_religious_body)




## Activities
(scottish_char %>% 
  count(activities) %>% 
  drop_na() %>% 
  mutate(per = percent(n/sum(n)), 
         activities = 
           fct_reorder(activities, n)) %>% 
    filter(n > 2) %>% 
  ggploter() +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = NULL,
       y = "What type of activities do you do?", 
       subtitle = "SOURCE: Scottish Charity Regulator"
       ) -> activities )

quick_save(activities)

# Activities by funding


(scottish_char %>% 
  count(geographical_spread) %>% 
  drop_na() %>% 
  mutate(per = percent(n/sum(n)), 
         geographical_spread = 
           fct_reorder(geographical_spread, n)) %>% 
  ggploter() +
  coord_flip() +
  scale_fill_brewer(palette = "Set1") +
  labs(x = "Geographical spread" ,
       subtitle = "SOURCE: Scottish Charity Regulator",
       y = NULL) -> geographical_spread)

quick_save(geographical_spread)


## parent organisation country
## 
(scottish_char %>% 
  count(parent_charity_country_of_registration) %>% 
  drop_na() %>% 
  mutate(per = percent(n/sum(n)), 
         parent_charity_country_of_registration = 
           fct_reorder(parent_charity_country_of_registration, n)
         ) %>% 
  ggploter() +
  scale_fill_brewer(palette = "Spectral") +
  labs(x = 
         "Where is the parent organisation located (if there is one)?" ,
       subtitle = "SOURCE: Scottish Charity Regulator",
       y = NULL) -> parent_charity_country_of_registration)

quick_save(parent_charity_country_of_registration)


map(scottish_char, ~n_distinct(.x))


(scottish_char %>% 
    count(constitutional_form) %>% 
    drop_na() %>% 
    mutate(per = percent(n/sum(n)), 
           constitutional_form = 
             fct_reorder(constitutional_form, n)
    ) %>% 
    ggploter() +
    coord_flip()+
    scale_fill_viridis_d() +
    labs(x = 
           "What is the\nconstitutional form?" ,
         subtitle = "SOURCE: Scottish Charity Regulator",
         y = NULL) -> constitutional_form)

quick_save(constitutional_form)




## Section 4 ----
## 
##  