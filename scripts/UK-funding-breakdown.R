## ----------------------
## Title:   UK-funding breakdown
## Purpose: Plots for UK funding breakdown
## Date: 11 April 2021
## ----------------------

library(tidyverse)
library(scales)
library(pdftools)
library(fs)
library(janitor)
library(ggfittext)
# Change working directory to Community Project Management home folder in OneDrive

current_wd <- getwd()

comm_proj <- "~/OneDrive/module/community-project-management-2021/"

setwd(comm_proj)

#setwd(current_wd)









## Section 2 ----
UK_funding_df <- read_csv("content/4-funding/UK-general-grant-spending-classification-2018-2019.csv")

pounds <- scales::label_number_si(unit = "£")

label_pounds <- scales::label_number_si(accuracy = 0.1)


col_fun <- colorRampPalette(brewer_pal(palette = "Set2")(9))


theme_set(theme_minimal() +
            theme(plot.title.position = "plot", 
                  plot.caption.position = "plot", 
                  text = element_text(
                    color = "black", 
                    size = 15
                  ), 
                  plot.margin = margin(
                    1.5,1.5,1.5,1.5,"cm"
                  )))

## Total grant spending ----
(total_spend_category <- UK_funding_df %>% 
  group_by(category) %>% 
  summarise(category_total = sum(sub_total)
            ) %>% 
  mutate(category = fct_reorder(
    category, category_total, 
  ), 
  percent = percent(category_total/sum(category_total, accuracy = .1))) %>% 
  ggplot(aes(category, category_total, 
             label = percent)) +
  geom_col(aes(fill = category), 
           color = "black",
           show.legend = F, size = .1) +
  geom_bar_text(aes(fill = category), 
                show.legend = F, 
                contrast = TRUE) +
  scale_y_continuous(labels = pounds, breaks = pretty_breaks()) +
  scale_fill_manual(values = col_fun(12)) +
  coord_flip() +
  labs(title = "Total UK spending by category",
       subtitle = "SOURCE: Government Grant Statistics¹",
       caption = "¹Data shown are from 2018 - 2019.",
       x = NULL, 
       y = NULL))



ggsave(total_spend_category, 
       filename = "content/4-funding/figures/UK-total-spend-category.png", 
       width = 11, 
       height = 8, 
       dpi = 400)

## Section 3 ----



sub_category_plots_ls <- 
UK_funding_df %>% 
  split(.$category) %>% 
  imap(~{
    
    
    total <- pounds(sum(.x$sub_total, 
                        na.rm = T))
    
    .x %>% 
      mutate(sub_category = fct_reorder(
        sub_category, sub_total
      ), 
      percent = 
        percent(sub_total/sum(sub_total))) %>% 
      ggplot(aes(sub_category, 
                 sub_total ,
                 label = percent)) +
      geom_col(aes(fill = sub_category), 
               color = "black",
               show.legend = F, 
               size = .1) +
      geom_bar_text(
        aes(
          fill = sub_category
          ), 
                    show.legend = F, 
                    contrast = TRUE) +
      scale_y_continuous(
        labels = pounds, 
        breaks = pretty_breaks()) +
      scale_fill_manual(values = col_fun(12)) +
      coord_flip() +
      labs(title = 
             paste0("Subcategory: ", 
                    .y, " (", total, ")"), 
           subtitle = "SOURCE: Government Grant Statistics¹",
           caption = "¹Data shown are from 2018 - 2019.",
           x = NULL, 
           y = NULL)
      
  })

sub_category_paths_ls <-
names(sub_category_plots_ls) %>% 
  make_clean_names(prefix = "content/4-funding/figures/subcategory-",
                   sep_out = "-", 
                   postfix = ".png") 
  

map2(sub_category_plots_ls, 
     sub_category_paths_ls, 
     ~{
ggsave(.x, 
       filename = .y, 
       width = 11, 
       height = 8, 
       dpi = 400)
     })



zip.path <- "~/Downloads/inkscape-open-symbols-master.zip"

new_dir <- "~/.config/inkscape/symbols"

library(zip)
dir_tree(new_dir)

unzip(zipfile = zip.path, exdir = new_dir)







## Section 4 ----