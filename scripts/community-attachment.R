

library(tidyverse)
library(fs)
library(glue)
library(janitor)
library(scales)
library(ggfittext)
library(ggtext)

shs2018_social_public <-
  read_delim(
    "scottish-household/scottish-household-2018/tab/shs2018_social_public.tab",
    "\t",
    escape_double = FALSE,
    trim_ws = TRUE
  )
View(shs2018_social_public)





dir_ls(recurse = T) %>% 
  str_subset("8617_shs2018_questionnaire.pdf") 

## To View the questionnaire
system("open scottish-household/scottish-household-2018/mrdoc/pdf/8617_shs2018_questionnaire.pdf")


    ######################
    ### Rate community ###
    ######################

"THINKING NOW ABOUT THE NEIGHBOURHOOD YOU LIVE IN, HOW
WOULD YOU RATE IT AS A PLACE TO LIVE?" %>% 
  str_to_sentence() -> rb1_question


# SHOWCARD J1

rb1_answer <- c(
  paste(c("very", "fairly"), "good"), 
  paste(c("fairly", "very"), "poor"), 
  "no opinion", 
  NA)


rate_neighbourhood_df <- shs2018_social_public %>% 
  count(rb1) %>% 
  mutate(rate_neighbourhood = rb1_answer) %>% 
  right_join(shs2018_social_public, by = "rb1") %>% 
  select(UNIQIDNEW, rate_neighbourhood) %>% 
  mutate(rate_neighbourhood_n = case_when(
    rate_neighbourhood == "very good" ~ 4,
    rate_neighbourhood == "fairly good" ~ 3,
    rate_neighbourhood == "fairly poor" ~ 2,
    rate_neighbourhood == "very poor" ~ 1,
    rate_neighbourhood == "no opinion" ~ NA_real_
  ))


        ###########################
        ### Belong neighborhood ###
        ###########################


"HOW STRONGLY DO YOU FEEL YOU BELONG TO YOUR IMMEDIATE
NEIGHBOURHOOD?" %>% 
  str_to_sentence() -> commbel_question


commbel_answer <- c(
  paste(c("very", "fairly", "not very", "not at all"), "strongly"), 
  "don't know", 
  NA
)

(belong_neighbourhood <- shs2018_social_public %>% 
  count(commbel) %>% 
  mutate(commbel_answer) %>% 
  right_join(shs2018_social_public, by = "commbel") %>% 
  select(UNIQIDNEW, 
         belong_neighbourhood = commbel_answer) %>% 
  mutate(belong_neighbourhood_n = case_when(
    belong_neighbourhood == "very strongly" ~ 4,
    belong_neighbourhood == "fairly strongly" ~ 3,
    belong_neighbourhood == "not very strongly" ~ 2,
    belong_neighbourhood == "not at all strongly" ~ 1,
    belong_neighbourhood == "don't know" ~ NA_real_,
  )))

        #########################
        ### Common occurrence ###
        #########################

"HOW COMMON WOULD YOU SAY THE FOLLOWING THINGS ARE IN THIS
NEIGHBOURHOOD? CODE IN GRID" %>% 
  str_to_sentence() -> asb1_question

asb1_categories <- 
c("Noisy neighbours or regular loud parties",
  "Vandalism, graffiti or other deliberate damage to property",
  "Rubbish or litter lying around",
  "Neighbour disputes",
  "Groups or individuals intimidating or harassing others",
  "Drug misuse or dealing",
  "Rowdy behaviour e.g. drunkenness, hooliganism or loutish behaviour",
  "Abandoned or burnt out vehicles",
  "Animal nuisance such as noise or dog fouling")


asb1_aswers <- c(
paste(c("very", 
        "fairly", 
        "not very", 
        "not at all"), 
      "common"), 
"don't know",
NA)
 

asb1_topics <- paste0("asb1", letters[1:9])


asb1_df <- 
  map2(asb1_topics, asb1_categories, ~{

.new_vary <- shs2018_social_public[.x] 

.recoded_vary <- case_when(
  .new_vary[1] == 1 ~ "very common",
  .new_vary[1] == 2 ~ "fairly common",
  .new_vary[1] == 3 ~ "not very common",
  .new_vary[1] == 4 ~ "not at all common",
  .new_vary[1] == 5 ~ "don't know", 
  is.na(.new_vary) ~ NA_character_
  ) 

.recoded_vary %>% 
  as_tibble() %>% 
  set_names(.y) %>% 
  clean_names()

}) %>% 
  bind_cols()  %>% 
  mutate(UNIQIDNEW = shs2018_social_public$UNIQIDNEW) %>% 
  select(UNIQIDNEW, everything())



# Bind together ------------

belong_neighbourhood



rate_neigh_order <- c(
  "very good" , 
  "fairly good", 
  "no opinion",
  "fairly poor",
  "very poor" ) 


.df_full <- 
rate_neighbourhood_df %>% 
  left_join(asb1_df, by = "UNIQIDNEW") 



.df_ls <- map(.df_full, 
    ~{
      .df_full %>% 
        group_by(rate_neighbourhood, !!.x) %>% 
        summarise(n = n()) %>% 
        group_by(rate_neighbourhood)%>% 
        mutate(prop = n/sum(n)) %>% 
        rename(vary = 2) %>% 
        mutate(vary = as.character(vary))
    })



.df_ls_v2 <- map(.df_ls, ~{
  
  .x$vary <- fct_relevel(.x$vary, 
  "very common", 
  "fairly common", 
  "not very common", 
  "not at all common", 
  "don't know")
  
  .x$rate_neighbourhood <- fct_relevel(
    .x$rate_neighbourhood, 
    "very good", 
    "fairly good", 
    "no opinion", 
    "fairly poor", 
    "very poor") 
  
  return(.x)
  
})



.df$noisy <- fct_relevel(.df$noisy, 
             "very common", 
             "fairly common", 
             "not very common", 
             "not at all common")


.df$rate_neighbourhood <- fct_relevel(
           .df$rate_neighbourhood, 
            "very good", 
            "fairly good", 
            "no opinion", 
            "fairly poor", 
            "very poor") 



asb1_cols <- 
  rev(c("skyblue4", "skyblue", "tomato", "tomato4"))

names(asb1_cols) <- asb1_aswers[1:4] 



paste("How", 
      str_to_lower(
        asb1_categories[1]), "impacts views on one\'s neighbourhood.")



titles. <-
glue("How ***{str_to_lower(asb1_categories)}*** impacts <br> how one rates their neighbourhood") 



neighbourhood_ratings_gg_ls <- imap(.df_ls_v2[-c(1:3)],
    ~{
      
      .y <- str_to_lower(.y) %>% 
        str_replace_all("_", " ")
      
      .title <- glue("Comparing the amount of ***{.y}*** and <br>   the impact on ***neighbourhood ratings***")
      
      .x %>% 
        drop_na() %>%
        filter(rate_neighbourhood != "no opinion",
               vary != "don't know") %>%
        ggplot(aes(
          x =  rate_neighbourhood, 
          y = prop,
          label = percent(prop, accuracy = 1),
          fill = vary
        )) +
        geom_col(position = "stack") +
        geom_bar_text(position = "stack", 
                      reflow = TRUE, 
                      min.size = 12) +
        scale_fill_manual(values = asb1_cols, 
                          name = NULL, 
                          labels = str_to_title) +
        scale_y_continuous(labels = percent) +
        theme(text = element_text(size = 16),
              axis.title.x = element_markdown(),
              plot.title = element_markdown(size = 18), 
              plot.caption  = element_markdown(hjust = 0),
              plot.title.position = "plot",
              plot.caption.position = "plot",
              plot.margin = margin(1,1,1,1, "cm")) +
        labs(title = str_wrap(.title, 50),
             subtitle = 
          "SOURCE: Scottish Household Survey 2018", 
             x =
        "***How would you rate your neighbourhood?***",
             y = NULL, 
             caption = "*Created by Dr Elliot Meador*") 
      
    }) 



imap(neighbourhood_ratings_gg_ls, ~{
  ggsave(.x, 
         filename = glue("{.y}.png"),
         width = 13,
         height = 8, 
         dpi = 350)
})

dir_create("~/OneDrive/module/community-project-management-2021/content/1-theory-analysis/figures")


dir_copy(".", "~/OneDrive/module/community-project-management-2021/content/1-theory-analysis/figures/")



names(neighbourhood_ratings_gg_ls) %>% 
  str_to_title() %>% 
  str_replace_all("_", " ") %>% 
  paste(collapse = "; ")












