## ----------------------
## Title:   Community Reframing Activity
## Purpose: -unpack and organise activity 
## Date:    21 February 2021
## ----------------------

library(tidyverse)
library(scales)
library(pdftools)
library(fs)
library(zip)

# Change working directory to Community Project Management home folder in OneDrive

current_wd <- getwd()

comm_proj <-"~/OneDrive/module/community-project-management-2021/"

setwd(comm_proj)

#setwd(current_wd)

## Copy files from download ----

zip_list("~/Downloads/CPM (Edinburgh)- Reverse-framing Community Capitals-357324.zip")


unzip(zipfile = "~/Downloads/CPM (Edinburgh)- Reverse-framing Community Capitals-357324.zip",
    exdir  =  "activities/reverse-framing-capitals")


## Clean file names ----

 # get old names
old_names <- list.files(
  "activities/reverse-framing-capitals", 
  full.names = T, 
  recursive = T) 

 # create some new cleaner names
new_names <- 
  old_names %>% 
  str_to_lower() %>% 
  str_remove_all(",") %>% 
  str_replace_all("_", "-") %>% 
  str_replace_all(" ", "-") %>% 
  str_replace_all("--", "-") %>% 
  str_replace_all("-.docx", ".docx")

# copy the new named files into the 
# same directory 
file.copy(old_names, new_names, overwrite = T)
  
# remove other files. 
unlink(old_names)
  

 # remove downloaded files to save room
unlink("~/Downloads/CPM (Edinburgh)- Reverse-framing Community Capitals-357324.zip")









