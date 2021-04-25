## ----------------------
## Title:  Calendar
## Purpose: Create a calander visualasation 
##          for the final month
## Date: 25 April 2021
## ----------------------

library(tidyverse)
library(scales)
library(pdftools)
library(fs)
library(calendR)
library(lubridate)

# Change working directory to Community Project Management home folder in OneDrive

current_wd <- getwd()

comm_proj <-"~/OneDrive/module/community-project-management-2021/"

setwd(comm_proj)

#setwd(current_wd)


## for help
browseURL("https://r-coder.com/calendar-plot-r/")




## Special days
special_day_v <-
  c(
    3, 
    10, 
    17, 
    24, 
    31
  )



special_colour_v <- 
  c(
    "skyblue", 
    "grey", 
    "grey",
    "grey", 
    "grey"
  )



text_v <- c(
  "One-on-one feedback\n NO in-class lecture", 
  "Course content Review", 
  "Work on Assessments\n NO in-class lecture", 
  "Assessment 1 due\n(Report)\n NO in-class lecture", 
  "Assessment 2 due\n(Funding application)\n NO in-class lecture"
)



# make calander
(final_month_calander <- 
    calendR(year = 2021,
            month =5, 
            special.days = special_day_v,
            special.col = special_colour_v,
            text = text_v,
            text.pos =  special_day_v, 
            legend.pos = "bottom",
            
            start = "M",  
            mbg.col = "skyblue",               
            months.col = "white",      
            lty = 0,                   
            bg.col = "#f4f4f4",       
            title = "Community Project Management (Edinburgh)", 
            subtitle = "May 2021",
            title.size = 25,   
            subtitle.size = 20,
            orientation = "p"))         


# Save

ggsave(final_month_calander, 
       filename = "content/5-putting-it-together/figures/May-calander.png", 
       width = 13, 
       height = 9, 
       dpi = 400)


system("open content/5-putting-it-together/5-lecture.pptx")







