#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(dplyr)
library(janitor)
library(gt)
library(lubridate)
library(cowplot)
library(plotly)
library(scales)


raw_data <- read.csv("IMPD_Use_Of_Force.csv") %>%
    clean_names()

# rename columns and change data types

clean_data <- raw_data %>%
    mutate(incident_num = incnum) %>%
    mutate(date = as.Date(occurred_dt)) %>%
    mutate(division = as.character(udtext24a)) %>%
    mutate(district = as.character(udtext24b)) %>%
    mutate(disposition = as.character(disposition)) %>%
    mutate(street_num = as.numeric(street_n)) %>%
    mutate(street_name = as.character(street)) %>%
    mutate(type_of_force = as.character(uof_force_type)) %>%
    mutate(reason_for_force = as.character(uof_reason)) %>%
    mutate(service = as.character(service_type)) %>%
    mutate(citizen_arrested = as.character(cit_arrested)) %>%
    mutate(citizen_weapon = as.character(cit_weapon_type)) %>%
    mutate(citizen_injured = as.character(cit_injured)) %>%
    mutate(citizen_hospitalized = as.character(cit_hospital)) %>%
    mutate(officer_injured = as.character(off_injured)) %>%
    mutate(officer_hospitalized = as.character(off_hospital)) %>%
    mutate(citizen_race = as.character(race)) %>%
    mutate(citizen_sex = as.character(sex)) %>%
    mutate(officer_number = as.character(offnum)) %>%
    mutate(officer_race = as.character(off_race)) %>%
    mutate(officer_sex = as.character(off_sex)) %>%
    mutate(officer_age = as.character(off_age)) %>%
    mutate(officer_years_employed = as.numeric(off_yr_employ)) %>%
    select(incident_num, date, officer_number, division, district, disposition, street_num, street_name, type_of_force, reason_for_force, service, citizen_arrested, citizen_weapon, citizen_injured, citizen_hospitalized, officer_injured, officer_hospitalized, citizen_race, citizen_sex, officer_race, officer_sex, officer_age, officer_years_employed)

# remove duplicate rows

clean_data <- unique(clean_data) 

# sample gt table

sample_data <- clean_data %>%
    head(5) %>%
    gt()

# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    navbarPage(
        "Indianapolis Metro Police Department - Use of Force Dashboard",
        tabPanel("Introduction",
                titlePanel("Introduction"),
                h3("Indianapolis Metro Police Department - Use of Force Dashboard"),
                p("This project uses public data to analyze the Indiana Metro Police Department's use of force since 2014. It uses data  from the OpenIndy Data Portal, part of Mayor Joe Hogsett's 'Disclose Indy' initative to ensure public access to Indianapolis' public records. The use of force is a sometimes-necessary way for a police department to conduct its operations, but a data-drivena aggregate analysis of how officers use force is critical to identifying potential systemic biases, particularly across the dimension of race."),
                p("In order to identify biases, this project analyzes the IMPD's use of force over time, the types of force it uses, and the impact of officer race on use of force distributions; it also provides a searchable database of all uses of force by officer ID number and other factors. Finally, it includes notes on data cleaning and more information in the 'About' page for those interested in how this project was put together."),
                h3("The Data:"),
                p("The data provided by IMPD is far from perfect- there were many duplicates, incomplete fields, and unclear variable names. The specifics of how the data was cleaned can be found in the 'Notes on Data Cleaning' Section, but here are a few sample rows of the cleaned data used:")
                ),
        tabPanel("Uses of Force Over Time",
                titlePanel("Uses of Force Over Time"),
                h3("TODO")),
        tabPanel("Types of Force",
                titlePanel("Types of Force"),
                h3("TODO")),
        tabPanel("Impact of Officer Race",
                 titlePanel("Impact of Officer Race"),
                 h3("TODO")),
        tabPanel("Search Database",
                 titlePanel("Search Database"),
                 h3("TODO")),
        tabPanel("Notes on Data Cleaning",
                 titlePanel("Uses of Force Over Time"),
                 h3("TODO")),
        tabPanel("About",
                 titlePanel("About"),
                 h3("TODO"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)
