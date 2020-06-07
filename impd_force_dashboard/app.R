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
library(DT)


raw_data <- read.csv("IMPD_Use_Of_Force.csv") %>%
    clean_names()

# rename columns and change data types

clean_data <- raw_data %>%
    mutate(incident_num = incnum) %>%
    mutate(date = as.Date(occurred_dt)) %>%
    mutate(date_occurring = as.character(occurred_dt)) %>%
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
    select(incident_num, date, date_occurring, officer_number, division, district, disposition, street_num, street_name, type_of_force, reason_for_force, service, citizen_arrested, citizen_weapon, citizen_injured, citizen_hospitalized, officer_injured, officer_hospitalized, citizen_race, citizen_sex, officer_race, officer_sex, officer_age, officer_years_employed)

# remove duplicate rows

clean_data <- unique(clean_data) 

total_obs <- 20623

uof_data <- clean_data %>%
    filter(type_of_force != "") %>%
    group_by(type_of_force) %>%
    summarize(count = n(), na.rm = TRUE) %>%
    arrange(-count)

# takes number i in list of types of force and outputs graph for that type

uof_race_function <- function(i) {
    type <- uof_data[i, 1] %>%
        slice(1) %>%
        pull(1)
    count <- uof_data[i, 2] %>%
        slice(1) %>%
        pull(1)
    
    specific_data <- clean_data %>%
        filter(type_of_force == type) %>%
        group_by(citizen_race) %>%
        summarize(count = n()) %>%
        filter(citizen_race == "Black" | citizen_race == "White" | citizen_race == "Asian" | citizen_race == "Hispanic")
    
    asian_add <- c("Asian", 0)
    black_add <- c("Black", 0)
    hispanic_add <- c("Hispanic", 0)
    white_add <- c("White", 0)
    
    new_data <- specific_data
    
    if ("Asian" %in% new_data$citizen_race == FALSE){
        new_data <- rbind(new_data, asian_add)
    }
    if ("Black" %in% new_data$citizen_race == FALSE){
        new_data <- rbind(new_data, black_add)
    }
    if ("Hispanic" %in% new_data$citizen_race == FALSE){
        new_data <- rbind(new_data, hispanic_add)
    }
    if ("White" %in% new_data$citizen_race == FALSE){
        new_data <- rbind(new_data, white_add)
    }
    
    new_data <- new_data %>%
        mutate(citizen_race = factor(citizen_race, levels = c("Asian", "Black", "Hispanic", "White")))
    
    a <- new_data[1,2] %>%
        pull(1)
    b <- new_data[2,2] %>%
        pull(1)
    h <- new_data[3,2] %>%
        pull(1)
    w <- new_data[4,2] %>%
        pull(1)
    
    a <- as.numeric(a)
    b <- as.numeric(b)
    h <- as.numeric(h)
    w <- as.numeric(w)
    
    total <- a + b + h + w
    
    graph <- new_data %>%
        ggplot(aes(x = citizen_race, y = as.numeric(count), fill = citizen_race)) +
        geom_bar(stat = "identity") +
        geom_segment(aes(x = 0.5, xend = 1.5, y = total*.003, yend = total*.003)) +
        geom_segment(aes(x = 1.5, xend = 2.5, y = total*.556, yend = total*.556)) +
        geom_segment(aes(x = 2.5, xend = 3.5, y = total*.046, yend = total*.046)) +
        geom_segment(aes(x = 3.5, xend = 4.5, y = total*.395, yend = total*.395)) +
        labs(title = paste("Force Type: ", type, "\nRacial Breakdown (", count, " instances)", sep = ""), x = "Citizen Race", y = "Count")
    
    print(graph)
}


g1 <- uof_race_function(1)
g2 <- uof_race_function(2)
g3 <- uof_race_function(3)
g4 <- uof_race_function(4)
g5 <- uof_race_function(5)
g6 <- uof_race_function(6)
g7 <- uof_race_function(7)
g8 <- uof_race_function(8)
g9 <- uof_race_function(9)
g10 <- uof_race_function(10)
g11 <- uof_race_function(11)
g12 <- uof_race_function(12)
g13 <- uof_race_function(13)
g14 <- uof_race_function(14)
g15 <- uof_race_function(15)
g16 <- uof_race_function(16)
g17 <- uof_race_function(17)
g18 <- uof_race_function(18)

types_grid <- plot_grid(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15, g16, g17, g18, nrow = 3, ncol = 6)


# sample table

sample_data <- clean_data %>%
    select(incident_num, date_occurring, officer_number, division, street_num, street_name, type_of_force, citizen_arrested, citizen_weapon, citizen_injured, officer_injured, citizen_race, citizen_sex, officer_race, officer_sex, officer_age, officer_years_employed) %>%
    filter(citizen_race != "") %>%
    head(5)

impd_demo_data <- tibble(race = c("Asian", "Hispanic", "Black", "White"), percent = c(0.1, 3.0, 16.0, 79.6))
impd_demo_bar <- impd_demo_data %>%
    ggplot(aes(x = reorder(race, percent), y = percent, fill = race)) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none") +
    geom_text(aes(label=percent), hjust=-0.15, size=3.5) + 
    ylim(0, 82) +
    labs(x = "", title = "Racial Demographics of IMPD Force", y = "Percent of IMPD Force") +
    coord_flip()

indy_demo_data <- tibble(race = c("Asian", "Black", "Hispanic", "White"), percent = c(3.2, 28.3, 10.2, 61.4))
indy_demo_bar <- indy_demo_data %>%
    ggplot(aes(x = reorder(race, percent), y = percent, fill = race)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=percent), hjust=-0.15, size=3.5) + 
    ylim(0, 82) +
    theme(legend.position = "none") +
    labs(x = "", y = "Percent of Indianapolis Population", title = "Racial Breakdown of Indianapolis") +
    coord_flip()

# DEMOGRAPHICS OF USE OF FORCE

race_overview_uof_plot <- clean_data %>%
    group_by(citizen_race) %>%
    summarize(count = n()) %>%
    filter(citizen_race == "Asian" | citizen_race == "Black" | citizen_race == "Hispanic" | citizen_race == "White") %>%
    mutate(percent = round(100*(count/total_obs), 1)) %>%
    mutate(citizen_race = factor(citizen_race, levels = c("Asian", "Hispanic", "Black", "White"))) %>%
    ggplot(aes(x = citizen_race, y = percent, fill = citizen_race)) +
    geom_bar(stat = "identity") +
    theme(legend.position = "none") +
    geom_text(aes(label=percent), hjust=-0.15, size=3.5) + 
    ylim(0, 70) +
    labs(x = "", title = "IMPD Uses of Force By Race", y = "Percent") +
    coord_flip()


# Indianapolis dempgraphics modified to fit scale of previous plot

indy_demo_bar2 <- indy_demo_data %>%
    ggplot(aes(x = reorder(race, percent), y = percent, fill = race)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label=percent), hjust=-0.15, size=3.5) + 
    ylim(0, 70) +
    theme(legend.position = "none") +
    labs(x = "", y = "Percent of Indianapolis Population", title = "Racial Breakdown of Indianapolis") +
    coord_flip()


# data broken down by year

yearly <- clean_data %>%
    filter(citizen_race == "Black" | citizen_race == "White" | citizen_race == "Asian" | citizen_race == "Hispanic") %>%
    mutate(year = year(date)) %>%
    filter(year <= 2019) %>%
    group_by(year, citizen_race) %>%
    summarize(count = n())

yearly_plot <- ggplot(yearly, aes(x = year, y = count, fill = citizen_race)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = "Instances of Force over Time by Race", x = "Year", y = "No. of instances")

# daily data for calendar

daily <- clean_data %>%
    mutate(Date = date) %>%
    select(Date) %>%
    group_by(Date) %>%
    summarize(Instances = n())


# types of force used

use_of_force_chart <- clean_data %>%
    filter(type_of_force != "") %>%
    group_by(type_of_force) %>%
    summarize(count = n(), na.rm = TRUE) %>%
    ggplot(aes(x = reorder(type_of_force, -count), y = count)) +
    geom_bar(stat = "identity") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Type of Force Used", y = "Count", title = "IMPD Uses of Force by Type") +
    geom_text(aes(label=count), vjust=.33, hjust = -0.3, size=2.5, angle = 90) +
    ylim(0, 4700)

#508 instances where type not reported




# OFFICER/CITIZEN RACE HEATMAPS

# heatmap of observed race matrix

racial_data <- clean_data %>%
    select(citizen_race, officer_race) %>%
    filter(citizen_race == "Black" | citizen_race == "White" | citizen_race == "Asian" | citizen_race == "Hispanic") %>%
    filter(officer_race == "Black" | officer_race == "White" | officer_race == "Asian" | officer_race == "Hispanic") %>%
    group_by(citizen_race, officer_race) %>%
    summarize(count = n()) %>%
    ungroup() %>%
    add_row(citizen_race = "Asian", officer_race = "Asian", count = 0) %>%
    add_row(citizen_race = "Hispanic", officer_race = "Asian", count = 0)

observed_plot <- racial_data %>%
    ggplot(aes(x = citizen_race, y = officer_race, fill = count, color = "black")) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "orange2") +
    geom_text(aes(label=count), size = 4, color = "black") + 
    theme(legend.position = "none") + 
    labs(x = "Citizen Race", y = "Officer Race")


racial_data <- racial_data %>%
    arrange(citizen_race, officer_race)

b <- 20264

# Baseline matrix if distribution is assumed to be even across general use of force data

racial_baseline_uof <- tibble(citizen_race = c("Asian", "Asian", "Asian", "Asian",
                                               "Black", "Black", "Black", "Black",
                                               "Hispanic", "Hispanic", "Hispanic", "Hispanic",
                                               "White", "White", "White", "White"),
                              officer_race = c("Asian", "Black", "Hispanic", "White",
                                               "Asian", "Black", "Hispanic", "White",
                                               "Asian", "Black", "Hispanic", "White",
                                               "Asian", "Black", "Hispanic", "White"),
                              count = c(.000003*b, .00048*b, .00009*b, .0024*b,
                                        .00056*b, .089*b, .00736*b, .443*b,
                                        .000046*b, .00736*b, .01668*b, .0366*b,
                                        .000395*b, .0632*b, .0119*b, .3144*b)
) %>%
    mutate(count = round(count, 1))




uof_base_plot <- racial_baseline_uof %>%
    ggplot(aes(x = citizen_race, y = officer_race, fill = count, color = "black")) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "orange2") +
    geom_text(aes(label=count), size = 4, color = "black") + 
    theme(legend.position = "none") + 
    labs(x = "Citizen Race", y = "Officer Race", title = "Use of Force Distribution Baseline")


# Baseline matrix if distribution is assumed to be even across general Indianapolis population


racial_baseline_indy <- tibble(citizen_race = c("Asian", "Asian", "Asian", "Asian",
                                                "Black", "Black", "Black", "Black",
                                                "Hispanic", "Hispanic", "Hispanic", "Hispanic",
                                                "White", "White", "White", "White"),
                               officer_race = c("Asian", "Black", "Hispanic", "White",
                                                "Asian", "Black", "Hispanic", "White",
                                                "Asian", "Black", "Hispanic", "White",
                                                "Asian", "Black", "Hispanic", "White"),
                               count = c(.000032*b, .00512*b, .00096*b, .0255*b,
                                         .000283*b, .0453*b, .00849*b, .225*b,
                                         .000102*b, .01632*b, .00306*b, .0812*b,
                                         .000614*b, .0984*b, .0184*b, .4887*b)
) %>%
    mutate(count = round(count, 1))

indy_base_plot <- racial_baseline_indy %>%
    ggplot(aes(x = citizen_race, y = officer_race, fill = count, color = "black")) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "orange2") +
    geom_text(aes(label=count), size = 4, color = "black") + 
    theme(legend.position = "none") + 
    labs(x = "Citizen Race", y = "Officer Race", title = "Indianapolis Population Baseline")


# loss function

total <- 0

for (i in 1:16){
    o <- racial_data[i,3] %>%
        pull(1)
    e <- racial_baseline_uof[i,3] %>%
        pull(1)
    temp <- (o - e)
    total <- temp + total
}


expected_dst <- plot_grid(indy_base_plot, uof_base_plot, nrow = 1, ncol = 2)




# Define UI for application
ui <- fluidPage(
    theme = shinytheme("cerulean"),
    navbarPage(
        "Indianapolis Metro Police Department - Use of Force Dashboard",
        tabPanel("Introduction",
                h1("Introduction"),
                h1("Indianapolis Metro Police Department - Use of Force Dashboard"),
                p("This project uses public data to analyze the Indiana Metro Police Department's use of force since 2014. It uses data  from the OpenIndy Data Portal, part of Mayor Joe Hogsett's 'Disclose Indy' initative to ensure public access to Indianapolis' public records. The use of force is a sometimes-necessary way for a police department to conduct its operations, but a data-driven aggregate analysis of how officers use force is critical to identifying potential systemic biases, particularly across the dimension of race."),
                p("In order to identify biases, this project analyzes the IMPD's demographics as it relates to use of force, use of force over time, the types of force it uses, and the impact of officer race on use of force distributions; it also provides a searchable database of all uses of force by officer ID number and other factors. Finally, it includes notes on data cleaning and more information in the 'About' page for those interested in how this project was put together."),
                a(href = "https://www.indystar.com/story/news/local/indianapolis/2020/06/05/indianapolis-police-impd-submits-draft-new-use-force-policy/3155833001/", "Additionally, here is an updated version of the IMPD's use of force policy for reference."),
                p(""),
                h1("Key Findings:"),
                h3("All of this information and more can be found by navigating the tabs at the top of this site."),
                h3("1. The IMPD is far from representative of the city of Indianapolis."),
                h3("2. The IMPD's use of force across racial demographics is significantly biased against black citizens."),
                h3("3. White officers use force significantly more than others, particularly against black citizens."),
                h3("4. The incomplete and unstandardized nature of the IMPD's data makes analysis of trends across type of force and time difficult."),
                p(""),
                p(""),
                a(href = "https://github.com/willschrepf/IMPD_data", "All of this project's code is available on GitHub.")
                ),
        tabPanel("Demographics of the Use of Force",
                 titlePanel("Demographics of the Use of Force"),
                 h2("Demographics of the IMPD vs. Indianapolis"),
                 p("First off, it's worth comparing the racial breakdown of the IMPD as a whole to that of Indianapolis. Data is from the IMPD 2018 Annual Report and Census, respectively; the four main racial categories from the Census- Asian, Black, Hispanic, White- will be the same used for all racial data througout."),
                 a(href = "https://citybase-cms-prod.s3.amazonaws.com/d95654f9a1234d87afd0724cc65172c9.pdf", "Link to IMPD Demographic Data"),
                 p(""),
                 a(href = "https://www.census.gov/quickfacts/fact/table/indianapoliscitybalanceindiana/PST045219#qf-headnote-a", "Link to Census Data"),
                 plotOutput("impd_demo_bar", width = "750px"),
                 plotOutput("indy_demo_bar", width = '750px'),
                 h3("The IMPD is far from an accurate reflection of Indianapolis as a whole- white officers make up a disproportionate amount of the IMPD's force, while black, Asian, and Hispanic populations are underrepresented."),
                 h2("Demographics of Uses of Force vs. Indianapolis"),
                 p("Next, let's compare the demographics of the the use of force across citizens' race compared to Indianapolis as a whole."),
                 plotOutput("race_overview_uof_plot", width = "750px"),
                 plotOutput("indy_demo_bar2", width = "750px"),
                 h3("The distribution of the IMPD's use of force is drastically different than the racial distribution of Indianapolis as a whole. Black citizens are nearly twice as likely to have force used on them relative to their representation in the population of Indianapolis- they make up 28.3 percent of Indianapolis' population, yet account for 55.6 percent of the IMPD's uses of force.")
                 ),
        tabPanel("Types of Force",
                titlePanel("Types of Force"),
                h2("Frequency of Types of Force"),
                plotOutput("use_of_force_chart", width = "1250px", height = "500px"),
                h2("Racial Breakdown for Each Type of Force"),
                h3("Lines on each bar represent that race's baseline proportion of forced used; additionally, only types with more than 100 instances are shown."),
                plotOutput("types_grid", height = "1000px", width = "2000px"),
                h3("Another issue with this data is unclear labeling in the type of force- for instance, what is the difference between 'Taser' and 'Less Lethal - Taser'? In order to establish more useful analysis by type of force, particularly in order to allow for analysis of lethal force (which has fewer data and is therefore greatly affected by these inconsistencies) the IMPD should standardize its reporting process."),
                h3("However, even with this flaw in the data, it does not appear that there are any significant trends across types of force by race.")
                ),
        tabPanel("Impact of Officer Race",
                 titlePanel("Impact of Officer Race"),
                 h2("Expected Distribution of Use of Force by Citizen/Officer Race"),
                 p("These numbers are obtained by multiplying the proportion of each category in a baseline by the number of force instances"),
                 p("Note that two baselines are used- one baseline is the demographics of Indianapolis as a whole, and the other is the IMPD's use of force demographics."),
                 plotOutput("expected_dst", width = "1500px"),
                 h2("Actual Distribution"),
                 p("Here is the actual, observed distribution of force across citizen and officer race."),
                 plotOutput("observed_plot", width = "750px"),
                 h3("These results make clear that no matter which baseline is used, white officers use force on citizens of all races significantly more than officers of other races. Compared to Indianapolis as a whole, white officers use force on black citizens 103% more than is expected.")
                 ),
        tabPanel("Uses of Force Over Time",
                 h1("Uses of Force Over Time"),
                 h2("Use of Force Calendar"),
                 p("Note that dates are organized horizontally, not vertically. The numbers on each date are the uses of force that day."),
                 plotlyOutput("interactive_calendar", height = "800px"),
                 h2("Yearly Uses of Force"),
                 plotOutput("yearly_plot", width = "750px"),
                 h3("One of the primary issues with this data is its inconsistency over time. It seems unlikely that the IMPD's use of force has nearly doubled every year since 2014. So, analyzing this data over time seems to be impossible. The IMPD should fill in the gaps in this data and ensure consistent reporting practices going forward so that a better understanding can be reached of how force is used across days, weeks, months, and years.")
        ),
        tabPanel("Search Database",
                 titlePanel("Search Database"),
                 h3("Search through the data by any column using the bar on the right."),
                 h3("This dataset notably excludes officer names- it does not appear that the IMPD makes this information public."),
                 dataTableOutput("search_table"),
                 a(href = "http://data.indy.gov/datasets/be9ec4331d0242c6994a256073be772f_16", "Here is the OpenIndy data source.")
                 ),
        tabPanel("Notes on Data Cleaning",
                 titlePanel("Notes on Data Cleaning"),
                 h3("Here are a few notes on how, exactly, the data was cleaned for transparency's sake:"),
                 p("A few notes on structural changes made to filter data: data was downloaded directly from the site, but needed to be reformatted. Duplicate rows were removed, as it appears multiple reports were sometimes filed about the same instance (OpenIndy and the IMPD were contacted to get clarification on this, but the data will remain as is until information is received proving these assumptions wrong). Also, it appears that every type of force is counted as an instance- for example, if the same officer uses both a taser and physical pressure on the same citizen, that is logged as two separate instances. As mentioned before, this structural organization of the data was maintained, as it seemed critical to include every instance that officers saw fit to log."),
                 p("Several columns were also renamed in order to more directly convey their meaning. All told, there are 20,623 relevant instances in the data, which range from the beginning of 2014 to May 27, 2020 (the datw was downloaded on June 1). Each instance has information on: the date, IMPD division, district, disposition, street number and name, type of force used, reason for use of force, circumstances of the officer's involvement (service), details on the arrest, injury, or hospitalization of the citizen, injury or hospitalization of the officer,  demographic information on both parties, and the officer's ID number."),
                 p("Even with this cleaning, many issues remain with the data itself- thousands of instances are incompletely reported, analysis of the data over time indicates that data hasn't been fully reported from year to year (as can be seen in the 'Use of Force Over Time' section). Although this data is incomplete and flawed, though, it is still useful to conduct aggregate analysis in order to identify systemic trends.")
                 ),
        tabPanel("About",
                 titlePanel("About"),
                 h3("This project was developed by Will Schrepferman, a Harvard student studying Government + Data Science. He can be reached at willschrepferman@college.harvard.edu."),
                 p("This work is intended to provide both the public and police with an example of what data-driven policing can look like. Transparency and easy-to-understand communication of the data behind the IMPD's use of force is useful in order to ensure accountability. Additionally, having standardized, quality data performance indicators is critical for police to evaluate the efficacy of policy changes with regard to implicit bias, deescelation, and more."))
    )
)


## searchable DT

clean_dt <- clean_data %>%
    select(date, officer_number, district, street_num, street_name, type_of_force, citizen_arrested, citizen_hospitalized, officer_hospitalized, citizen_race, citizen_sex, officer_race, officer_sex)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$tbl <- renderTable(sample_data)
    output$impd_demo_bar <- renderPlot(impd_demo_bar)
    output$indy_demo_bar <- renderPlot(indy_demo_bar)
    output$race_overview_uof_plot <- renderPlot(race_overview_uof_plot)
    output$indy_demo_bar2 <- renderPlot(indy_demo_bar2)
    output$yearly_plot <- renderPlot(yearly_plot)
    output$use_of_force_chart <- renderPlot(use_of_force_chart)
    output$types_grid <- renderPlot(types_grid)
    output$expected_dst <- renderPlot(expected_dst)
    output$observed_plot <- renderPlot(observed_plot)
    output$search_table <- renderDataTable(clean_dt)
    output$interactive_calendar <- renderPlotly(
        print(ggplotly(daily %>%
                           mutate(year = year(Date),
                                  month = month(Date, label = TRUE),
                                  wkday = fct_relevel(wday(Date, label=TRUE),
                                                      c("Mon", "Tue","Wed","Thu","Fri","Sat","Sun")),
                                  day = day(Date),
                                  wk = format(Date, "%W")) %>%
                           filter(!is.na(year)) %>%
                           select(Date, year, month, wkday, day, wk, Instances) %>%
                           ggplot(aes(wk, wkday, fill = Instances, Date = Date)) +
                           geom_tile() +
                           scale_fill_gradient(low = "lightgreen", high = "orange") +
                           geom_text(aes(label=Instances), size = 3) +
                           labs(x = "", y = "", title = "Calendar Map of Use of Force", fill = "Instances", subtitle = "Number on Square is How Many Instances Were Used That Day") +
                           theme(panel.background = element_blank(),
                                 axis.ticks = element_blank(),
                                 axis.text.x = element_blank(),
                                 strip.background = element_rect("grey92")
                           ) +
                           facet_grid(year~month, scales="free", space="free"), tooltip = c("Date", "fill")))
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
