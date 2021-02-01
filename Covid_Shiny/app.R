#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinythemes)
theme_set(theme_minimal())


our_world_in_data <- read_csv(
    file = "https://covid.ourworldindata.org/data/owid-covid-data.csv", 
    col_types = cols(
        .default = col_double(),
        iso_code = col_character(),
        continent = col_character(),
        location = col_character(),
        date = col_date(),
        tests_units = col_character()
    )
) 


# Define UI for application
ui <- navbarPage(theme = shinytheme("flatly"), title = "Covid19-Data",
                 
    tabPanel("Cases",
        # Plots
        tabsetPanel(
            tabPanel("Total Cases", plotOutput('total_cases')), 
            tabPanel("Cases/100k", plotOutput('Cases_100k'))
            ),
    
        # separating line   
        hr(),

        # Continent-input & slider-bar for dates
        fluidRow(
            column(3,
                selectInput(inputId = 'mycontinent', label = "Continent", choices = unique(our_world_in_data$continent))
                ),
                
            column(width = 8, offset = 1,
                sliderInput(inputId = 'mydate', label = "Date", 
                            min = as.Date("2020-01-22", "%Y-%m-%d"),
                            max = Sys.Date()-1,
                            value = Sys.Date()-1)
                )
        )),

    tabPanel("Deaths",
         # Plots
         tabsetPanel(
             tabPanel("Total Deaths", plotOutput('total_deaths')), 
             tabPanel("Deaths/100k", plotOutput('deaths_100k'))
         ),
         
         # separating line   
         hr(),
         
         # Continent-input & slider-bar for dates
         fluidRow(
             column(3,
                    selectInput(inputId = 'mycontinent2', label = "Continent", choices = unique(our_world_in_data$continent))
             ),
             
             column(width = 8, offset = 1,
                    sliderInput(inputId = 'mydate2', label = "Date", 
                                min = as.Date("2020-01-22", "%Y-%m-%d"),
                                max = Sys.Date()-1,
                                value = Sys.Date()-1)
             )
         )))


# Define server logic 
server <- function(input, output) {
    
    # plots for 'Cases' tab
    output$total_cases <- renderPlot({
        our_world_in_data %>% 
        select(c(date, continent, location, total_cases)) %>% 
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
        filter(continent == input$mycontinent) %>%
        filter(date == input$mydate) %>% 
        ggplot(aes(total_cases, reorder(location, total_cases),
                   fill = ifelse(total_cases == max(total_cases), "highlighted", "normal")))+
        geom_bar(stat = "identity", width = 0.6)+
            scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
            labs(x = "Total Cases", y = " ") +
                theme(legend.position = "none") 
    })
    
    output$Cases_100k <- renderPlot({
        our_world_in_data %>% 
            select(c(date, continent, location, total_cases_per_million)) %>% 
            mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
            mutate(total_cases_per_100k = total_cases_per_million/10) %>% 
            filter(continent == input$mycontinent) %>%
            filter(date == input$mydate) %>% 
            ggplot(aes(total_cases_per_100k, reorder(location, total_cases_per_100k),
                       fill = ifelse(total_cases_per_100k == max(total_cases_per_100k), "highlighted", "normal")))+
            geom_bar(stat = "identity", width = 0.6)+
            scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
            labs(x = "Total Cases / 100.000", y = " ") +
            theme(legend.position = "none") 
    })
    
    # plots for 'deaths' tab
    output$total_deaths <- renderPlot({
        our_world_in_data %>% 
            select(c(date, continent, location, total_deaths)) %>% 
            mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
            filter(continent == input$mycontinent2) %>%
            filter(date == input$mydate2) %>% 
            ggplot(aes(total_deaths, reorder(location, total_deaths),
                       fill = ifelse(total_deaths == max(total_deaths), "highlighted", "normal")))+
            geom_bar(stat = "identity", width = 0.6)+
            scale_fill_manual(name = "total_deaths", values=c("red","lightsalmon")) +
            scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
            labs(x = "Total Deaths", y = " ") +
            theme(legend.position = "none") 
    })
    
    output$deaths_100k <- renderPlot({
        our_world_in_data %>% 
            select(c(date, continent, location, total_deaths_per_million)) %>% 
            mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
            mutate(total_deaths_per_100k = total_deaths_per_million/10) %>% 
            filter(continent == input$mycontinent2) %>%
            filter(date == input$mydate2) %>% 
            ggplot(aes(total_deaths_per_100k, reorder(location, total_deaths_per_100k),
                       fill = ifelse(total_deaths_per_100k == max(total_deaths_per_100k), "highlighted", "normal")))+
            geom_bar(stat = "identity", width = 0.6)+
            scale_fill_manual(name = "total_deaths_per_100k", values=c("red","lightsalmon")) +
            scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
            labs(x = "Total Deaths / 100.000", y = " ") +
            theme(legend.position = "none") 
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
