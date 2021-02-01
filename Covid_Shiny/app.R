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
ui <- fluidPage(theme = shinytheme("flatly"),

    # Application title
    titlePanel("Covid19 Data"),

    # Sidebar with a slider input for dates
    sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput(inputId = 'mycontinent', label = "continent", choices = c("Europe", "Asia")),
            sliderInput(inputId = 'mydate', label = "date", 
                        min = as.Date("2020-01-22", "%Y-%m-%d"),
                        max = Sys.Date()-1,
                        value = Sys.Date()-1),
        ),
        
    mainPanel(
        width = 10,
        tabsetPanel(
            tabPanel("Cases", plotOutput('total_cases')), 
            tabPanel("Cases/100k", plotOutput('Cases_100k'))
        )
    )
))

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$total_cases <- renderPlot({
        our_world_in_data %>% 
        select(c(date, continent, location, total_cases)) %>% 
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
        filter(continent == input$mycontinent) %>%
        filter(date == input$mydate) %>% 
        ggplot(aes(total_cases, reorder(location, total_cases),
                   fill = ifelse(total_cases == max(total_cases), "highlighted", "normal")))+
        geom_bar(stat = "identity")+
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
            geom_bar(stat = "identity")+
            scale_x_continuous(labels = function(x) format(x, scientific = FALSE)) +
            labs(x = "Total Cases / 100.000", y = " ") +
            theme(legend.position = "none") 
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
