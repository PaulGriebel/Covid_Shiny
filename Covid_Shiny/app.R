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
library(lubridate)

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


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid19 Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = 'mycontinent', label = "continent", choices = c("Europe", "Asia")),
            dateInput(inputId = 'mydate', label = "date"),
        ),
        
    mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        our_world_in_data %>% 
        select(c(date, continent, location, total_cases, total_deaths)) %>% 
        mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
        filter(continent == input$mycontinent) %>%
        filter(date == input$mydate) %>% 
        ggplot(aes(total_cases, location))+
        geom_bar(stat = "identity")+
            scale_x_continuous()
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
