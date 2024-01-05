library(readr)
library(shiny)
library(DT)
library(dplyr)
library(openxlsx)
#Leer csv
worldcup <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-29/worldcups.csv")

unique_years <- unique(worldcup$year)

ui <- fluidPage(
    titlePanel("Mundiales de Fútbol"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("year", "Año", min(unique_years), max(unique_years), value = mean(unique_years)),
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Dashboard", DT::dataTableOutput("table"), plotOutput("barplot", width = "100%"))
            )
        )
    )
)

server <- function(input, output) {
    output$table <- DT::renderDataTable({
        if(input$year %in% unique_years){
            data <- worldcup %>%
            filter(year == input$year) %>%
            select(winner, second, third)
        } else {
            closest_years <- unique_years[which.min(abs(unique_years - input$year))]
            message <- paste("Este año no se jugó un mundial de fútbol. El mundial más cercano a la fecha seleccionada se jugó en el año ", closest_years[1])
            data <- data.frame(Aviso = message)
        }
    })
    output$barplot <- renderPlot({
        databar <- worldcup %>%
        filter(year <= input$year) %>%
        select(host, attendance)
        
        barplot(databar$attendance, names.arg = databar$host, horiz = TRUE, las = 1, xlab = "Asistencia", main = "Asistencia a los mundiales por país sede")
    })
}

shinyApp(ui, server)