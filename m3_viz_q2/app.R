library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyverse)

# m3_viz_q2


source("helpers.R")



st_df <- loadStateMortalityData()
us_df <- loadUSMortalityData()
disease_lst <- unique(df$Disease_ID)
st_lst <- unique(df$State)



ui <- fluidPage(

  div(id='header',
    titlePanel("Mortality by State"),
  ),

  div(id='input',
          selectInput(
            "state",
            label="Choose a state",
            choices=st_lst
          ),
          selectInput(
            "disease",
            label="Choose a cause of death",
            choices=disease_lst
          ),
    ),

  div(id='input',
      plotlyOutput("distPlot", height = "800px", width = "800px"),
  )

)

# Define server logic required to draw a bar chart
server <- function(input, output) {

    output$distPlot <- renderPlotly({

      stfl_df <-filterStateDiseaseDF(st_df,  input$state, input$disease)
      usfl_df <- filterUSDiseaseDF(us_df, input$disease)


      fig <- plot_ly()

      fig <- fig %>% add_trace(data=usfl_df, x = ~Year, y = ~Crude.Rate,
                               name='Crude.Rate.US',
                               type = 'scatter', mode = 'markers',
                               line = list(color = 'red', width = 2, dash = 'dash'),
                               marker = list(color='red', size=8))

      fig <- fig %>% add_trace(data=stfl_df,x = ~Year, y = ~Crude.Rate,
                               name = 'Crude.Rate.ST',
                               type = 'scatter', mode = 'lines+markers',
                               line = list(color = 'steelblue', width = 4),
                               marker = list(color='steelblue', size=8))
      fig


    })
}

# Run the application
shinyApp(ui = ui, server = server)
