library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# m3_viz


mort_df <- read.csv('data/cleaned-cdc-mortality-1999-2010-2.csv')
mort_df <- mort_df %>% filter(Year == 2010)

disease_lst <- unique(mort_df$ICD.Chapter)



ui <- fluidPage(

    titlePanel("Mortality by State"),

    sidebarLayout(
        sidebarPanel(
          selectInput(
            "disease",
            label="Choose a cause of death",
            choices=disease_lst
          )
        ),

        mainPanel(
           #plotOutput("distPlot")
           #box(plotlyOutput("distPlot", height = "800px", width = "800px"))
            plotlyOutput("distPlot", height = "800px", width = "800px")
        )
    )

)

# Define server logic required to draw a bar chart
server <- function(input, output) {


    output$distPlot <- renderPlotly({

      mort_df %>% filter(ICD.Chapter == input$disease) %>%
        #arrange(Crude.Rate) %>%
        plot_ly(y = ~State,x = ~Crude.Rate,
                color = ~State,
                colors = c("lightgray","darkblue"),
                showlegend = FALSE, type="bar"
        ) %>%
        layout(yaxis = list(categoryorder = "total ascending"))

    })
}

# Run the application
shinyApp(ui = ui, server = server)
