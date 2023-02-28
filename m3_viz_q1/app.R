library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# m3_viz_q1


mort_df <- read.csv('data/cleaned-cdc-mortality-1999-2010-2.csv')
mort_df <- mort_df %>% filter(Year == 2010)

mort_df$ICD <- replace(mort_df$ICD,mort_df$ICD=="Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism","Diseases of the blood and blood-forming organ")
mort_df$ICD <- replace(mort_df$ICD,mort_df$ICD=="Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified"  ,"Symptoms not elsewhere classified")
mort_df$ICD <- replace(mort_df$ICD,mort_df$ICD=="Diseases of the musculoskeletal system and connective tissue" , "Diseases of the musculoskeletal system")
mort_df$ICD <- replace(mort_df$ICD,mort_df$ICD=="Certain conditions originating in the perinatal period" , "Conditions / perinatal period" )
mort_df$ICD <- replace(mort_df$ICD,mort_df$ICD=="Congenital malformations, deformations and chromosomal abnormalities" ,  "Congenital malformations, and chromosomal abnormalities")

disease_lst <- unique(mort_df$ICD)



ui <- fluidPage(

  div(id='header',
    titlePanel("Mortality by State"),
  ),

  div(id='input',
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

      mort_df %>% filter(ICD == input$disease) %>%
        #arrange(Crude.Rate) %>%
        plot_ly(y = ~State,x = ~Crude.Rate,
                color = ~State,
                colors = c("lightgray","darkblue"),
                showlegend = FALSE, type="bar"
        ) %>%
        layout(
          yaxis = list(categoryorder = "total ascending"),
          title = input$disease
        )

    })
}

# Run the application
shinyApp(ui = ui, server = server)
