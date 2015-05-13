library(shiny)

shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  titlePanel("Schelling Viz"),  
  navlistPanel(
    "Spatial Results",
    tabPanel("Visualizing the course of a simulation",

              fluidRow(
                
               column(6,
                      sliderInput("step", label = "Simulation Step",
                           min = 0, max = 99, value = 0, step = 1, animate=F)),
               column(6,
                      selectInput("var", label = "Quantity to map",
                                  choices = c("Total Population" = "totalPop",
                                              "% Greens" = "pctgreens",
                                              "% Reds" = "pctreds"
                                              ), selected = "% Greens"))
             ),
             plotOutput("map_cell")
    )
    
)
))
