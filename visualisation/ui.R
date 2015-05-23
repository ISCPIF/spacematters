library(shiny)

shinyUI(fluidPage(
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  titlePanel("Schelling Viz"),  
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    checkboxInput('header', 'Header', FALSE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"')
  ),
   mainPanel(

              fluidRow(
                
               column(6,
                      sliderInput("step", label = "Simulation Step",
                           min = 0, max = 99, value = 0, step = 1, animate=T)),
               column(6,
                      selectInput("var", label = "Quantity to map",
                                  choices = c("Density" = "totalPop",
                                              "% Greens" = "pctgreens",
                                              "% Reds" = "pctreds",
                                              "% Unsatisfied"="pctunsatisfied"
                                              ), selected = "pctgreens"))
             ),
             plotOutput("map_cell"),
             dataTableOutput("measurestable")#,
            # plotOutput("plotindexes")
   )
    

))
