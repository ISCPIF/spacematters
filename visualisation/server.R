library(shiny)
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(reshape2)
library(grid)
library(lattice)


shinyServer(function(input, output) {
  resultFile <- reactiveValues(location= "data/result.csv")
  
  output$map_cell <- renderPlot({
    result <- read.csv(resultFile$location, sep=",", dec=".", header=F)

    colnames(result) <- c("step", "x", "y", "capacity", "greens", "reds")
 result$totalPop <- result$greens + result$red
 result$pctgreens <- result$greens / result$totalPop * 100
 result$pctreds <- result$reds / result$totalPop * 100
 result$empty <- result$capacity - result$totalPop
 result$pctempty <- result$empty / result$capacity * 100
 
 currentstep <- subset(result, step == input$step)[,-1]
 
 tempo <- melt(currentstep,
               id.vars=c("x","y"),
               measure.vars=input$var)
 tempo2 <- dcast(tempo, x~y)

 map <- as.matrix(tempo2[,-1])
 rownames(map) <- tempo2[,1]

size <- dim(map)[[1]]

if (input$var == "totalPop") my_palette <- colorRampPalette(c("white", "black"))(n = 299)
if (input$var == "pctgreens") my_palette <- colorRampPalette(c("white", "forestgreen"))(n = 299)
if (input$var == "pctreds") my_palette <- colorRampPalette(c("white", "firebrick1"))(n = 299)
if (input$var == "maj") my_palette <- colorRampPalette(c("forestgreen", "grey", "firebrick1"))(n = 100)

mapPop <- levelplot(map, 
          col.regions=my_palette, 
          colorkey=T ,
        xlab="", ylab="",
        cex.axis=0.1,
        scales=list(x=list(at=c(0,size+1)), y=list(at=c(0,size+1))
          )
        )
 
return(mapPop)
  })
})
