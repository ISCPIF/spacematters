library(shiny)
library(RColorBrewer)
library(RCurl)
library(ggplot2)
library(reshape2)
library(grid)
library(lattice)
library(corrplot)

rquery.cormat<-function(x,
                        type=c('lower', 'upper', 'full', 'flatten'),
                        graph=TRUE,
                        graphType=c("correlogram", "heatmap"),
                        col=NULL, ...)
{
  library(corrplot)
  # Helper functions
  #+++++++++++++++++
  # Compute the matrix of correlation p-values
  cor.pmat <- function(x, ...) {
    mat <- as.matrix(x)
    n <- ncol(mat)
    p.mat<- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], ...)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  # Get lower triangle of the matrix
  getLower.tri<-function(mat){
    upper<-mat
    upper[upper.tri(mat)]<-""
    mat<-as.data.frame(upper)
    mat
  }
  # Get upper triangle of the matrix
  getUpper.tri<-function(mat){
    lt<-mat
    lt[lower.tri(mat)]<-""
    mat<-as.data.frame(lt)
    mat
  }
  # Get flatten matrix
  flattenCorrMatrix <- function(cormat, pmat) {
    ut <- upper.tri(cormat)
    data.frame(
      row = rownames(cormat)[row(cormat)[ut]],
      column = rownames(cormat)[col(cormat)[ut]],
      cor  =(cormat)[ut],
      p = pmat[ut]
    )
  }
  # Define color
  if (is.null(col)) {
    col <- colorRampPalette(
      c("#67001F", "#B2182B", "#D6604D", "#F4A582",
        "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
        "#4393C3", "#2166AC", "#053061"))(200)
    col<-rev(col)
  }
  
  # Correlation matrix
  cormat<-signif(cor(x, use = "complete.obs", ...),2)
  pmat<-signif(cor.pmat(x, ...),2)
  # Reorder correlation matrix
  ord<-corrMatOrder(cormat, order="hclust")
  cormat<-cormat[ord, ord]
  pmat<-pmat[ord, ord]
  # Replace correlation coeff by symbols
  sym<-symnum(cormat, abbr.colnames=FALSE)
  # Correlogram
  if(graph & graphType[1]=="correlogram"){
    corrplot(cormat, type=ifelse(type[1]=="flatten", "lower", type[1]),
             tl.col="black", tl.srt=45,col=col,...)
  }
  else if(graphType[1]=="heatmap")
    heatmap(cormat, col=col, symm=TRUE)
  # Get lower/upper triangle
  if(type[1]=="lower"){
    cormat<-getLower.tri(cormat)
    pmat<-getLower.tri(pmat)
  }
  else if(type[1]=="upper"){
    cormat<-getUpper.tri(cormat)
    pmat<-getUpper.tri(pmat)
    sym=t(sym)
  }
  else if(type[1]=="flatten"){
    cormat<-flattenCorrMatrix(cormat, pmat)
    pmat=NULL
    sym=NULL
  }
  list(r=cormat, p=pmat, sym=sym)
}
    
    
shinyServer(function(input, output) {
  resultFile <- reactiveValues(pathmacro= "data/resultmacro.csv", pathmicro= "data/resultmicro.csv")
  resultTable <- reactiveValues(datamacro = NULL, datamicro = NULL)
  
  
  output$map_cell <- renderPlot({
    inMicroFile <- input$file1  
    if (is.null(inMicroFile)) {
      resultTable$datamicro <- read.csv(resultFile$pathmicro, sep=",", dec=".", header=F)
    }
    if (!is.null(inMicroFile)) {
      resultTable$datamicro <- read.csv(inMicroFile$datapath, header=input$header, sep=input$sep, 
                         quote=input$quote)
    }
  
    
   # result <- read.csv("data/result.csv", sep=",", dec=".", header=F)
    #summary(result)
   result <- resultTable$datamicro
    colnames(result) <- c("step", "x", "y", "capacity", "greens", "reds", "satisgreen", "satisred")
 result$totalPop <- result$greens + result$red
 result$pctgreens <- result$greens / result$totalPop * 100
 result$pctreds <- result$reds / result$totalPop * 100
 result$empty <- result$capacity - result$totalPop
 result$pctempty <- result$empty / result$capacity * 100
 result$satisfiedgreen <- ifelse(result$satisgreen == "false", result$greens, 0)
 result$satisfiedred <- ifelse(result$satisred == "false", result$reds, 0)
 result$satisfied <-  result$satisfiedgreen +  result$satisfiedred
 result$pctsatisfied <- result$satisfied / result$totalPop * 100
 result$pctunsatisfied <-  100 -  result$pctsatisfied
 
 resultTable$data  <- result
 
 Table <- as.data.frame(resultTable$data)
 
 currentstep <- subset(Table, step == input$step)[,-1]
 
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
if (input$var == "pctunsatisfied") my_palette <- colorRampPalette(c("white", "dodgerblue3"))(n = 100)

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



output$measurestable <- renderTable({
  
  inMacroFile <- input$file2
  if (is.null(inMacroFile)) {
    resultTable$datamacro <- read.csv(resultFile$pathmacro, sep=",", dec=".", header=F)
  }
  if (!is.null(inMacroFile)) {
    resultTable$datamacro <- read.csv(inMicroFile$datapath, header=input$header, sep=input$sep, 
                        quote=input$quote)
  }
  
  indexes <- resultTable$datamacro
  colnames(indexes) <-  c("step", "unsatisfied","dissimilarity", "moranRed","Entropy", "ExposureRed",
                          "ExposureGreen", "IsolationRed","IsolationGreen",
                          "ConcentrationRed",  "ConcentrationGreen")
  rownames(indexes) <-   as.numeric(rownames(indexes)) - 1
  
  currentstep <- subset(indexes, step == input$step)[,-1]
 return(currentstep)
},digits = 3)


output$plotindexes <- renderPlot({
  indexes <- resultTable$datamacro
  colnames(indexes) <-  c("step", "unsatisfied","dissimilarity", "moranRed","Entropy", "ExposureRed",
                          "ExposureGreen", "IsolationRed","IsolationGreen",
                          "ConcentrationRed",  "ConcentrationGreen")
  p <- rquery.cormat(indexes, type="full")
  return(p)
})



})

