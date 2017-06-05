library(shiny)
library(treemap)
library(RColorBrewer)

shinyServer(function(input, output) {
  
  originaldata <- read.csv("originalDataset.csv", TRUE, sep = ",",na.strings = TRUE)
  dataf = data.frame(originaldata)
  population <- read.csv("population.csv",TRUE, sep = ",", na.strings = TRUE)
  datafP = data.frame(population)
  sample <- read.csv("sample.csv", TRUE, sep = ",",na.strings = TRUE)
  datafS = data.frame(sample)
  
  
  output$dataTable <- renderTable(read.csv("dataTable.csv"))
  output$populationRatio <- renderTable(read.csv("PopulationRatio.csv"))
  
  inputs= reactive({
    value = c(input$male,input$female)
  })
  
  output$population <- renderPlot({
    treemap(datafP, index = c("GenderP"), vSize = "PopulationP", type = "index", 
            palette = colorRampPalette(brewer.pal(4, "Pastel1"))(4), 
            title = "Gender Proportion in Population", fontsize.title = 14)
  },width = 300, height = 300)
  
  output$sample <- renderPlot({
    treemap(datafS, index = c("GenderS"), vSize = "PopulationS", type="index", 
            palette =  colorRampPalette(brewer.pal(4, "Pastel1"))(4),
            title="Gender Proportion in the Sample", fontsize.title = 14)
  },width = 300, height = 300)
  
  output$samplePop <- renderPlot({
   
    value = inputs()
    dataf[c(1,3),"Population"] = dataf[c(1,3),"Population"] *  value[1]
    dataf[c(2,4),"Population"] = dataf[c(2,4),"Population"] *  value[2]
    
    treemap(dataf, index=c("Gender","TVshow"), vSize = "Population", type="index", 
            palette =  colorRampPalette(brewer.pal(4, "Pastel1"))(4), 
            title="Use Sample to Represent Population", fontsize.title = 14)
  }, width = 300, height = 300)
  
  output$bar <- renderPlot({
    
    value = inputs()
    dataf[c(1,3),"Population"] = dataf[c(1,3),"Population"] *  value[1]
    dataf[c(2,4),"Population"] = dataf[c(2,4),"Population"] *  value[2]
    
    ellen = c(dataf[2,"Population"], dataf[1,"Population"])
    late = c(dataf[4,"Population"], dataf[3,"Population"])
    dataframe = data.frame("The Ellen Show" = ellen, "The Late Night Show" = late)
    matrix = as.matrix(dataframe)
    barplot(matrix, col = c("#FBB4AE","#B3CDE3"), main = "Supporting Rate of Both Show", 
            width = 0.8, xlim = c(0,3))
    legend("topright", c("Female","Male"), fill=c("#FBB4AE","#B3CDE3"))
    
  }, width = 600, height = 400)
  
  output$Congrats <- renderText(
    print("Congratulations!") 
  )
  
  output$Solution <- renderText(
    print("This is the correct weight.
          Here is how we compute the weight:
          ...........................................................................................")
  )
  
  output$hintM <- renderText(
    if (input$male == 1){print("Move the slider to reach the right weight.")}
    else if (input$male == 1.6){print("Congradulations! You got the correct weight for male.")}
    else if (input$male < 1.6){print("Hint: Move towards right to get the correct weight.")}
    else if (input$male > 1.6){print("Hint: Move towards left to get the correct weight.")}
  )
  output$hintF <- renderText(
    if (input$female == 1){print("Move the slider to reach the right weight.")}
    else if (input$female == 0.75){print("Congradulations! You got the correct weight for female.")}
    else if (input$female < 0.75){print("Hint: Move towards right to get the correct weight.")}
    else if (input$female > 0.75){print("Hint: Move towards left to get the correct weight.")}
  )
  
})


