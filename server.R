library(shiny)
library(treemap)
library(RColorBrewer)
library(ggplot2)

shinyServer(function(input, output) {
  
  originaldata <- read.csv("originalDataset.csv", TRUE, sep = ",",na.strings = TRUE)
  dataf = data.frame(originaldata)
  population <- read.csv("population.csv",TRUE, sep = ",", na.strings = TRUE)
  datafP = data.frame(population)
  sample <- read.csv("sample.csv", TRUE, sep = ",",na.strings = TRUE)
  datafS = data.frame(sample)
  electionPopulationEW <- read.csv("electionPopulationEqually.csv",TRUE, sep = ",", na.strings = TRUE)
  eleDatafEW = data.frame(electionPopulationEW)
  
  
  output$dataTable <- renderTable(read.csv("dataTable.csv"))
  output$populationRatio <- renderTable(read.csv("PopulationRatio.csv"))
  
  inputs= reactive({
    value = c(input$male,input$female,input$christian,input$catholic,input$jewish,input$other)
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
  
  output$elePopEW <- renderPlot({
    value = inputs()
    # eleDatafEW[c(1,2,3),"PopulationE"] = eleDatafEW[c(1,2,3),"PopulationE"] * value[3]
    # eleDatafEW[c(4,5,6),"PopulationE"] = eleDatafEW[c(4,5,6),"PopulationE"] * value[4]
    # eleDatafEW[c(7,8,9),"PopulationE"] = eleDatafEW[c(7,8,9),"PopulationE"] * value[5]
    # eleDatafEW[c(10,11,12),"PopulationE"] = eleDatafEW[c(10,11,12),"PopulationE"] * value[6]
    
    barplot(prop.table(rbind(c(eleDatafE[1,4],eleDatafE[4,4],eleDatafE[7,4],eleDatafE[10,4],eleDatafE[13,4]),
                             c(eleDatafE[2,4],eleDatafE[5,4],eleDatafE[8,4],eleDatafE[11,4],eleDatafE[14,4]),
                             c(eleDatafE[3,4],eleDatafE[6,4],eleDatafE[9,4],eleDatafE[12,4],eleDatafE[15,4])))
            ,horiz = TRUE, col = c("#1C2C5B","grey","brown3"), names.arg = c("Christian","Catholic","Jewish","Others","None")
            , main = "Comparison of Two Candidates"
            , width = c(value[3],value[4],value[5],value[6],1)
            #xlim = c(1,10)
    )

    # treemap(eleDatafEW, index = c("Candidate","ReligionE"), vSize = "PopulationE", type = "index",
    #         palette = colorRampPalette(brewer.pal(4, "Pastel1"))(4),
    #         title = "Equal Distribution", fontsize.title = 14)
  },width = 1200, height = 400)
  
  output$elePopWBar <- renderPlot({
    value = inputs()
    # eleDatafEW[c(1,2,3),"PopulationE"] = eleDatafEW[c(1,2,3),"PopulationE"] * value[3]
    # eleDatafEW[c(4,5,6),"PopulationE"] = eleDatafEW[c(4,5,6),"PopulationE"] * value[4]
    # eleDatafEW[c(7,8,9),"PopulationE"] = eleDatafEW[c(7,8,9),"PopulationE"] * value[5]
    # eleDatafEW[c(10,11,12),"PopulationE"] = eleDatafEW[c(10,11,12),"PopulationE"] * value[6]

    barplot(prop.table(rbind(c(eleDatafE[1,4] * value[3],eleDatafE[3,4] * value[3]),
                             c(eleDatafE[4,4] * value[4],eleDatafE[6,4] * value[4]),
                             c(eleDatafE[7,4] * value[5],eleDatafE[9,4] * value[5]),
                             c(eleDatafE[10,4]* value[6],eleDatafE[12,4]* value[6]),
                             c(eleDatafE[13,4],eleDatafE[15,4])))
            , names.arg = c("Clinton","Trump")
            , col= brewer.pal(10, "PRGn")
            )
    
    # x <- data.frame(aa=c(0.2,0.6,0.1,0.1),
    #                 bb=c(0.4,0.5,0.05,0.05),
    #                 dd = 1:4)
    # #x <- melt(x, "dd")
    # col=c(rep(c("white","grey"),2),rep(c("white","red"),2))
    # ggplot(x,y) + geom_bar(stat = "identity", fill = col)
  },width = 400, height = 500)
  
  
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


