library(shiny)
library(treemap)
library(RColorBrewer)
library(ggplot2)
library(shinyBS)

shinyServer(function(input, output) {
  
  originaldata <- read.csv("originalDataset.csv", TRUE, sep = ",",na.strings = TRUE)
  dataf = data.frame(originaldata)
  population <- read.csv("population.csv",TRUE, sep = ",", na.strings = TRUE)
  datafP = data.frame(population)
  sample <- read.csv("sample.csv", TRUE, sep = ",",na.strings = TRUE)
  datafS = data.frame(sample)
  electionPopulationEW <- read.csv("electionPopulationRace.csv",TRUE, sep = ",", na.strings = TRUE)
  eleDatafEW = data.frame(electionPopulationEW)
  
  
  output$dataTable <- renderTable(read.csv("dataTable.csv"))
  output$populationRatio <- renderTable(read.csv("PopulationRatio.csv"))
  
  inputs= reactive({
    value = c(input$male,input$female,input$white*0.50,input$black*0.25,input$hispanic*0.15,input$asian*0.1,input$other*0.02)
  })
  
  output$population <- renderPlot({
    treemap(datafP, index = c("GenderP"), vSize = "PopulationP", type = "index", 
            palette = colorRampPalette(brewer.pal(4, "Pastel1"))(4), 
            title = "Gender Proportion in Population", fontsize.title = 16, fontsize.labels = 16)
  },width = 250, height = 250)
  
  output$sample <- renderPlot({
    treemap(datafS, index = c("GenderS"), vSize = "PopulationS", type="index", 
            palette =  colorRampPalette(brewer.pal(4, "Pastel1"))(4),
            title="Gender Proportion in the Sample", fontsize.title = 16, fontsize.labels = 16)
  },width = 250, height = 250)
  
  output$samplePop <- renderPlot({
   
    value = inputs()
    dataf[c(1,3),"Population"] = dataf[c(1,3),"Population"] *  value[1]
    dataf[c(2,4),"Population"] = dataf[c(2,4),"Population"] *  value[2]
    
    treemap(dataf, index=c("Gender","TVshow"), vSize = "Population", type="index", 
            palette =  colorRampPalette(brewer.pal(4, "Pastel1"))(4), 
            title="Use Sample to Represent Population", fontsize.title = 14, fontsize.labels = 16)
  }, width = 260, height = 260)
  
  output$elePopEW <- renderPlot({
    value = inputs()
    
    barplot(prop.table(rbind(c(eleDatafEW[1,4],eleDatafEW[4,4],eleDatafEW[7,4],eleDatafEW[10,4],eleDatafEW[13,4]),
                             c(eleDatafEW[2,4],eleDatafEW[5,4],eleDatafEW[8,4],eleDatafEW[11,4],eleDatafEW[14,4]),
                             c(eleDatafEW[3,4],eleDatafEW[6,4],eleDatafEW[9,4],eleDatafEW[12,4],eleDatafEW[15,4])))
            ,horiz = TRUE, col = c("#002868","azure1","#BF0A30"), names.arg = c("White","Black","Latino","Asian","Other")
            , main = "Comparison of Two Candidates", las = 1,
            , width = c(value[3],value[4],value[5],value[6],value[7])
    )
  },width = 900, height = 300)
  
  output$elePopWBar <- renderPlot({
    value = inputs()

    barplot(prop.table(rbind(c(eleDatafEW[1,4] * value[3],eleDatafEW[3,4] * value[3]),
                             c(eleDatafEW[4,4] * value[4],eleDatafEW[6,4] * value[4]),
                             c(eleDatafEW[7,4] * value[5],eleDatafEW[9,4] * value[5]),
                             c(eleDatafEW[10,4]* value[6],eleDatafEW[12,4]* value[6]),
                             c(eleDatafEW[13,4]* value[7],eleDatafEW[15,4]* value[7])))
            , names.arg = c("Clinton","Trump")
            , col= brewer.pal(8, "YlOrBr")
            )
  },width = 400, height = 400)
  
  
  output$bar <- renderPlot({
    
    value = inputs()
    dataf[c(1,3),"Population"] = dataf[c(1,3),"Population"] *  value[1]
    dataf[c(2,4),"Population"] = dataf[c(2,4),"Population"] *  value[2]
    
    ellen = c(dataf[2,"Population"], dataf[1,"Population"])
    late = c(dataf[4,"Population"], dataf[3,"Population"])
    dataframe = data.frame("The Ellen Show" = ellen, "The Late Night Show" = late)
    matrix = as.matrix(dataframe)
    par(lwd = 2)
    barplot(matrix, col = c("#FBB4AE","#B3CDE3"), main = "Supporting Rate of Both Show", 
            width = 0.8, xlim = c(0,2),cex.names=1, cex.main = 1.3)
    par(lwd = 2)
    legend("topright", c("Female","Male"), fill=c("#FBB4AE","#B3CDE3"))
    
  }, width = 370, height = 320)
  
  output$hintM <- renderText(
    if (input$male == 1.4 | input$male == 1.8){print("You are close to the right answer.")}
    else if (input$male == 1.6){print("Congradulations! You got the correct weight for male.")}
    else {print("Move the slider to reach the correct weight.")}
  )
  output$hintF <- renderText(
    if (input$female == 0.74){print("Congradulations! You got the correct weight for female.")}
    else if (input$female >= 0.7 & input$female <= 0.8){print("You are close to the right answer.")}
    else {print("Move the slider to reach the correct weight.")}
  )
  output$Congrats <- renderText(
    print("Congratulations!") 
  )
  
  output$Solution <- renderText(
    print("This is the correct weight.
          Here is how we compute the weight:
          ...........................................................................................")
  )
  
  output$hintW <- renderText(
    if (input$white == 1.4){print("Congradulations! You got the correct weight for White.")}
    else if (input$white >= 1.2 & input$white <= 1.6){print("You are close to the right answer.")}
    else {print("Move the slider to reach the right weight.")}
  )
  output$hintB <- renderText(
    if (input$black == 0.5){print("Congradulations! You got the correct weight for Black.")}
    else if (input$black >= 0.3 & input$black <= 0.7){print("You are close to the right answer.")}
    else {print("Move the slider to reach the right weight.")}
  )
  output$hintH <- renderText(
    if (input$hispanic == 0.7){print("Congradulations! You got the correct weight for Hispanic.")}
    else if (input$hispanic <= 0.9 & input$hispanic >= 0.5){print("You are close to the right answer.")}
    else {print("Move the slider to reach the right weight.")}
    
  )
  output$hintA <- renderText(
    if (input$asian == 0.4){print("Congradulations! You got the correct weight for Asian.")}
    else if (input$asian >= 0.2 & input$asian <= 0.6){print("You are close to the correct answer.")}
    else {print("Move the slider to reach the right weight.")}
  )
  output$hintO <- renderText(
    if (input$other == 1.5){print("Congradulations! You got the correct weight for Other race.")}
    else if (input$other >= 1.2 & input$other <= 1.8){print("You are close to the correct answer.")}
    else {print("Move the slider to reach the right weight.")}
  )
  
  
  output$Congradulation <- renderText(
    print("Congratulations!") 
  )

  output$Solutions <- renderText(
    print("Finding the correct weight is hard, especially when the population proportion is unknown. ")
    )

  #################################################progress
  output$progress <- renderUI({
    tags$div(
      'class' = "progress progress-striped active",
      tags$div('class' = "progress-bar progress-bar-info", 'style'=paste0("width:",round(input$male * 30),"%",sep = '')),
      tags$div('class' = "progress-bar progress-bar-default", 'style'=paste0("width:",round(input$female * 70),"%",sep = ''))
    )
    
  })
  output$progressB <- renderUI({
    tags$div(
      'class' = "progress progress-striped active",
      tags$div('class' = "progress-bar progress-bar-success", 'style'=paste0("width:",input$other * 2,"%",sep = '')),
      tags$div('class' = "progress-bar progress-bar-warning", 'style'=paste0("width:",input$asian * 10,"%",sep = '')),
      tags$div('class' = "progress-bar progress-bar-danger", 'style'=paste0("width:",input$hispanic * 15,"%",sep = '')),
      tags$div('class' = "progress-bar progress-bar-info", 'style'=paste0("width:",input$black * 25,"%",sep = '')),
      tags$div('class' = "progress-bar progress-bar-default", 'style'=paste0("width:",input$white * 50,"%",sep = ''))
    )
    
  })
  output$warning <- renderUI({
    if (input$male * 30 + input$female * 70 <= 100){
      h4("Keep in mind when you adjust the sample, the summation should always be 1.")
    }else{
      h3("Warning: The summation is larger than 1 now.",style = "color: red")
    }
  })
  output$warningB <- renderUI({
    if (input$other * 2 + input$asian * 10 + input$hispanic * 13 + input$black * 25 + input$white * 50 <= 100){
      h4("Keep in mind when you adjust the sample, the summation should always be 1.")
    }else{
      h4("Warning: The summation is larger than 1 now.",style = "color: red")
    }
  })
  
})


