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
    value = c(input$male,input$female,input$white*0.50,input$black*0.25,input$hispanic*0.13,input$asian*0.1,input$other*0.02)
  })
  
  output$population <- renderPlot({
    treemap(datafP, index = c("GenderP"), vSize = "PopulationP", type = "index", 
            palette = colorRampPalette(brewer.pal(4, "Pastel1"))(4), 
            title = "Gender Proportion in Population", fontsize.title = 18, fontsize.labels = 16)
  },width = 300, height = 300)
  
  output$sample <- renderPlot({
    treemap(datafS, index = c("GenderS"), vSize = "PopulationS", type="index", 
            palette =  colorRampPalette(brewer.pal(4, "Pastel1"))(4),
            title="Gender Proportion in the Sample", fontsize.title = 18, fontsize.labels = 16)
  },width = 300, height = 300)
  
  output$samplePop <- renderPlot({
   
    value = inputs()
    dataf[c(1,3),"Population"] = dataf[c(1,3),"Population"] *  value[1]
    dataf[c(2,4),"Population"] = dataf[c(2,4),"Population"] *  value[2]
    
    treemap(dataf, index=c("Gender","TVshow"), vSize = "Population", type="index", 
            palette =  colorRampPalette(brewer.pal(4, "Pastel1"))(4), 
            title="Use Sample to Represent Population", fontsize.title = 18, fontsize.labels = 16)
  }, width = 300, height = 300)
  
  output$elePopEW <- renderPlot({
    value = inputs()
    
    barplot(prop.table(rbind(c(eleDatafEW[1,4],eleDatafEW[4,4],eleDatafEW[7,4],eleDatafEW[10,4],eleDatafEW[13,4]),
                             c(eleDatafEW[2,4],eleDatafEW[5,4],eleDatafEW[8,4],eleDatafEW[11,4],eleDatafEW[14,4]),
                             c(eleDatafEW[3,4],eleDatafEW[6,4],eleDatafEW[9,4],eleDatafEW[12,4],eleDatafEW[15,4])))
            ,horiz = TRUE, col = c("#002868","azure1","#BF0A30"), names.arg = c("White","Black","Hispanic/Latino","Asian","Other")
            , main = "Comparison of Two Candidates", las = 1,
            , width = c(value[3],value[4],value[5],value[6],value[7])
    )
  },width = 1100, height = 400)
  
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
  },width = 400, height = 300)
  
  
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
            width = 0.8, xlim = c(0,2),cex.names=1.4, cex.main = 1.4)
    par(lwd = 2)
    legend("topright", c("Female","Male"), fill=c("#FBB4AE","#B3CDE3"))
    
  }, width = 450, height = 365)
  
  output$hintM <- renderText(
    if (input$male == 1){print("Move the slider to reach the right weight."
                               ,size="\\fontsize{15pt}\\")}
    #else if (input$male == 1.6){print("Congradulations! You got the correct weight for male.")}
    else if (input$male < 1.6){print("Hint: Move towards right to get the correct weight.")}
    else if (input$male > 1.6){print("Hint: Move towards left to get the correct weight.")}
  )
  output$successM <- renderText({print("Congratulations! You got the correct weight for male.")})
  output$successF <- renderText({print("Congratulations! You got the correct weight for female.")})
  output$successO <- renderText({print("Congratulations! You got the correct weight for Other.")})
  output$successA <- renderText({print("Congratulations! You got the correct weight for Asians.")})
  output$successH <- renderText({print("Congratulations! You got the correct weight for Hispanic/Latino.")})
  output$successB <- renderText({print("Congratulations! You got the correct weight for Black.")})
  output$successW <- renderText({print("Congratulations! You got the correct weight for White.")})
  output$hintF <- renderText(
    if (input$female == 1){print("Move the slider to reach the right weight.")}
    #else if (input$female == 0.75){print("Congradulations! You got the correct weight for female.")} 
    else if (input$female < 0.75){print("Hint: Move towards right to get the correct weight.")}
    else if (input$female > 0.75){print("Hint: Move towards left to get the correct weight.")}
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
    if (input$white == 1){print("Move the slider to reach the right weight.")}
   # else if (input$white == 1.7){print("Congradulations! You got the correct weight for white.")}
    else if (input$white < 1.4){print("Hint: Move towards right to get the correct weight.")}
    else if (input$white > 1.4){print("Hint: Move towards left to get the correct weight.")}
  )
  output$hintB <- renderText(
    if (input$black == 1){print("Move the slider to reach the right weight.")}
    #else if (input$black == 3.8){print("Congradulations! You got the correct weight for black.")}
    else if (input$black < 0.5){print("Hint: Move towards right to get the correct weight.")}
    else if (input$black > 0.5){print("Hint: Move towards left to get the correct weight.")}
  )
  output$hintH <- renderText(
    if (input$hispanic == 1){print("Move the slider to reach the right weight.")}
   # else if (input$hispanic == 0.2){print("Congradulations! You got the correct weight for hispanic")}
    else if (input$hispanic < 0.8){print("Hint: Move towards right to get the correct weight.")}
    else if (input$hispanic > 0.8){print("Hint: Move towards left to get the correct weight.")}
  )
  output$hintA <- renderText(
    if (input$asian == 1){print("Move the slider to reach the right weight.")}
   # else if (input$asian == 4){print("Congradulations! You got the correct weight for Other religions.")}
    else if (input$asian < 0.4){print("Hint: Move towards right to get the correct weight.")}
    else if (input$asian > 0.4){print("Hint: Move towards left to get the correct weight.")}
  )
  output$hintO <- renderText(
    if (input$other == 1){print("Move the slider to reach the right weight.")}
   # else if (input$other == 0.3){print("Congradulations! You got the correct weight for None religions.")}
    else if (input$other < 1.5){print("Hint: Move towards right to get the correct weight.")}
    else if (input$other > 1.5){print("Hint: Move towards left to get the correct weight.")}
  )
  
  
  output$Congradulation <- renderText(
    print("Congratulations!") 
  )
  
  output$Solutions <- renderText(
    print("This is the correct weight.
          Here is how we compute the weight:
          ...........................................................................................")
    )
  
  ##################################################progress
  output$progress <- renderUI({
    tags$div(
      'class' = "progress progress-striped active",
      tags$div('class' = "progress-bar progress-bar-success", 'style'=paste0("width:",round(input$male * 30),"%",sep = '')),
      tags$div('class' = "progress-bar progress-bar-warning", 'style'=paste0("width:",round(input$female * 70),"%",sep = ''))
    )
    
  })
  output$progressB <- renderUI({
    tags$div(
      'class' = "progress progress-striped active",
      tags$div('class' = "progress-bar progress-bar-success", 'style'=paste0("width:",round(input$other * 2),"%",sep = '')),
      tags$div('class' = "progress-bar progress-bar-warning", 'style'=paste0("width:",round(input$asian * 10),"%",sep = '')),
      tags$div('class' = "progress-bar progress-bar-danger", 'style'=paste0("width:",round(input$hispanic * 13),"%",sep = '')),
      tags$div('class' = "progress-bar progress-bar-info", 'style'=paste0("width:",round(input$black * 25),"%",sep = '')),
      tags$div('class' = "progress-bar progress-bar-default", 'style'=paste0("width:",round(input$white * 50),"%",sep = ''))
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
      h3("Keep in mind when you adjust the sample, the summation should always be 1.")
    }else{
      h3("Warning: The summation is larger than 1 now.",style = "color: red")
    }
  })
  
})


