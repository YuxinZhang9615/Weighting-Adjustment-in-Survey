library(shiny)
library(treemap)
library(RColorBrewer)

shinyUI(
  navbarPage("Weight Adjustment On Sampling",
             tabPanel("Easy",
                      fluidPage(
                        tags$a(href='http://stat.psu.edu/',tags$img(src='logo5.png', align = "left", width = 60)),
                        
                        titlePanel("Weighting adjustment with one auxiliary variable"),
                        
                        fluidPage(
                          fluidRow(
                            wellPanel(h4("In order to find out between The Ellen Show and The Late Night Show which one is more 
                                      popular in our campus, we did a survey on 100 students. However, this sample cannot 
                                      represent the population well because the proportion of female in this sample is significantly 
                                      larger than the proportion of female in the population. Therefore, we need weighting adjustment
                                      to the data we got. Based on the following table and proportion graph, can you guess what is the 
                                      correct weight? Try playing around with both sliders following the instruction."),
                                      fluidRow(column(4,tableOutput("populationRatio")),column(4,tableOutput("dataTable"))))
                          ),
                          
                          fluidRow(
                            column(3,plotOutput("population")),
                            column(3,plotOutput("sample")),
                            column(3,plotOutput("samplePop")),
                            column(3,wellPanel(
                              sliderInput("male","Weight for Male:", min = 0, value = 1, max = 2, step = 0.05),
                              textOutput("hintM"),
                              sliderInput("female","Weight for Female", min = 0, value = 1, max = 2, step = 0.05),
                              textOutput("hintF")))
                          ),
                          
                          fluidRow(
                            column(5,plotOutput("bar")),
                            column(7,conditionalPanel(condition = "(input.male == 1.6) & (input.female == 0.75)",
                                                      wellPanel(h1(textOutput("Congrats")), h4(textOutput("Solution")))))
                          )
                          
                          
                        ))
                      ),
             tabPanel("Hard",
                      fluidPage())
             )
)
