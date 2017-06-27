library(shiny)
library(treemap)
library(RColorBrewer)
library(ggplot2)

shinyUI(
  navbarPage("Weight Adjustment On Sampling",
             tabPanel("Easy",
                      fluidPage(
                        theme = "bootstrap.css",
                        tags$a(href='http://stat.psu.edu/',tags$img(src='logo5.png', align = "left", width = 60)),
                        tags$head(tags$style("#successM{color: red;
                                 font-size: 20px;
                                             font-style: italic;
                                             }"
                         )),
                        tags$head(tags$style("#successF{color: red;
                                 font-size: 20px;
                                             font-style: italic;
                                             }"
                         )),
                        
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
                            column(4,br(),br(),wellPanel(
                              fluidRow(h3("Left is the treemap of gender proportion in population.")), 
                              fluidRow(h3("Right is the treemap of gender proportion in the sample.")),br(),
                              fluidRow(img(src = "arrow5.png", align = "right",width = 100))
                            )),
                            column(4,plotOutput("population")),
                            column(3,plotOutput("sample"))
                          ),
                          fluidRow(
                            column(4,br(),
                                   
                                   uiOutput("progress")
                                   
                                   ,wellPanel(
                              sliderInput("male","Weight for Male:", min = 0, value = 1, max = 2, step = 0.05),
                              textOutput("hintM"),
                              conditionalPanel("input.male == 1.6", textOutput("successM")),
                              
                              br(),
                              sliderInput("female","Weight for Female", min = 0, value = 1, max = 2, step = 0.05),
                              textOutput("hintF"),
                              conditionalPanel("input.female == 0.75", textOutput("successF")))),
                            column(3,plotOutput("samplePop")),
                            column(3,plotOutput("bar"))
                          ),
                          
                          fluidRow(
                          
                            column(7,conditionalPanel(condition = "(input.male == 1.6) & (input.female == 0.75)",
                                                      wellPanel(h1(textOutput("Congrats")), h4(textOutput("Solution")))))
                          
                        )))
                      ),
             tabPanel("Hard",
                      fluidPage(theme = "sliderColor.css",
                        titlePanel("Weighting adjustment with unknown population"),
                        
                        fluidPage(
                          fluidRow(
                            wellPanel(h4("In order to find out.............................................................."),
                                      fluidRow(column(6,img(src = "election.jpg"))))
                            ),
                          fluidRow(
                            column(12,plotOutput("elePopEW")),
                            column(5,plotOutput("elePopWBar")),
                            column(3,wellPanel(
                              sliderInput("other",paste("Weight for Other Religions:"), min = 0, value = 1, max = 4, step = 0.1),
                              textOutput("hintO"),
                              sliderInput("jewish","Weight for Jewish:", min = 0, value = 1, max = 4, step = 0.1),
                              textOutput("hintJ"),
                              sliderInput("catholic","Weight for Catholic:", min = 0, value = 1, max = 4, step = 0.1),
                              textOutput("hintC"),
                              sliderInput("christian","Weight for Christian:", min = 0, value = 1, max = 4, step = 0.1),
                              textOutput("hintCH")
                              )
                              )
                          ),
                          fluidRow(
                            column(7,conditionalPanel(condition = "(input.christian == 3.5) & (input.catholic == 1.6)
                                                      & (input.jewish == 0.2) & (input.other == 0.5)",
                                                      wellPanel(h1(textOutput("Congradulation")), h4(textOutput("Solutions")))))
                          )
                      ))
             )
)
)
