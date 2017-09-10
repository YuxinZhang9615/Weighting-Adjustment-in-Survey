library(shiny)
library(treemap)
library(RColorBrewer)
library(ggplot2)
library(shinyBS)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Weight Adjustment On Sampling",
                  titleWidth = 180),
  dashboardSidebar(width = 180,
    sidebarMenu(
      menuItem("Introduction", tabName = "introduction", icon = icon("dashboard")),
      menuItem("Easy Level", tabName = "easy", icon = icon("th")),
      menuItem("Hard Level", tabName = "hard", icon = icon("th"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "introduction",
              fluidPage(
                h1("About"),
                h2("Explore how weighting adjustment affects the predicted results in survey analysis"),
                h1("Background"),
                h2("A selected sample may not be a good representation of a population due to many reasons.
                   Non-response rate is one of the biggest challenges. When some variables measured 
                   in the survey are under- or over-represented, statisticians use a weighting adjustment as a common correction
                   technique. Each survey respondent gets an adjustment weight. 
                   Subjects in underrepresented group get a weight more than one, and subjects in overrepresented group  
                   get a weight smaller than one."),
                h1("Instruction"),
                h2("Move the sliders around to explore how the weighting adjustment affects the results. Use your best
                   judgement to find out the correct adjustment weight for each scenario. Notice that the summation
                   bar should never be larger than one because the weighted sample should never be larger than the population."),
                h1("Acknowledgement and Credit"),
                # h2("Jon Huang, Samuel Jacoby, Michael Strickland and K.K.Rebecca Lai (2016 Nov.8).",
                #    tags$a(href = "https://www.nytimes.com/interactive/2016/11/08/us/politics/election-exit-polls.html","Election 2016: Exit Polls.", style = "text-decoration: underline; color: #f08080"), 
                #    tags$i("New York Times.")),
                h2("This app was developed and coded by Yuxin Zhang. The exit poll data set was extracted from", 
                   tags$a(href = "https://www.nytimes.com/interactive/2016/11/08/us/politics/election-exit-polls.html","Election 2016: Exit Polls.", style = "text-decoration: underline; color: #f08080"),"on July 20, 2017.")
                )
              ),
      tabItem(tabName = "easy",
              fluidPage(
                theme = "theme.css",
                tags$a(href='http://stat.psu.edu/',tags$img(src='logo5.png', align = "left", width = 60)),
                tags$head(tags$style("#successM{color: red;
                                     font-size: 12px;
                                     font-style: italic;
                                     }"
                         )),
                tags$head(tags$style("#successF{color: red;
                                     font-size: 12px;
                                     font-style: italic;
                                     }"
                         )),
                tags$head(tags$style("#successO{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
                        )),
                tags$head(tags$style("#successA{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
                        )),
                tags$head(tags$style("#successH{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
                        )),
                tags$head(tags$style("#successB{color: red;
                                     font-size: 20px;
                                     font-style: italic;
                                     }"
                        )),
                tags$head(tags$style("#successW{color: red;
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
                              fluidRow(column(4,img(src = "image1.png", width = 200)),column(4,img(src = "image2.png", width = 300))))
                    ),
                  
                  fluidRow(
                   wellPanel(
                      fluidRow(h3("Left is the treemap of gender proportion in population.")), 
                      fluidRow(h3("Right is the treemap of gender proportion in the sample.")),br(),
                      fluidRow(img(src = "arrow5.png", align = "right",width = 80))
                      , class = "col-lg-4 col-md-6 col-sm-12 col-xs-12"),
                    wellPanel(plotOutput("population"), class = "wellBorder col-lg-4 col-md-6 col-sm-12 col-xs-12"),
                    wellPanel(plotOutput("sample"), class = "wellBorder col-lg-4 col-md-6 col-sm-12 col-xs-12")
                  ),
                  fluidRow(
                    uiOutput("warning"),
                    uiOutput("progress"),
                    div(style = "position: relative; top:-15px", div(style = "float: left", print("0")),div(style = "float:right", print("n"))),
                    
                    wellPanel(
                      sliderInput("male","Weight for Male:", min = 0, value = 1, max = 2, step = 0.2),
                      textOutput("hintM"),
                      #conditionalPanel("input.male == 1.6", textOutput("successM")),
                      br(),
                      sliderInput("female","Weight for Female", min = 0, value = 1, max = 2, step = 0.02),
                      textOutput("hintF"),
                      #conditionalPanel("input.female == 0.74", textOutput("successF"))
                      class = "col-lg-4 col-md-6"),
                    
                    wellPanel(plotOutput("samplePop"), class = "wellBorder col-lg-3 col-md-6 col-sm-12 col-xs-12"),
                    wellPanel(plotOutput("bar"), class = "wellBorder col-lg-4 col-md-6 col-sm-12 col-xs-12")
                  ),
                  
                  fluidRow(
                    
                    column(12,conditionalPanel(condition = "(input.male == 1.6) & (input.female == 0.74)",
                                              wellPanel(h1(textOutput("Congrats")), 
                                                        h5("The proportion of female is larger than the proportion of male in the sample, which does not
                                                           represent the population well. Before the weighting adjustment, the supporting rate of
                                                           The Ellen Show is much higher than that of The Late Night Show, but after the weighting adjustment,
                                                           the supporting rate of The Ellen Show is almost the same with that of the Late Night Show."),
                                                        h5("This is a simple example of weighting adjustment with one auxiliary variable.
                                                           The population distribution is available so we can compare the response distribution of
                                                           sample with the population distribution."),
                                                        h5("We can make the response representative with respect to gender. The weight is
                                                           obtained by dividing the population percentage by the corresponding response percentage.
                                                           The weight for male is 48 / 30 = 1.6 . The weight for female is 52 / 70 = 0.74 ."),
                                                        h5("If you understand the weighting adjustment with the population distribution known,
                                                           please go to the hard level to explore the weighting technique with the population 
                                                           distribution unknown."))))
                    
                  )))
              ),
      tabItem(tabName = "hard",
              fluidPage(theme = "sliderColor.css",
                        titlePanel("Weighting adjustment with unknown population"),
                        
                        fluidPage(
                          fluidRow(
                            wellPanel(
                                fluidRow(
                                    column(5,h4("In order to predict the result of election correctly, statisticians need to use weighting adjustment to deal with
                                                 problems like non-response rate in sample analysis. This is the exit poll data from 2016 election. Can you try
                                                 playing around with the slider to get the correct weight? See how big a difference that makes.")),
                                    column(6,offset = 1,img(src = "image3.png", width = 300))), class = "well1"),
                          
                            column(12,div(style = "height:260px;",plotOutput("elePopEW"))),
                            column(7,uiOutput("warningB")), column(5,div(style = "margin-top:7px",img(src = "legend.png", width = 400))),
                            column(12,uiOutput("progressB")),
                            column(5,plotOutput("elePopWBar")),
                            column(7,wellPanel(
                              fluidRow(
                                column(5,sliderInput("other",paste("Weight for other race:"), min = 0, value = 1, max = 2, step = 0.3), style = "height:60px"),
                                column(7,textOutput("hintO"))
                              ),
                              fluidRow(
                                column(5, sliderInput("asian",paste("Weight for Asian:"), min = 0, value = 1, max = 2, step = 0.2), style = "height:60px"),
                                column(7, textOutput("hintA"))
                              ),
                              fluidRow(
                                column(5,sliderInput("hispanic","Weight for Hispanic/Latino:", min = 0, value = 1, max = 2, step = 0.1), style = "height:60px"),
                                column(7,textOutput("hintH"))
                              ),
                              fluidRow(
                                column(5,sliderInput("black","Weight for Black:", min = 0, value = 1, max = 2, step = 0.1), style = "height:60px"),
                                column(7,textOutput("hintB"))
                              ),
                              fluidRow(
                                column(5,sliderInput("white","Weight for White:", min = 0, value = 1, max = 2, step = 0.2), style = "height:60px"),
                                column(7,textOutput("hintW"))
                              )
                              
                            )
                            )
                          ),
                          fluidRow(
                            conditionalPanel(condition = "(input.white == 1.4) & (input.black == 0.5)
                                                      & (input.hispanic == 0.7) & (input.asian == 0.4) & (input.other == 1.5)",
                                                      wellPanel(h1(textOutput("Congradulation")), h4(textOutput("Solutions"))))
                          )
                            ))
              )
    )
  )
)


