#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages("shinydashboard")
library(shiny) 
library(shinydashboard)

dashboardPage(skin = "green",
    dashboardHeader(title = "Grub Munch"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Overview", tabName = "Overview", icon = icon("dashboard")),
            menuItem("N-grams",tabName = "N-grams", icon = icon("th")),
            menuItem("Sentiment", tabName = "Sentiment", icon = icon("table")),
            menuItem("Frequency", tabName = "Frequency", icon = icon("chart-line")),
            menuItem("NB Model", tabName = "Correlation", icon = icon("connectdevelop"))
        )
    ),
    dashboardBody(align="center",
        tabItems(
            #First Tab content
            tabItem(tabName = "Overview",
                    h2("Finding the Right Cuisine and Features when Opening a Restaurant in the Bay Area"),
                    fluidRow(
                        infoBox("Survey Count","40",width = 4,color = "green"),
                        infoBox("Age Distribution", "20 - 40",width = 4,color = "green"),
                        infoBox("Question Styles","1 Binary, 1 Short Text and 4 Descriptive",width = 4,color = "green"),
                        h3("Overview"),
                        tags$img(src ='NLP_Overview.png', height = 450, width=800)
                        
                       
                    )),
            
            #Second tab content
            tabItem(tabName = "N-grams",
                    h2("What matters most?"),
                    fluidRow(
                        box(title = "Network Map", plotOutput("networkplot", height = 500, width = 850),width = 12 ),
                        box(title = "Control Network Graph",
                            selectInput("net", "Connection:",list("All" = 0,"Focused" = 1))
                        )
                    )),
            #Third tab
            tabItem(tabName = "Sentiment",
                    h2("Sentiments"),
                    mainPanel(
                        tabsetPanel(
                            tabPanel("Positive or Negative", 
                                     #box(title = "Experience, Dessert and Decision",selectInput("bing", "Choice:",list("Dinning Experience" = 1,"Desserts?" = 0,"Decisions"=2))),
                                     sliderInput("sentbing", "Increased Value:", min=2, max=8, value=10, step = 2),
                                     box(title = "Dinning Experience", plotOutput("q4Bing",height = 500,width = 400),width = 12),
                                     box(title = "Dessert Sentiment", plotOutput("q5Bing",height = 500,width = 400),width = 12),
                                     box(title = "Decision Sentiment", plotOutput("q3Bing",height = 500,width = 400),width = 12)), 
                            tabPanel("Emotional Sentiment", 
                                     sliderInput("sentnrc", "Increased Emotions:", min=2, max=8, value=5, step = 2),
                                     box(title = "Dinning Experience", plotOutput("q4NRC",height = 500,width = 400),width = 12),
                                     box(title = "Dessert Sentiment", plotOutput("q5NRC",height = 500,width = 400),width = 12),
                                     box(title = "Decision Sentiment", plotOutput("q3NRC",height = 500,width = 400),width = 12)), 
                            tabPanel("Word Cloud", box(title = "Dessert", plotOutput("q5NRCW",height = 500,width = 580),width = 16))))
                    
            
        ),
        #Fourth tab
        tabItem(tabName = "Frequency",
                h2("Most Popular"),
                mainPanel(
                    tabsetPanel(
                        tabPanel("Age", box(title = "Dinning Experience", plotOutput("freq1",height = 500,width = 400),width = 12)), 
                        tabPanel("Cuisine", box(title = "Dinning Experience", plotOutput("freq2",height = 500,width = 400),width = 12)), 
                        tabPanel("Dine out", box(title = "Dessert", plotOutput("freq6",height = 500,width = 550),width = 16)
                                 )
                        )
                    )
        ),
        tabItem(tabName = "Correlation",
                h2("Predictive Models"),
                mainPanel(
                    tabsetPanel(
                        tabPanel("NB Model", box(title = "Predictive Modelling", verbatimTextOutput("predict"),width = 30)), 
                        tabPanel("Prediction Accuracy",box(title = "Predictions", verbatimTextOutput("final"),width = 30),
                                 h3("Final Predictive Accuracy 81%")
                    )
                )
        )
        )
    )
)
)