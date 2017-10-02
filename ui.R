library(shiny)

shinyUI(
  navbarPage("HorsePower prediction App",
             tabPanel("App",
  fluidPage(
  titlePanel("Predict Horsepower from MPG"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sliderMPG", "What is the MPG of the car?", 10, 35, value = 20),
      sliderInput("sliderDisp", "What is the Displacement of the car?", 50, 500, value = 140),
      checkboxInput("showModel1", "Show/Hide Model 1", value = TRUE),
      checkboxInput("showModel2", "Show/Hide Model 2", value = TRUE),
      radioButtons("cylinder",  "Cylinders", choices = c(4, 6, 8)),
      submitButton("Submit") # New!
    ),
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Plot",
                           plotOutput("plot1")
                  
                  ),
                  tabPanel("Model with Cyl",
                    h3("Predicted Horsepower from Model with Cylinders:"),
                    plotOutput("plot2")
                  ),
                  tabPanel("Model with Disp",
                    h3("Predicted Horsepower from Model with Displacement:"),
                    plotOutput("plot3")
                  )
      )
    )
  )
)),
tabPanel("About",
         h3("Documentation"),
         br(),
         htmlOutput("docMsg"))
))