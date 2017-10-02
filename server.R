library(shiny)


shinyServer(function(input, output) {
  mtcars$mpgsp <- ifelse(mtcars$mpg - 20 > 0, mtcars$mpg - 20, 0)
  model1 <- lm(hp ~ mpg, data = mtcars)
  model2 <- lm(hp ~ mpgsp + mpg, data = mtcars)
  model3 <- lm(hp ~ mpg + as.integer(cyl), data = mtcars)
  model4 <- lm(hp ~ mpgsp + mpg + as.integer(cyl), data = mtcars)
  model5 <- lm(hp ~ mpg + disp, data = mtcars)
  model6 <- lm(hp ~ mpgsp + mpg + disp, data = mtcars)
  
  
  model1pred <- reactive({
    mpgInput <- input$sliderMPG
    predict(model1, newdata = data.frame(mpg = mpgInput))
  })
  
  model2pred <- reactive({
    mpgInput <- input$sliderMPG
    predict(model2, newdata = 
              data.frame(mpg = mpgInput,
                         mpgsp = ifelse(mpgInput - 20 > 0,
                                        mpgInput - 20, 0)))
  })
  
  model3pred <- reactive({
    mpgInput <- input$sliderMPG
    cylInput1 <- as.integer(input$cylinder)
    predict(model3, newdata = data.frame(mpg = mpgInput, cyl = cylInput1))
  })
  
  model4pred <- reactive({
    mpgInput <- input$sliderMPG
    cylInput2 <- as.integer(input$cylinder)
    predict(model4, newdata = 
              data.frame(mpg = mpgInput,
                         cyl = cylInput2, 
                         mpgsp = ifelse(mpgInput - 20 > 0,
                                        mpgInput - 20, 0)))
  })
  
  model5pred <- reactive({
    mpgInput <- input$sliderMPG
    dispInput1 <- input$sliderDisp
    predict(model5, newdata = data.frame(mpg = mpgInput, disp = dispInput1))
  })
  
  model6pred <- reactive({
    mpgInput <- input$sliderMPG
    dispInput2 <- input$sliderDisp
    predict(model6, newdata = 
              data.frame(mpg = mpgInput,
                         disp = dispInput2,
                         mpgsp = ifelse(mpgInput - 20 > 0,
                                        mpgInput - 20, 0)))
  })
  
  output$docHead <- renderText("Documention")
  output$docMsg <- renderText("The interactive documentation is <a href=\"https://subodhan.shinyapps.io/ddp_project_documentation\">here</a>")
  
  output$plot1 <- renderPlot({
    mpgInput <- input$sliderMPG
    plot(mtcars$mpg, mtcars$hp, xlab = "Miles Per Gallon", 
         ylab = "Horsepower", bty = "n", pch = 16,
         xlim = c(10, 35), ylim = c(50, 350))
    if(input$showModel1){
      abline(model1, col = "red", lwd = 2)
      points(mpgInput, model1pred(), col = "red", pch = 16, cex = 2)
    }
    if(input$showModel2){
      model2lines <- predict(model2, newdata = data.frame(
        mpg = 10:35, mpgsp = ifelse(10:35 - 20 > 0, 10:35 - 20, 0)
      ))
      lines(10:35, model2lines, col = "blue", lwd = 2)
      points(mpgInput, model2pred(), col = "blue", pch = 16, cex = 2)
    }
    legend(25, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16, 
           col = c("red", "blue"), bty = "n", cex = 1.2)
  })
  
  output$plot2 <- renderPlot({
    mpgInput <- input$sliderMPG
    cylInput <- as.integer(input$cylinder)
    plot(mtcars$mpg, mtcars$hp, xlab = "Miles Per Gallon", 
         ylab = "Horsepower", bty = "n", pch = 16,
         xlim = c(10, 35), ylim = c(50, 350))
    if(input$showModel1){
      abline(model3, col = "red", lwd = 2)
      points(mpgInput, model3pred(), col = "red", pch = 16, cex = 2)
    }
    if(input$showModel2){
      model4lines <- predict(model4, newdata = data.frame(
        mpg = 10:35, mpgsp = ifelse(10:35 - 20 > 0, 10:35 - 20, 0), cyl = cylInput
      ))
      lines(10:35, model4lines, col = "blue", lwd = 2)
      points(mpgInput, model4pred(), col = "blue", pch = 16, cex = 2)
    }
    legend(25, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16, 
           col = c("red", "blue"), bty = "n", cex = 1.2)
  })
  
  output$plot3 <- renderPlot({
    mpgInput <- input$sliderMPG
    dispInput = input$sliderDisp   
    plot(mtcars$mpg, mtcars$hp, xlab = "Miles Per Gallon", 
         ylab = "Horsepower", bty = "n", pch = 16,
         xlim = c(10, 35), ylim = c(50, 350))
    if(input$showModel1){
      abline(model5, col = "red", lwd = 2)
      points(mpgInput, model5pred(), col = "red", pch = 16, cex = 2)
    }
    if(input$showModel2){
      model6lines <- predict(model6, newdata = data.frame(
        mpg = 10:35, mpgsp = ifelse(10:35 - 20 > 0, 10:35 - 20, 0), disp = dispInput
      ))
      lines(10:35, model6lines, col = "blue", lwd = 2)
      points(mpgInput, model6pred(), col = "blue", pch = 16, cex = 2)
    }
    legend(25, 250, c("Model 1 Prediction", "Model 2 Prediction"), pch = 16, 
           col = c("red", "blue"), bty = "n", cex = 1.2)
  })
})
