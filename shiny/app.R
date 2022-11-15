#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

data(mtcars)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mtcars Data"),
    h4("Author: Ji"),
    h4("Date: 11/14/2022"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "am", 
                        label = "Transmission",
                        choices = list("automatic" = 0, "manual" = 1,
                                       selected = 1)),
            sliderInput(inputId = "wt",
                        label = "Wt (1000 lbs):",
                        min = 1.6,
                        max = 5.4,
                        value = 30),
            sliderInput(inputId = "qsec",
                        label = "Qsec (1/4 mile time):",
                        min = 14.5,
                        max = 22.5,
                        value = 30)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput(outputId = "distPlot"),
           h4(textOutput("para")),
           h4(textOutput("pred"))
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$para<-renderText({
      paste("You have selected Transmission =",input$am,", Wt =",input$wt,"and Qsec =",input$qsec,".")
    })

    output$distPlot <- renderPlot({
      # split data according to am input
      mtcars_v1 <- mtcars[which(mtcars$am == input$am),]
      # fit mpg ~ wt_+sec
      fit <- lm(mpg~wt+qsec, mtcars_v1)
      # predict mpg
      pred <- predict(fit,
                      newdata = data.frame(
                        wt=input$wt,
                        qsec=input$qsec,
                        am=input$am))
      # plot with the specified am value
        plot <- ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
          stat_smooth(method = "lm", col = "red") +
          labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 4),
                             " mpg =",signif(fit$coef[[2]],4),"Wt +", 
                             signif(fit$coef[[3]],4),"Qsec +" , signif(fit$coef[[1]],4),
                             " P =",signif(summary(fit)$coef[2,4], 4))) +
          geom_point(data = mtcars_v1, aes(x=wt, y = mpg, size = qsec),color = "blue")
        
        plot
    })
    output$pred <- renderText({
      # split data according to am input
      mtcars_v1 <- mtcars[which(mtcars$am == input$am),]
      # fit mpg ~ wt_+sec
      fit <- lm(mpg~wt+qsec, mtcars_v1)
      pred <- predict(fit,
                      newdata = data.frame(
                        wt=input$wt,
                        qsec=input$qsec,
                        am=input$am))
      paste("The predict mpg is:", round(pred,digits = 3),"miles/(US) gallon.")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
