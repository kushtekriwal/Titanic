library(shiny)
library(rpart)
library(vcdExtra)

fit.titanic = rpart(survived ~ ., data = Titanicp)
  
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Titanic survivors prediction"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        radioButtons("Pclass",
                     "Passenger Class:",
                     choices = list("1st","2nd","3rd"),
                     selected = "1st"),
         radioButtons("Sex",
                     "Sex:",
                     choices = list("male", "female"),
                     selected = "male"),
         sliderInput("Age",
                     "Age:",
                     min = 0,
                     max = 80,
                     step = 1,
                     value = 30),
         sliderInput("Sibsp",
                     "Siblings and Spouses:",
                     min = 0,
                     max = 8,
                     step = 1,
                     value = 1),
         sliderInput("Parch",
                     "Parents and Children:",
                     min = 0,
                     max = 6,
                     step = 1,
                     value = 1)
      ),
      # Show a plot of the generated distribution
      mainPanel(
        textOutput("prediction"),
        plotOutput("tree")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$prediction <- renderText({
    new_data = data.frame(
      pclass = input$Pclass,
      sex = input$Sex,
      age = input$Age,
      sibsp = input$Sibsp,
      parch = input$Parch
    )
    titanic.predict <- predict(fit.titanic, new_data, type = "class")
    paste("The person ", titanic.predict, ".", sep = "")
  })
  output$tree <- renderPlot({
    #fancyRpartPlot(fit.iris)
    plot(fit.titanic)
    text(fit.titanic)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

