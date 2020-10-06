#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Regression Algorithms on Dataset"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
  
      selectInput("select_reg", "Select type",
                  choices = c("Simple linear regression", "Multiple linear regression")),
      
      fileInput("file1", "Choose CSV File",
                accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      uiOutput("subpred"),
      conditionalPanel(condition = "input.select_reg == 'Simple linear regression'",
                       uiOutput("subind")),
      conditionalPanel(condition = "input.select_reg == 'Multiple linear regression'",
                       uiOutput("select_mul"))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      conditionalPanel(condition = "input.select_reg == 'Simple linear regression'",
      h2("Simple Linear Regression"),
      h3("Scatter Plot"),
      plotOutput("scatter"),
      plotOutput("plot_summ"),
      h3("Applying Simple Linear Regression"),
      verbatimTextOutput("slr"),
      h3("Summary of Regression Model"),
      verbatimTextOutput("summary_slr"),
      plotOutput("xplot"),
      p("R-squared is a statistical measure of how close the data are to the fitted regression line. It is also known as the coefficient of determination, or the coefficient of multiple determination for multiple regression. In general, the higher the R-squared, the better the model fits your data. "),
      h2("Goodness of fit"),
      verbatimTextOutput("goodness"),
      uiOutput("pred"),
      verbatimTextOutput("prediction"),
      uiOutput("show_data"),
      tableOutput("data")
      ),
      conditionalPanel(condition = "input.select_reg == 'Multiple linear regression'",
                       h2("Multiple Linear Regression"),
                       h3("Pearson's product-moment correlation coefficient"),
                       verbatimTextOutput("cor_mul"),
                       plotOutput("plot_mul"),
                       h3("Applying Multiple Linear Regression"),
                       verbatimTextOutput("mul_reg"),
                       h3("Summary of Multiple Regression"),
                       verbatimTextOutput("mul_sum"),
                       p("R-squared is a statistical measure of how close the data are to the fitted regression line. It is also known as the coefficient of determination, or the coefficient of multiple determination for multiple regression. In general, the higher the R-squared, the better the model fits your data. "),
                       h2("Goodness of fit"),
                       verbatimTextOutput("mul_goodness"),
                       uiOutput("show_data_mul"),
                       tableOutput("mul_data"),
                       uiOutput("pred_value"),
                       verbatimTextOutput("mul_prediction")
                       )
      
    
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  file <- reactive({ inFile <- input$file1
  sample_data <- read.csv(inFile$datapath)})
  
  output$subBoxPlot <- renderPlot({
    sample_data = file()
    y <- sample_data[,input$predict]
    x <- sample_data[,input$ind][]
  })
  
  output$plot <- renderPlot({
    sample_data = file()
    y <- sample_data[,input$predict]
    x <- sample_data[,input$ind]
    plot(y, x , main = "Scatter Plot")
    abline(lm(x ~ y))
  })
  
  output$subpred <- renderUI({
    sample_data = file()
    selectInput("predict","Select dependent Variable:", choices=names(sample_data)) 
  })
  
  output$subind <- renderUI({
    sample_data = file()
    selectInput("ind","Select indepedent Variable:", choices = names(sample_data)[!names(sample_data)==input$predict])
  })
  
  output$select_mul <- renderUI({
    sample_data = file()
    selectInput("mul", "Select independent Variable: ", choices = names(sample_data)[!names(sample_data)==input$predict], multiple = TRUE)
      })
  
  output$pred <- renderUI({
    textInput("pred_variable", "Enter Value : ")
  })
  
  output$show_data <- renderUI({
    textInput("sh_data", "Enter Value : ")
  })
  
  output$show_data_mul <- renderUI({
    textInput("sh_data1", "Enter Value : ")
  })
  
  output$data <- renderTable({
    sample_data <- file()
    y <- sample_data[, input$predict]
    x <- sample_data[, input$ind]
    s_data <- data.frame(x,y)
    n <- as.numeric(input$sh_data)
    head(s_data, n)
  })
  
  
  output$mul_data <- renderTable({
    sample_data <- file()
    y <- sample_data[, (input$predict)]
    x <- sample_data[, (input$mul)]
    s_data <- data.frame(x,y)
    n <- as.numeric(input$sh_data1)
    head(s_data, n)
  })
  
  output$cor_mul <- renderPrint({
    sample_data <- file()
    sample_data <- data.frame(sample_data)
    y <- sample_data[!is.na(rowMeans(sample_data[,(input$mul)])),(input$predict)]
    x <- sample_data[!is.na(rowMeans(sample_data[,(input$mul)])),(input$mul)]
    df <- data.frame(x,y)
    cor(df)
    
  })

  
  output$plot_summ <- renderPlot({
    sample_data <- file()
    y <- sample_data[, input$predict]
    x <- sample_data[, input$ind]
    
    df <- data.frame(x,y)
    
    names(df)[1] = input$predict
    names(df)[2] = input$ind
    print(df)
    library(psych)
    pairs.panels(df,
                 method = "pearson",
                 density = TRUE,
                 ellipses = FALSE)
    pairs.panels(df)
  })
  
  output$plot_mul <- renderPlot({
    sample_data <- file()
    y <- sample_data[, (input$predict)]
    x <- sample_data[, (input$mul)]
    df <- data.frame(y,x)
    pairs.panels(df)
  })
  
  output$mul_reg <- renderPrint({
    sample_data <- file()
    y <- sample_data[, (input$predict)]
    x <- sample_data[, (input$mul)]
    df <- data.frame(x,y)
    model <- lm(y~., data = df)
    model
  })
  
  output$mul_sum <- renderPrint({
    sample_data <- file()
    y <- sample_data[, (input$predict)]
    x <- sample_data[, (input$mul)]
    df <- data.frame(x,y)
    model <- lm(y~., data = df)
    result <- summary(model)
    result
  })
  
  output$mul_goodness <- renderPrint({
    sample_data <- file()
    y <- sample_data[, (input$predict)]
    x <- sample_data[, (input$mul)]
    df <- data.frame(x,y)
    model <- lm(y ~ ., data = df)
    result <- summary(model)
    cat("Goodness of fit :",result$r.squared*100)
  })
  
  output$slr <- renderPrint({
    sample_data <- file()
    y <- sample_data[, input$predict]
    x <- sample_data[, input$ind]
    result <- lm(y ~ x )
    result
  })
  
  output$summary_slr <- renderPrint({
    sample_data <- file()
    y <- sample_data[, input$predict]
    x <- sample_data[, input$ind]
    
    result <- lm(y ~ x)
    summary(result)
    
  })
  
  output$xplot <- renderPlot({
    sample_data <- file()
    y <- sample_data[, input$predict]
    x <- sample_data[, input$ind]
    
    result <- lm(y ~ x)
    plot(x, y)
    abline(result)
  })
  
  output$scatter <- renderPlot({
    sample_data = file()
    y <- sample_data[, input$predict]
    x <- sample_data[, input$ind]
    df = data.frame(x,y)
    par(fig = c(0,0.8,0,0.8), new = TRUE)
    plot(x,y)
    par(fig = c(0,0.8,0.55,1), new = TRUE)
    boxplot(y, horizontal = TRUE, axes = TRUE)
    par(fig = c(0.7,1,0,0.8), new = TRUE)
    boxplot(x, axes = TRUE)
  })
  
  output$goodness <- renderPrint({
    sample_data <- file()
    y <- sample_data[, input$predict]
    x <- sample_data[, input$ind]
    
    model <- lm(y ~ x)
    result <- summary(model)
    
    cat("Goodness of Fit: ",result$r.squared*100)
    
    
  })
  
  output$prediction <- renderPrint({
    sample_data <- file()
    y <- sample_data[, input$predict]
    x <- sample_data[, input$ind]
    df=data.frame(x,y)
    model <- lm(y ~ x, data = df)
    
    result <- predict(model, data.frame(x=as.numeric(input$pred_variable)), interval = "prediction", level = 0.95)
    
    cat("Predicted value of ", input$pred_variable, "is ", result[1])
  })
  
  output$pred_value <- renderUI({
    textInput("mulpred", "Enter Value to predict : ")
  })
  
  output$mul_prediction <- renderPrint({
    sample_data <- file()
    y <- sample_data[, (input$predict)]
    new_y <- y[!is.na(rowMeans(y)),]
    x <- sample_data[, (input$mul)]
    new_x <- x[!is.na(rowMeans(y))]
    
    newdf <- data.frame(x,y)
    model <- lm(y ~ ., data = newdf)
    
    v1 <- as.numeric(strsplit(input$mulpred,",")[[1]])
    z <- as.data.frame(matrix(v1,1,length(v1)))
    names(z) <- (input$x)
    cat("Predicted value:",predict(model,z)[1])
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


