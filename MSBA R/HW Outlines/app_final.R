################################################################################
# A simple example integrating descriptive, predictive, and prescriptive
# analytics into a shiny app
#
################################################################################
# https://shiny.posit.co/
# https://rstudio.github.io/shinythemes/
#install.packages("shinythemes")
library(shiny)
library(shinythemes)

ui <- fluidPage(
  theme = shinytheme("flatly"),
  #theme = bs_theme(bootswatch = "minty"),
  #shinythemes::themeSelector(),  # allows us to change theme
  
  titlePanel("Using R for Analytics"),
  mainPanel(
    navbarPage(
      tabPanel("--"),
      tabPanel("Data",
               sidebarPanel(
                            fileInput(inputId="myCSV", label="Load CSV"
                                      , accept=".csv", buttonLabel="Upload"
                                      , placeholder ="", multiple=F, width='100%'
                                      )
                            ), 
                            tableOutput(outputId="contents")
               ),
      tabPanel("Describe",
               plotOutput(outputId="hist")
               ),
      tabPanel("Predict",
               sidebarPanel(
                            actionButton(inputId = "run", label = "Run")
               ), 
               textOutput(outputId="modelResults") 
               ),
      tabPanel("Optimize",
               sidebarPanel(
                            actionButton(inputId = "run2", label = "Run")
               ), 
               textOutput(outputId="optResult"), 
               textOutput(outputId="optDecisions"),
               plotOutput(outputId="plot2")
               )
    )
  )
)

server = function(input, output) {

################# Data Tab #######################################
  # Load in our data as a reactive dataset
  data <- reactive({
    # your csv file
    file <- input$myCSV
    # check to make sure it's a CSV
    ext <- tools::file_ext(file$datapath)
    req(file)
    validate(need(ext == "csv", "Please upload a csv file"))
    # read it in
    myData <- read.table(file=file$datapath, header=T, sep=",")
    names(myData) <- c("y","x1","x2")
    myData
  })
  
  # Display our data as a table 
  output$contents <- renderTable({
    data()
  })

################# Describe Tab #######################################
  
  output$hist <- renderPlot({ 
    
    par(mfrow=c(1,1))
    library(scatterplot3d)
    plot3d <- scatterplot3d(x = data()$x1, y = data()$x2, z = data()$y
                            , xlab="x1", ylab="x2", zlab="y"
                            , xlim=c(0,14), ylim=c(0,14), zlim=c(0,40)
                            , bg="black", color="white", angle=65, scale.y=1
                            , cex.symbols=1, pch=16
                            , box=F, col.grid="grey", lty.grid=par("lty")
    )
    # add light grey grid
    source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
    addgrids3d(data(), grid=c("xy", "xz", "yz")
               , angle = 65, xlim=c(0,14), ylim=c(0,14), zlim=c(0,40)
               , scale.y=0.7
               )
    # put points back on top of grid
    plot3d$points3d(x=data()$x1, y=data()$x2, z=data()$y, type="p"
                    , pch=16, cex=1, col="black")
    
    # fit a multiple linear regression to add a plane to the graph
    f <- lm(data()$y ~ data()$x1 + data()$x2)
    summary(f)
    
    # add the fitted regression plane to the 3d-plot
    plot3d$plane3d(f, lty.box="dashed", draw_lines=T, draw_polygon=T)
  })
  
  ################# Predict Tab #######################################
  
  # fit a multiple linear regression once the user hits the "Run" button
  model <- eventReactive(input$run, {
    f <- lm(y ~ ., data=data())
    data.frame(summary(f)[["coefficients"]])[,1]
  })
  
  output$modelResults <- renderText({ 
    model()
  })
  
  ################# Optimize Tab #######################################

  # formulate an optimization model 
  model2 <- eventReactive(input$run2, {
    library(lpSolveAPI)
    library(lpSolve)
    # there are two decision variables
    (lps.model <- make.lp(nrow=0, ncol=3))
    # real decision variables
    set.type(lps.model, columns=1, type="real")
    set.type(lps.model, columns=2, type="real")
    # set objective function
    lp.control(lps.model, sense="max")
    set.objfn(lps.model, obj=model())
    # define constraints
    add.constraint(lps.model, c(1,0,0), "=", 1)       #intercept term
    add.constraint(lps.model, c(0,1,0), ">=", 2)      #box constraint
    add.constraint(lps.model, c(0,1,0), "<=", 10)     #box constraint
    add.constraint(lps.model, c(0,0,1), ">=", 2)      #box constraint
    add.constraint(lps.model, c(0,0,1), "<=", 10)     #box constraint
    add.constraint(lps.model, c(0,10,20), "<=", 225)  #cost constraint
    # final model that is ready to be solved
    lps.model 
  })
  
  output$optResult <- renderText({
    solve(model2())
    paste("Optimal Objective:",round(get.objective(model2()),2))
  }) 
  
  output$optDecisions <- renderText({
    paste("Decisions:",data.frame(get.variables(model2())))
  }) 
  
  output$plot2 <- renderPlot({ 
    
    par(mfrow=c(1,1))
    library(scatterplot3d)
    plot3d <- scatterplot3d(x = data()$x1, y = data()$x2, z = data()$y
                            , xlab="x1", ylab="x2", zlab="y"
                            , xlim=c(0,12), ylim=c(0,12), zlim=c(0,40)
                            , bg="black", color="white", angle=65, scale.y=0.7
                            , cex.symbols=1.6, pch=16
                            , box=F, col.grid="grey", lty.grid=par("lty")
    )
    # add light grey grid
    source('http://www.sthda.com/sthda/RDoc/functions/addgrids3d.r')
    addgrids3d(data(), grid=c("xy", "xz", "yz")
               , angle = 65, xlim=c(0,12), ylim=c(0,12), zlim=c(0,40)
               , scale.y=0.7
    )
    # put points back on top of grid
    plot3d$points3d(x=data()$x1, y=data()$x2, z=data()$y, type="p"
                    , pch=16, cex=1, col="black")
    
    # fit a multiple linear regression to add a plane to the graph
    f <- lm(data()$y ~ data()$x1 + data()$x2)
    summary(f)
    
    # add the fitted regression plane to the 3d-plot
    plot3d$plane3d(f, lty.box="dashed", draw_lines=T, draw_polygon=T)
    
    solve(model2())
    # plot optimal decision point
    plot3d$points3d(x=get.variables(model2())[2]
                    , get.variables(model2())[3]
                    , z=round(get.objective(model2()),2)
                    , type="p"
                    , pch=16, cex=1, col="red")
    
  })
  
}

shinyApp(ui = ui, server = server)