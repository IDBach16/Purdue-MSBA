library(shiny)
library(readxl)
library(dplyr)
library(tidyverse)
library(plotly)
library(shinythemes)
library(sqldf)
# Load necessary libraries
library(optimx)
library(writexl)
setwd("C:/Users/ibach/OneDrive - Terillium/Desktop/Purdue MSBA/R for Analytics/Final_Project")
source("data_cleaning_modeling.R")

#setting wd should be unnecessary as long as all files are in the same directory
#setwd("C:/Users/ibach/OneDrive - Terillium/Desktop/Purdue MSBA/R for Analytics/Final_Project")
data <- read_excel("Window_Manufacturing.xlsx")
# Define the expected variables and their types
expected_variables <- list(
  "Breakage Rate" = " Target: numerical",
  "Window Type" = "Input: categorical",
  "Window Size" = "Input: numerical",
  "Glass thickness" = "Input: numerical",
  "Window color" = "Input: numerical",
  "Glass Supplier" = "Input: categorical",
  "Glass Supplier Location" = "Input: categorical",
  "Ambient Temp" = "Input: numerical",
  "Cut speed" = "Input: numerical",
  "Edge Deletion rate" = "Input: numerical",
  "Spacer Distance" = "Input: numerical",
  "Silicon Viscosity" = "Input: numerical" #,
  #   "Pass/Fail" = "categorical", #present in data dictionary, but not present in sample file
  #    "Yield" = "numerical",#present in data dictionary, but not present in sample file
  #    "YldFrctn" = "numerical"#present in data dictionary, but not present in sample file
)



###Custom functions

setTypes <- function(df) {
  
  
  numeric_vars <- c("Window Size", "Ambient Temp", "Cut speed", "Edge Deletion rate", "Spacer Distance", "Silicon Viscosity")
  
  for (col_name in numeric_vars) {
    # Check if the column exists in the dataframe
    if (col_name %in% names(df)) {
      # Convert the column to numeric
      df[[col_name]] <- as.numeric(df[[col_name]])
    }
  }
  
  
  #set categorical variables to factors
  categorical_vars <- c("Window Type", "Glass Supplier", "Glass Supplier Location")
  
  for (col_name in categorical_vars) {
    # Check if the column exists in the dataframe
    if (col_name %in% names(df)) {
      # Convert the column to numeric
      df[[col_name]] <- as.factor(df[[col_name]])
    }
  }
  
  return(df)
}

handleNAS <- function(df) {
  
  
  # Check missing data and count missing values for each column
  missing_data <- sapply(df, function(x) sum(is.na(x)))
  
  # Replace missing values in numeric columns with mean value
  numeric_vars <- c("Window Size", "Ambient Temp", "Cut speed", "Edge Deletion rate", "Spacer Distance", "Silicon Viscosity")
  for (col_name in numeric_vars) {
    # Compute median excluding NA values
    median <- median(df[[col_name]], na.rm = TRUE)
    # Replace NA values with computed mean
    df[[col_name]][is.na(df[[col_name]])] <- median
  }
  
  if (!require(sqldf)) {
  install.packages("sqldf")
}

# Load sqldf package
library(sqldf)
  
  #Most of the suppliers are in one location, we can replace the missing supplier values with the most commonly used supplier by location, and vis a versa
  
  # Step 1: Calculate the mode for "Glass Supplier" for each "Glass Supplier Location"
  mode_suppliers <- sqldf("
  SELECT 
    [Glass Supplier Location], 
    [Glass Supplier], 
    COUNT(*) as cnt
  FROM df
  WHERE [Glass Supplier] IS NOT NULL
  GROUP BY [Glass Supplier Location], [Glass Supplier]
")
  
  # Get the mode glass supplier for each location
  mode_suppliers <- sqldf("
  SELECT 
    [Glass Supplier Location], 
    [Glass Supplier]
  FROM (
    SELECT 
      [Glass Supplier Location], 
      [Glass Supplier], 
      cnt,
      RANK() OVER (PARTITION BY [Glass Supplier Location] ORDER BY cnt DESC) as rnk
    FROM mode_suppliers
  )
  WHERE rnk = 1
")
  
  # Step 2: Update the "Glass Supplier" column in the dataframe
  df <- sqldf("
  SELECT 
    d.*,
    COALESCE(d.[Glass Supplier], m.[Glass Supplier]) AS [Glass Supplier]
  FROM df d
  LEFT JOIN mode_suppliers m
  ON d.[Glass Supplier Location] = m.[Glass Supplier Location]
")
  
  # Drop the old "Glass Supplier" column and rename the new one
  df$`Glass Supplier` <- NULL
  names(df)[names(df) == "COALESCE(d.[Glass Supplier], m.[Glass Supplier])"] <- "Glass Supplier"
  
  
  
  #now let's do the locatios by mode supplier for each location:

  
  # Step 1: Calculate the mode for "Glass Supplier Location" for each "Glass Supplier"
  mode_locations <- sqldf("
  SELECT 
    [Glass Supplier], 
    [Glass Supplier Location], 
    COUNT(*) as cnt
  FROM df
  WHERE [Glass Supplier Location] IS NOT NULL
  GROUP BY [Glass Supplier], [Glass Supplier Location]
")
  
  # Get the mode glass supplier location for each supplier
  mode_locations <- sqldf("
  SELECT 
    [Glass Supplier], 
    [Glass Supplier Location]
  FROM (
    SELECT 
      [Glass Supplier], 
      [Glass Supplier Location], 
      cnt,
      RANK() OVER (PARTITION BY [Glass Supplier] ORDER BY cnt DESC) as rnk
    FROM mode_locations
  )
  WHERE rnk = 1
")
  
  # Step 2: Update the "Glass Supplier Location" column in the dataframe
  df <- sqldf("
  SELECT 
    d.*,
    COALESCE(d.[Glass Supplier Location], m.[Glass Supplier Location]) AS [Updated Glass Supplier Location]
  FROM df d
  LEFT JOIN mode_locations m
  ON d.[Glass Supplier] = m.[Glass Supplier]
")
  
  # Replace the original "Glass Supplier Location" column with the updated one
  df$`Glass Supplier Location` <- df$`Updated Glass Supplier Location`
  df$`Updated Glass Supplier Location` <- NULL
  
  
  
  # View the updated dataframe
  ##print(df)
  
  
  # Replace remaining missing values in categorical columns with mode
  categorical_vars <- c("Window Type")
  for (col_name in categorical_vars) {
    # Compute mode value
    mode_val <- as.character(names(sort(table(df[[col_name]]), decreasing = TRUE)[1]))
    # Replace NA values with computed mode
    df[[col_name]][is.na(df[[col_name]])] <- mode_val
  }
  
  return(df)
}

pre_process_data <- function(data){
  df<-data
  library(caret)
  names(df)[1] <- "y"
  # ##print(df)
  # #browser
  return(df)
}






# Define UI for application
ui <- fluidPage(
  theme = shinytheme("cyborg"),
  #theme = bs_theme(bootswatch = "minty"),
  #shinythemes::themeSelector(),  # allows us to change theme
  
  # Add a boilermaker background image
  tags$style(HTML("
    body {
      background-image: url('https://business.purdue.edu/alumni/zoom-background-images/ross-aide.jpg');
      background-attachment: fixed;
      background-size: cover;
    }
  ")),
  # Add a dark background for better content readability
  tags$style(HTML("
    /* Adjusting text color for h3 and p elements under the Introduction tab */
    .tab-content .active h3, .tab-content .active p {
        color: #E0E0E0;  /* Light gray color */
    }
    
    /* Optional: Adding a slightly darker background for better contrast */
    .tab-content .active {
        background-color: rgba(0, 0, 0, 0.7);  /* Black with 70% opacity */
        padding: 15px;  /* Some padding for better spacing */
        border-radius: 5px;  /* Rounded corners */
    }
")),
  titlePanel("Window Manufacturing Analyzer"),
  tabsetPanel(
    tabPanel("Data Upload",
             sidebarLayout(
               sidebarPanel(
                 fileInput("file", "Choose XLSX File (see the data tab for required fields)",
                           accept = c(".xlsx")),
                 h3("Data Filters"),
                 selectInput("WindowType", "Window Type", choices = NULL, multiple = TRUE, selectize = FALSE),
                 uiOutput("WindowSizeSlider"),
                 uiOutput("GlassThicknessSlider"),
                 uiOutput("WindowColorSlider"),
                 selectInput("GlassSupplier", "Glass Supplier", choices = NULL, multiple = TRUE, selectize = FALSE),
                 selectInput("GlassSupplierLocation", "Glass Supplier Location", choices = NULL, multiple = TRUE, selectize = FALSE),
                 uiOutput("AmbientTempSlider"),
                 uiOutput("CutSpeedSlider"),
                 uiOutput("EdgeDeletionRateSlider"),
                 uiOutput("SpacerDistanceSlider"),
                 uiOutput("SiliconViscositySlider")
               ),
               mainPanel(
                 tableOutput("fileContents")
               )
             )),
    tabPanel("App User Guide",
             mainPanel(
               wellPanel(
               h4("Business Problem Framing"),
               
                 p("Summit City Glass is a Fort Wayne based glass manufacturer with four suppliers. Summit City Glass wishes to reduce its breakage rate by 1%, reducing cost in the form of lower lost productivity, lower material waste and lower salvage/disposal cost of material waste. It estimates that if yield can be maintaned, a 1% reduction in breakage would save the company between $500 and $1000 dollars a day")
               ),
               wellPanel(
               h4("Analytics Problem Framing"),
               
                 p("Our tool performs analysis on a user uploaded data file containing production given within a given window. It creates a linear regression model, splitting the supplied data into a data training and data testing model, allowing it to both teach itself to predict breakage rates, and test its effectiveness against known data. Using this tool, it identifies patterns in the data, including which factors affect breakage the most, uses that data to be able to predict breakage rates for given inputs, and in turn determine the optimal cutting speed to minimize breakage with given parameters."),
                 p("The current test data does not include a cost constraint factor, such as yield. For the prototype to achieve its goal, such a constraint would need to be included and added to future iterations of the tool"),

                 p("To simplify the model, any NA/Blank Data is replaced with the median value for numerical data and the mode for categorical data. In the case of supplier and location, we can more accurately calculate modes according to their corresponding supplier/location pattern as most suppliers have only one location. While future iterations could attempt to adjust numerical values according to highly correlated fields, as calculated and shown on the predictive tab, more complete data will always produce superior models."),
                 p("The default data loaded into the tool was given to us by Summit City Glass to build and train our model."),
                 p("Clicking the download button will download an excel file containing default data file as provided by Summit City Glass and described in the data dictionary. "),
                 
                  p("Summit City Glass employees can update and retrain the model with new data using the file upload button. If the file they upload doesn't have the necessary variable fields, an error will appear below the data dictionary on this tab detailing what input is missing."),
                  h5("Filtering the data: a note of caution"),  
                 p("The current model can also be filtered after uploading the file, but users should be careful not to filter too narrowly, especially with categorical fields, as the presciptive analytics feature may not be able to adapt to the changes in the model in this iteration of the tool, causing it the applicaton to crash. Future iterations of the tool will include more advanced logic to handle dynamic changes to the model as well as better error handling around the model")
               ),
               wellPanel(
               h4("Data"),
               
               downloadButton("downloadData", "Download Default Data"),
               h4("Expected Variables in file upload"),
               verbatimTextOutput("var_list"),
               verbatimTextOutput("message")
               )

             )
    ),
    tabPanel("Descriptive Analytics",
             mainPanel(
               
               h3("Descriptive Analytics"),
               plotlyOutput(outputId="scatterPlot"),
               plotOutput(outputId="boxPlot"),
               plotlyOutput(outputId="barGraph"),
               plotlyOutput(outputId="bubbleChart")
               
             )
    ),
    tabPanel("Predictive Analytics",
             
             sidebarPanel(
               actionButton(inputId = "run", label = "Run Predictive Analysis")
             ),  
             mainPanel(
               wellPanel(
               h3("Summary Statistics"),
               tableOutput(outputId="modelResults")
               ),
               wellPanel(
               h3("Coefficient Matrix"),
               tableOutput(outputId="CoefficientResults")
               ),
               wellPanel(
               h3("Correlation Matrix"),
               tableOutput(outputId="correlationResults")
               )
             ),
             
    ),
    tabPanel("Prescriptive Analytics",
    sidebarLayout(
      sidebarPanel(
        # Sliders for non-controllable variables
       # sliderInput("ambient_temp", "Ambient Temperature", min = min_temp, max = max_temp, value = (min_temp + max_temp) / 2),
      #  sliderInput("window_size", "Window Size", min = min_size, max = max_size, value = (min_size + max_size) / 2),
      #  sliderInput("glass_thickness", "Glass Thickness", min = min_thickness, max = max_thickness, value = (min_thickness + max_thickness) / 2),
        actionButton("optimize", "Run Optimization"),
        selectInput("Window_Type_prescriptive", "Window Type", choices = NULL, multiple = FALSE, selectize = FALSE),
        uiOutput("window_size_prescriptive"),
        uiOutput("Glass_thickness_prescriptive"),
        uiOutput("window_color_prescriptive"),
        selectInput("Glass_Supplier_prescriptive", "Glass Supplier", choices = NULL, multiple = FALSE, selectize = FALSE),
        selectInput("Glass_SupplierLocation_prescriptive", "Glass Supplier Location", choices = NULL, multiple = FALSE, selectize = FALSE),
        uiOutput("ambient_temp_prescriptive"),
        # uiOutput("CutSpeedSlider"),
        # uiOutput("EdgeDeletionRateSlider"),
        # uiOutput("SpacerDistanceSlider"),
        uiOutput("SiliconViscositySlider_prescriptive"),
        
        # Numeric inputs for initial controllable variables
        numericInput("cut_speed_prescriptive", "Initial Cut Speed", value = 1.50, min = 0.32, max = 3.22),
        numericInput("edge_deletion_prescriptive", "Initial Edge Deletion Rate", value =  15.00, min = 13.75, max = 17.75),
        numericInput("spacer_distance_prescriptive", "Initial Spacer Distance", value = 12.00, min = 7.81, max = 16.20)
       
        # Action button to run optimization
        
      ),
      mainPanel(
        # Display the optimal actions
        verbatimTextOutput("optimalActions")
      )
    )
    )
    # 
    #          sidebarPanel(
    #            actionButton(inputId = "run2", label = "Run")
    #          ), 
    #          mainPanel(
    #            h3("Replace optimization results based on the predictive model"),
    #            textOutput(outputId="optResult"), 
    #            textOutput(outputId="optDecisions"),
    #            plotOutput(outputId="plot2")
    #          )
    # )
  )
)





# Define server logic
server <- function(input, output, session) {
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("default_data", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(data(), file)
    }
  )

  
  ################# Describe Tab #######################################
  
  # Render the scatterplot with a regression line
  output$scatterPlot <- renderPlotly({
    df <- filtered_data()
    
    ggplot(df, aes(x = `Cut speed`, y = `Breakage Rate`)) +
      geom_point() +  # Add points for scatterplot
      geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add a regression line
      labs(x = "Cut Speed", y = "Breakage Rate", title = "Scatterplot of Breakage Rate vs Cut Speed with Regression Line") +
      theme(plot.title = element_text(hjust = 0.5, size = 11, face = "bold"))
  })
  
  # Render the boxplot for Breakage Rate vs Window Type
  output$boxPlot <- renderPlot({
    df <- filtered_data()
    
    library(corrplot)
    
    numeric_vars <- df %>%
      select_if(is.numeric)
    
    cor_matrix <- cor(numeric_vars, use = "complete.obs")
    
    corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, addCoef.col = "black")
    
  })
  
  # Render the bubble chart for Breakage Rate, Glass Supplier, and Window Size
  output$bubbleChart <- renderPlotly({
    df <- filtered_data()
    # Create the boxplot
    ggplot(df, aes(x = `Glass Supplier`, y = `Breakage Rate`, fill = `Window Type`)) +
      geom_boxplot() +
      labs(title = "Boxplot of Breakage Rate by Glass Supplier and Window Type",
           x = "Glass Supplier",
           y = "Breakage Rate",
           fill = "Window Type") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
  })
  
  
  
  # Render the bar graph for Breakage Rate by Edge Deletion Rate
  output$barGraph <- renderPlotly({
    df_data <- filtered_data()
    
    ggplot(df_data, aes(x = `Breakage Rate`, fill = `Glass Supplier`)) +
      geom_density(alpha = 0.6) +
      labs(title = "Density Plot of Breakage Rate by Glass Supplier", x = "Breakage Rate", y = "Density") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"))
    
    
  })
  
  
  ################# Predict Tab #######################################
  
  # fit a multiple linear regression once the user hits the "Run" button
  model <- eventReactive(input$run, {
    df <- filtered_data()
    df <- pre_process_data(df)
    #browser
    # Assuming clean_and_model_data returns a list with necessary information
    results <- clean_and_model_data(df)
    
    

    
    # Extract summary information
    train_summary <- t(results$train_summary)
    test_summary <- t(results$test_summary)
    
    # Create a data frame with summary information
    # summary_data <- data.frame(
    #   Metric = c("Accuracy", "Precision", "Recall"),
    #   Training = unlist(results$train_summary$statistics),
    #   Testing = unlist(results$test_summary$statistics)
    # )
    # 
    # Return summary_data
   # summary_data
    
    stats<- as.data.frame(matrix(nrow=2,ncol=4))
    names(stats) <- c("Data Set Statistics", "RMSE", "Rsquared", "MAE")
    stats[1, 1] <- "Test"  # First cell of the first row
    stats[2, 1] <- "Train"  # First cell of the second row
    stats[1,-1]<-train_summary
    stats[2,-1]<-test_summary
    
    f<-results$model
    
    # Extract coefficients and related statistics
    coefficients_df <- summary(f)$coefficients
    
   # te<-results$test
    
    # Convert coefficients to a data frame
    coefficients_df <- as.data.frame(coefficients_df)
    
    # Add column names as a separate column for better display
    coefficients_df$Variable <- rownames(coefficients_df)
    
    # Reset row names to NULL to avoid 'invalid row.names' error
    rownames(coefficients_df) <- NULL
    
    #let's get the correlation matrix for fun too
    #results$train$y <- results$train$.outcome
    
    # Create predictors using model.matrix
    predictors <- model.matrix(exp(y) ~ ., data = results$train)  # Exclude intercept
    
    # Calculate correlation matrix for predictors
    corr_matrix <- cor(predictors)
    
    # Convert correlation matrix to data frame for display
    corr_df <- as.data.frame(corr_matrix)
    rownames(corr_df) <- colnames(corr_matrix)  # Set row names to column names
    
    # Ensure both data frames have the same number of rows (if necessary)
    min_rows <- min(nrow(coefficients_df), nrow(corr_df))
    coefficients_df <- coefficients_df[1:min_rows, ]
    corr_df <- corr_df[1:min_rows, ]
    
    return(list(stats=stats, coefficients = coefficients_df, correlations = corr_df))
    
    
    
    return(stats)
  })
  
  output$modelResults <- renderTable({
    model()$stats
  })
  
  output$CoefficientResults <- renderTable({ 
    model()$coefficients
  })
  
  output$correlationResults <- renderTable({
    model()$correlations
  }, rownames = TRUE)
  
  ################# Optimize Tab #######################################
  
  # formulate an optimization model
  
    # Define the objective function
    #objective_function <- function(cut_speed, edge_deletion, spacer_distance, window_size, non_controllable) {
    objective_function <- function(params) {   
  
      ##browser
      
      cut_speed <- params[1]
      edge_deletion <- params[2]
      spacer_distance <- params[3]
     
       df <- filtered_data()
        df <- pre_process_data(df)
    #  #browser
      # Assuming clean_and_model_data returns a list with necessary information
      results <- clean_and_model_data(df)
      
 #     #browser
      trained_model<- results$model
      
      
      new_data <- data.frame(
        XWindowSize = if (!is.null(input$window_size_prescriptive)) input$window_size_prescriptive else 0,
        XGlassthickness = if (!is.null(input$Glass_thickness_prescriptive)) input$Glass_thickness_prescriptive else 0,
        XAmbientTemp = if (!is.null(input$ambient_temp_prescriptive)) input$ambient_temp_prescriptive else 0,
        XCutspeed = cut_speed,
        XEdgeDeletionrate = if (!is.null(input$edge_deletion_prescriptive)) input$edge_deletion_prescriptive else 0,
        XSpacerDistance = if (!is.null(input$spacer_distance_prescriptive)) input$spacer_distance_prescriptive else 0,
        #XWindowcolor = if (!is.null(input$window_color_prescriptive)) input$window_color_prescriptive else 0,
        XSiliconViscosity =if (!is.null(input$SiliconViscositySlider_prescriptive)) input$SiliconViscositySlider_prescriptive else 0
      )
      
      
      
      # Handle categorical inputs
      if (!is.null(input$Window_Type_prescriptive)) {
        # Convert to dummy variables (assuming Window_Type has predefined categories)
        new_data$XWindowTypeAluminum <- ifelse(input$Window_Type_prescriptive == "Aluminum", 1, 0)
        new_data$XWindowTypeVinyl <- ifelse(input$Window_Type_prescriptive == "Vinyl", 1, 0)
        new_data$XWindowTypeWood <- ifelse(input$Window_Type_prescriptive == "Wood", 1, 0)
      }
      
      if (!is.null(input$Glass_Supplier_prescriptive)) {
        # Convert to dummy variables (assuming Glass_Supplier has predefined categories)
        new_data$XGlassSupplierSupplierA <- ifelse(input$Glass_Supplier_prescriptive == "Supplier A", 1, 0)
        new_data$XGlassSupplierSupplierB <- ifelse(input$Glass_Supplier_prescriptive == "Supplier B", 1, 0)
        new_data$XGlassSupplierSupplierC <- ifelse(input$Glass_Supplier_prescriptive == "Supplier C", 1, 0)
        new_data$XGlassSupplierSupplierD <- ifelse(input$Glass_Supplier_prescriptive == "Supplier D", 1, 0)
      }
      
      
      ##########note to future dev: filter logic cahnges the required inputs 
      #as fields are dropped and added off, find a way to set them
      #programatically to make the model more flexible! 
    
      
      # Predict y using trained model
      #trained model is logy to avoid negative predictions
      predicted_y <- exp(predict(trained_model, newdata = new_data))
      
      
      return(predicted_y)
    }
  
    # In your eventReactive block:
    model2 <- observeEvent(input$optimize, {
     
      ##browser
      
      df <- filtered_data()
      df <- pre_process_data(df)
  
      max_cut_speed <- max(df$`Cut speed`)
      min_cut_speed <- min(df$`Cut speed`)
      min_edge_deletion <- min(df$`Edge Deletion rate`)
      max_edge_deletion <- max(df$`Edge Deletion rate`)
      min_spacer_distance <- min(df$`Spacer Distance`)
      max_spacer_distance <- max(df$`Spacer Distance`)
      rm(df)
      
##browser
      
      # Initial values for controllable variables
      initial_values <- c(input$cut_speed_prescriptive)#, input$edge_deletion_prescriptive, input$spacer_distance_prescriptive)
      
      # Define constraints
      lower_bounds <- c(min_cut_speed)#, min_edge_deletion, min_spacer_distance)
      upper_bounds <- c(max_cut_speed)#, max_edge_deletion, max_spacer_distance)

      
      # Run optimization
      result <- optimx(par = initial_values, 
                       fn = objective_function, 
                       lower = lower_bounds, 
                       upper = upper_bounds, 
                       method = "L-BFGS-B")
      
      # Show the optimal actions
      output$optimalActions <- renderPrint({
       # #browser
        r<-result
        cut_speed = result$p1
       print("the ideal cut speed is: ") 
       cut_speed

      })
    })
    

  # Load the default XLSX file
  default_data <- reactive({
    read_excel("Window_Manufacturing.xlsx")
  })
  
  # Reactive expression to read the XLSX file
  data <- reactive({
    if (is.null(input$file)) {
      default_data()
    } else {
      read_excel(input$file$datapath)
    }
    
  })
  
  # Update input choices and sliders based on uploaded file
  observe({
    req(data())
    df <- data()
    df <- setTypes(df)
    df <- handleNAS(df)
    
    #filter inputs
    updateSelectInput(session, "WindowType", choices = unique(df$`Window Type`), selected = unique(df$`Window Type`))
    updateSelectInput(session, "GlassSupplier", choices = unique(df$`Glass Supplier`), selected = unique(df$`Glass Supplier`))
    updateSelectInput(session, "GlassSupplierLocation", choices = unique(df$`Glass Supplier Location`), selected = unique(df$`Glass Supplier Location`))
    
    #optimizer inputs
    updateSelectInput(session, "Window_Type_prescriptive", choices = unique(df$`Window Type`), selected = mode(df$`Window Type`))
    updateSelectInput(session, "Glass_Supplier_prescriptive", choices = unique(df$`Glass Supplier`), selected = mode(df$`Glass Supplier`))
    updateSelectInput(session, "Glass_SupplierLocation_prescriptive", choices = unique(df$`Glass Supplier Location`), selected = mode(df$`Glass Supplier Location`))
    
    
    output$WindowSizeSlider <- renderUI({
      sliderInput("WindowSize", "Window Size",
                  min = round(min(df$`Window Size`, na.rm = TRUE), 2),
                  max = round(max(df$`Window Size`, na.rm = TRUE), 2),
                  value = c(round(min(df$`Window Size`, na.rm = TRUE), 2), round(max(df$`Window Size`, na.rm = TRUE), 2)),
                  step = 0.01)
    })
    
    output$GlassThicknessSlider <- renderUI({
      sliderInput("GlassThickness", "Glass thickness",
                  min = round(min(df$`Glass thickness`, na.rm = TRUE), 2),
                  max = round(max(df$`Glass thickness`, na.rm = TRUE), 2),
                  value = c(round(min(df$`Glass thickness`, na.rm = TRUE), 2), round(max(df$`Glass thickness`, na.rm = TRUE), 2)),
                  step = 0.01)
    })
    
    output$WindowColorSlider <- renderUI({
      sliderInput("WindowColor", "Window color",
                  min = round(min(df$`Window color`, na.rm = TRUE), 2),
                  max = round(max(df$`Window color`, na.rm = TRUE), 2),
                  value = c(round(min(df$`Window color`, na.rm = TRUE), 2), round(max(df$`Window color`, na.rm = TRUE), 2)),
                  step = 0.01)
    })
     output$window_color_prescriptive <- renderUI({ 
       
       
       sliderInput("window_color_prescriptive", "Window Color", 
                  min = round(min(df$`Window color`, na.rm = TRUE), 2), 
                  max = round(max(df$`Window color`, na.rm = TRUE), 2), 
                  value = round(mean(df$`Window color`, na.rm = TRUE),2),
                  step=0.01)
     })
     
     
     output$window_size_prescriptive <- renderUI({ 
       
       
       sliderInput("window_size_prescriptive", "Window Size", 
                   min = round(min(df$`Window Size`, na.rm = TRUE), 2), 
                   max = round(max(df$`Window Size`, na.rm = TRUE), 2), 
                   value = round(mean(df$`Window Size`, na.rm = TRUE),2),
                   step=0.01)
     })
     
     output$Glass_thickness_prescriptive <- renderUI({ 
       
       
       sliderInput("Glass_thickness_prescriptive", "Glass Thickness", 
                   min = round(min(df$`Glass thickness`, na.rm = TRUE), 2), 
                   max = round(max(df$`Glass thickness`, na.rm = TRUE), 2), 
                   value = round(mean(df$`Glass thickness`, na.rm = TRUE),2),
                   step=0.01)
     })
     
     output$ambient_temp_prescriptive <- renderUI({ 
       
       
       sliderInput("ambient_temp_prescriptive", "Ambient Temperature", 
                   min = round(min(df$`Ambient Temp`, na.rm = TRUE), 2), 
                   max = round(max(df$`Ambient Temp`, na.rm = TRUE), 2), 
                   value = round(mean(df$`Ambient Temp`, na.rm = TRUE),2),
                   step=0.01)
     })
     
     output$SiliconViscositySlider_prescriptive <- renderUI({ 
       
       
       sliderInput("SiliconViscositySlider_prescriptive", "Silicone Viscosity", 
                   min = round(min(df$`Ambient Temp`, na.rm = TRUE), 2), 
                   max = round(max(df$`Ambient Temp`, na.rm = TRUE), 2), 
                   value = round(mean(df$`Ambient Temp`, na.rm = TRUE),2),
                   step=0.01)
     })
     

     
     
    output$AmbientTempSlider <- renderUI({
      sliderInput("AmbientTemp", "Ambient Temp",
                  min = round(min(df$`Ambient Temp`, na.rm = TRUE), 2),
                  max = round(max(df$`Ambient Temp`, na.rm = TRUE), 2),
                  value = c(round(min(df$`Ambient Temp`, na.rm = TRUE), 2), round(max(df$`Ambient Temp`, na.rm = TRUE), 2)),
                  step = 0.01)
    })
    
    output$CutSpeedSlider <- renderUI({
      sliderInput("CutSpeed", "Cut speed",
                  min = round(min(df$`Cut speed`, na.rm = TRUE), 2),
                  max = round(max(df$`Cut speed`, na.rm = TRUE), 2),
                  value = c(round(min(df$`Cut speed`, na.rm = TRUE), 2), round(max(df$`Cut speed`, na.rm = TRUE), 2)),
                  step = 0.01)
    })
    
    output$EdgeDeletionRateSlider <- renderUI({
      sliderInput("EdgeDeletionRate", "Edge Deletion rate",
                  min = round(min(df$`Edge Deletion rate`, na.rm = TRUE), 2),
                  max = round(max(df$`Edge Deletion rate`, na.rm = TRUE), 2),
                  value = c(round(min(df$`Edge Deletion rate`, na.rm = TRUE), 2), round(max(df$`Edge Deletion rate`, na.rm = TRUE), 2)),
                  step = 0.01)
    })
    
    output$SpacerDistanceSlider <- renderUI({
      sliderInput("SpacerDistance", "Spacer Distance",
                  min = round(min(df$`Spacer Distance`, na.rm = TRUE), 2),
                  max = round(max(df$`Spacer Distance`, na.rm = TRUE), 2),
                  value = c(round(min(df$`Spacer Distance`, na.rm = TRUE), 2), round(max(df$`Spacer Distance`, na.rm = TRUE), 2)),
                  step = 0.01)
    })
    
    output$SiliconViscositySlider <- renderUI({
      sliderInput("SiliconViscosity", "Silicon Viscosity",
                  min = round(min(df$`Silicon Viscosity`, na.rm = TRUE), 2),
                  max = round(max(df$`Silicon Viscosity`, na.rm = TRUE), 2),
                  value = c(round(min(df$`Silicon Viscosity`, na.rm = TRUE), 2), round(max(df$`Silicon Viscosity`, na.rm = TRUE), 2)),
                  step = 0.01)
    })
    
    
    # 
    # numericInput("cut_speed_prescriptive", "Initial Cut Speed", value = (min_cut_speed + max_cut_speed) / 2, min = min_cut_speed, max = max_cut_speed),
    # numericInput("edge_deletion_prescriptive", "Initial Edge Deletion Rate", value = (min_edge_deletion + max_edge_deletion) / 2, min = min_edge_deletion, max = max_edge_deletion),
    # numericInput("spacer_distance_prescriptive", "Initial Spacer Distance", value = (min_spacer_distance + max_spacer_distance) / 2, min = min_spacer_distance, max = max_spacer_distance),
    # 
    
    
  })
  
  # Reactive expression to filter the data based on inputs
  filtered_data <- reactive({
    req(data())
    df <- data()
    df<-setTypes(df)
    df<-handleNAS(df)
    
    if (length(input$WindowType) > 0) {
      df <- df %>% filter(`Window Type` %in% input$WindowType)
    }
    df <- df %>% filter(`Window Size` >= input$WindowSize[1] & `Window Size` <= input$WindowSize[2])
    df <- df %>% filter(`Glass thickness` >= input$GlassThickness[1] & `Glass thickness` <= input$GlassThickness[2])
    df <- df %>% filter(`Window color` >= input$WindowColor[1] & `Window color` <= input$WindowColor[2])
    if (length(input$GlassSupplier) > 0) {
      df <- df %>% filter(`Glass Supplier` %in% input$GlassSupplier)
    }
    if (length(input$GlassSupplierLocation) > 0) {
      df <- df %>% filter(`Glass Supplier Location` %in% input$GlassSupplierLocation)
    }
    df <- df %>% filter(`Ambient Temp` >= input$AmbientTemp[1] & `Ambient Temp` <= input$AmbientTemp[2])
    df <- df %>% filter(`Cut speed` >= input$CutSpeed[1] & `Cut speed` <= input$CutSpeed[2])
    df <- df %>% filter(`Edge Deletion rate` >= input$EdgeDeletionRate[1] & `Edge Deletion rate` <= input$EdgeDeletionRate[2])
    df <- df %>% filter(`Spacer Distance` >= input$SpacerDistance[1] & `Spacer Distance` <= input$SpacerDistance[2])
    df <- df %>% filter(`Silicon Viscosity` >= input$SiliconViscosity[1] & `Silicon Viscosity` <= input$SiliconViscosity[2])
    
    
    
    
    
    mainDF<-df
    
    
  
    df
  })
  
  # Render the filtered data table
  output$fileContents <- renderTable({
    df<-filtered_data()
    
    # Check if all expected variables are in the dataframe
    missing_vars <- setdiff(names(expected_variables), colnames(df))
    if (length(missing_vars) == 0) {
      df
    } else {
      stop(paste("Missing required variables:", paste(missing_vars, collapse = ", ")))
    }
  })
  
  # Display a message if there are missing variables
  output$message <- renderText({
    req(data())
    df <- data()
    
    missing_vars <- setdiff(names(expected_variables), colnames(df))
    if (length(missing_vars) == 0) {
      "All required variables are present."
    } else {
      paste("Missing required variables:", paste(missing_vars, collapse = ", "))
    }
  })
  
  
  
  # Display the list of expected variables
  output$var_list <- renderText({
    # Create a character vector to store name and value pairs
    info <- lapply(names(expected_variables), function(name) {
      paste(name, expected_variables[[name]], sep = ": ")
    })
    # Collapse the vector into a single string with line breaks
    paste(info, collapse = "\n")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)