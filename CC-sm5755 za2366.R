library(shiny)
library(ggplot2)

# Sample dataset (simplified employee performance data)
set.seed(123)
data <- data.frame(
  EmployeeID = 1:100,
  Department = sample(c("Sales", "Marketing", "IT", "HR"), 100, replace = TRUE),
  YearsExperience = sample(1:15, 100, replace = TRUE),
  MonthlyHours = round(runif(100, 120, 200)),
  PerformanceScore = round(runif(100, 50, 100))
)

# Define UI for the app with added sections for details, summary, and problem statement
ui <- fluidPage(
  # CSS for styling
  tags$head(
    tags$style(HTML("
      body {background-color: #f7f9fc; font-family: Arial, sans-serif;}
      .title-panel {text-align: center; margin-top: 20px; color: #2C3E50;}
      .sub-title {color: #34495E; font-size: 16px;}
      .summary-panel, .problem-panel {background-color: #ecf0f1; padding: 20px; border-radius: 8px; margin: 20px 0;}
      .data-panel {margin-top: 20px;}
      h2 {color: #2C3E50;}
      h4 {color: #34495E;}
      .plot-container {margin-top: 30px;}
    "))
  ),
  
  # Title Panel
  div(class = "title-panel",
      h1("Exploratory Data Analysis and Visualization(STATGR5702)"),
      h4("Master in Data Science"),
      h4("Sneha Maurya & Zubair Atha"),
      h4("Course: Exploratory Data Analysis and Visualization")
  ),
  
  # Summary and Problem Statement Panel
  div(class = "summary-panel",
      h2("Summary of the Data"),
      p("This dataset simulates employee performance data with attributes including Department, Years of Experience, Monthly Hours, and Performance Score. The dataset is intended for analysis and visualization to uncover insights related to employee productivity and performance metrics.")
  ),
  
  div(class = "problem-panel",
      h2("Problem Statement"),
      p("The objective of this analysis is to explore relationships among various employee attributes such as department, experience, hours worked, and performance score, to gain insights into factors influencing performance and productivity. This analysis is powered by R's Shiny framework for creating interactive web applications, alongside ggplot2 for advanced data visualization, enabling dynamic exploration of data relationships and patterns.")
  ),
  
  # Sidebar layout for controls and plot
  sidebarLayout(
    sidebarPanel(
      selectInput("plotType", "Choose Plot Type:", 
                  choices = c("Scatter Plot", "Bar Plot", "Line Plot", "Histogram")),
      selectInput("xvar", HTML("X-axis Variable: <span title='Choose the variable for the X-axis'>?</span>"), 
                  choices = c("YearsExperience", "MonthlyHours", "Department", "PerformanceScore")),
      uiOutput("yvar_ui"), # Dynamic UI for y-variable based on plot type
      selectInput("colorVar", HTML("Color By: <span title='Choose a variable to color by for better differentiation'>?</span>"), choices = c("None", "Department", "YearsExperience")),
      width = 3,
      style = "background-color: #ecf0f1; padding: 20px; border-radius: 8px;"
    ),
    mainPanel(
      div(class = "plot-container",
          tabsetPanel(
            tabPanel("Main Plot", 
                     plotOutput("dynamicPlot", height = "600px"),
                     textOutput("plotExplanation")),
            tabPanel("Summary Plot",
                     plotOutput("summaryPlot", height = "600px"),
                     h4("Summary of Performance by Department"))
          )
      ),
      width = 9
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Dynamically show or hide the Y-axis variable based on plot type
  output$yvar_ui <- renderUI({
    if (input$plotType == "Histogram") {
      return(NULL) # No Y-axis selection for Histogram
    } else {
      selectInput("yvar", HTML("Y-axis Variable: <span title='Choose the variable for the Y-axis'>?</span>"), 
                  choices = c("PerformanceScore", "MonthlyHours", "YearsExperience"))
    }
  })
  
  # Generate the main plot based on user inputs
  output$dynamicPlot <- renderPlot({
    plot_data <- data
    x <- input$xvar
    y <- if (input$plotType == "Histogram") NULL else input$yvar
    color_var <- if (input$colorVar == "None") NULL else input$colorVar
    
    # Initialize ggplot with conditional color/fill aesthetics
    p <- ggplot(plot_data, aes_string(x = x, y = y, color = color_var, fill = color_var)) +
      theme_minimal(base_size = 15) +
      labs(
        title = paste(input$plotType, "of Employee Performance"),
        x = x,
        y = ifelse(is.null(y), "", y)
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#34495E"),
        axis.title = element_text(face = "bold", color = "#34495E"),
        axis.text = element_text(color = "#34495E"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold")
      )
    
    # Check if color_var is categorical or continuous and apply the correct scale
    if (!is.null(color_var)) {
      if (is.numeric(plot_data[[color_var]])) {
        p <- p + scale_color_gradient(low = "#3498db", high = "#e74c3c")
      } else {
        p <- p + scale_color_manual(values = c("Sales" = "#1abc9c", "Marketing" = "#e67e22", "IT" = "#9b59b6", "HR" = "#f39c12"))
      }
    }
    
    # Choose the plot type based on user selection
    if (input$plotType == "Scatter Plot") {
      p <- p + geom_point(size = 4, alpha = 0.7)
    } else if (input$plotType == "Bar Plot") {
      p <- p + geom_bar(stat = "identity", position = "dodge", color = "black", width = 0.7)
    } else if (input$plotType == "Line Plot") {
      p <- p + geom_line(size = 1.5, alpha = 0.8)
    } else if (input$plotType == "Histogram") {
      p <- ggplot(plot_data, aes_string(x = x, fill = color_var)) +
        geom_histogram(binwidth = 5, color = "black", alpha = 0.7) +
        theme_minimal(base_size = 15) +
        labs(
          title = "Histogram of Employee Performance",
          x = x,
          y = "Count"
        ) +
        theme(
          plot.title = element_text(hjust = 0.5, face = "bold", color = "#34495E"),
          axis.title = element_text(face = "bold", color = "#34495E"),
          axis.text = element_text(color = "#34495E")
        )
      if (!is.null(color_var) && is.numeric(plot_data[[color_var]])) {
        p <- p + scale_fill_gradient(low = "#3498db", high = "#e74c3c")
      }
    }
    p
  })
  
  # Explanation text for each plot type
  output$plotExplanation <- renderText({
    switch(input$plotType,
           "Scatter Plot" = "Scatter Plot: Useful for examining correlations between variables, like YearsExperience vs. PerformanceScore.",
           "Bar Plot" = "Bar Plot: Helps compare categorical data, such as average performance by department.",
           "Line Plot" = "Line Plot: Shows trends over time, which may not be directly applicable here but can illustrate trends related to experience or hours.",
           "Histogram" = "Histogram: Great for understanding distributions, like the distribution of monthly hours or performance scores."
    )
  })
  
  # Summary plot
  output$summaryPlot <- renderPlot({
    ggplot(data, aes(x = Department, y = PerformanceScore, fill = Department)) +
      geom_boxplot() +
      theme_minimal(base_size = 15) +
      labs(
        title = "Summary of Performance by Department",
        x = "Department",
        y = "Performance Score"
      ) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold", color = "#34495E"),
        axis.title = element_text(face = "bold", color = "#34495E"),
        axis.text = element_text(color = "#34495E")
      )
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


