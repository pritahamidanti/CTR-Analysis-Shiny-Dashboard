library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(car)
library(MASS)
library(multcompView)
library(stats)
library(agricolae)

# Data
set.seed(123)
CTRs <- c(2.5, 2.7, 2.8, 2.6, 3.0, 2.4, 2.9, 2.5, 2.6, 2.7,
          3.8, 3.5, 4.0, 3.7, 3.9, 3.6, 4.1, 3.4, 3.8, 3.9,
          3.1, 2.9, 3.0, 3.2, 3.3, 2.8, 3.4, 3.1, 3.2, 3.5)
Sidebar <- gl(3, 10, 30, labels = c("left", "center", "right")) 
data <- data.frame(Sidebar, CTRs)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Click-Through Rates (CTR) Analysis"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Input", tabName = "data_input", icon = icon("th")),
      menuItem("ANOVA and Tukey", tabName = "anova_tukey", icon = icon("table")),
      menuItem("Descriptive Stats", tabName = "desc_stats", icon = icon("dashboard")),
      menuItem("Regression Analysis", tabName = "regression", icon = icon("bar-chart-o")),
      menuItem("Input New Data", tabName = "manual_input", icon = icon("edit"))
    )
  ),
  dashboardBody(
    tabItems(
      # Data Input Tab
      tabItem(
        tabName = "data_input",
        fluidRow(
          box(
            title = "Data Input",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            DTOutput("tbl"),
            uiOutput("data")
          )
        )
      ),
      
      # ANOVA and Tukey Tab
      tabItem(
        tabName = "anova_tukey",
        fluidRow(
          box(
            title = "ANOVA",
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotOutput("anova_plot"),
            verbatimTextOutput("anova_summary")
          ),
          box(
            title = "Tukey HSD",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotOutput("tukey_plot"),
            verbatimTextOutput("tukey_summary")
          )
        )
      ),
      
      # Descriptive Stats Tab
      tabItem(
        tabName = "desc_stats",
        fluidRow(
          box(
            title = "Descriptive Statistics",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,  
            DTOutput("desc_stats_table")
          ),
          box(
            title = "Boxplot",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,  
            plotOutput("boxplot")
          )
        )
      ),
      
      # Regression Analysis Tab
      tabItem(
        tabName = "regression",
        fluidRow(
          box(
            title = "Regression Analysis",
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            plotOutput("regression_plot"),
            verbatimTextOutput("regression_summary")
          )
        )
      ),
      
      # Manual Input Tab
      tabItem(
        tabName = "manual_input",
        fluidRow(
          box(
            title = "Manual Input",
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            width = 12,
            textInput("left_input", "Left Value:", ""),
            textInput("center_input", "Center Value:", ""),
            textInput("right_input", "Right Value:", ""),
            actionButton("submit_btn", "Submit"),
            verbatimTextOutput("manual_input_summary")
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  input_data <- reactiveVal(data)
  
  output$tbl <- DT::renderDT({
    data <- input_data()
    DT::datatable(data, extensions = "Buttons", options = list(
      lengthChange = FALSE,
      dom = "Blfrtip",
      buttons = c("copy", "csv", "excel", "pdf", "print")
    ))
  })
  
  output$data <- renderUI({
    data <- input_data(data)
  })
  
  # ANOVA
  output$anova_plot <- renderPlot({
    fit_anova <- aov(CTRs ~ Sidebar, data = data)
    plot(fit_anova)
  })
  
  output$anova_summary <- renderPrint({
    fit_anova <- aov(CTRs ~ Sidebar, data = data)
    summary(fit_anova)
  })
  
  # Tukey HSD
  output$tukey_plot <- renderPlot({
    fit_anova <- aov(CTRs ~ Sidebar, data = data)
    tukey_results <- HSD.test(fit_anova, "Sidebar", console = TRUE, alpha = 0.05)
    plot(tukey_results)
  })
  
  output$tukey_summary <- renderPrint({
    fit_anova <- aov(CTRs ~ Sidebar, data = data)
    tukey_results <- HSD.test(fit_anova, "Sidebar", console = TRUE, alpha = 0.05)
    tukey_results
  })
  
  # Descriptive Stats
  output$desc_stats_table <- renderDT({
    summary(data)
  })
  
  # Boxplot
  output$boxplot <- renderPlot({
    ggplot(data, aes(x = Sidebar, y = CTRs)) +
      geom_boxplot(fill = "skyblue", color = "black") +
      labs(title = "Boxplot of CTRs by Sidebar", x = "Sidebar", y = "CTRs")
  })
  
  # Regression Analysis
  output$regression_plot <- renderPlot({
    fit_regression <- lm(CTRs ~ Sidebar, data = data)
    plot(fit_regression)
  })
  
  output$regression_summary <- renderPrint({
    fit_regression <- lm(CTRs ~ Sidebar, data = data)
    summary(fit_regression)
  })
  
  # Manual Input
  manual_data <- reactiveValues(data = data)
  
  observeEvent(input$submit_btn, {
    left_value <- as.numeric(input$left_input)
    center_value <- as.numeric(input$center_input)
    right_value <- as.numeric(input$right_input)
    
    new_data <- data.frame(
      Sidebar = rep(c("left", "center", "right"), each = 1),
      CTRs = c(left_value, center_value, right_value)
    )
    
    manual_data$data <- rbind(manual_data$data, new_data)
  })
  
  output$manual_input_summary <- renderPrint({
    summary(manual_data$data)
  })
}

shinyApp(ui = ui, server = server)
