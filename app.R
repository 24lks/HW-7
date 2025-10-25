library(shiny)
library(shinyalert)
library(tidyverse)

source("helpers.R")

# Load sample data outside the UI
my_sample <- readRDS("my_sample_temp.rds")

# Define UI
ui <- fluidPage(
  "Add a title panel here!",
  sidebarLayout(
    sidebarPanel(
      h2("Select Variables to Find Correlation:"),
      selectInput(
        inputId = "x_var",
        label = "x Variable",
        choices = list(
          "Total person's income",
          "Water cost",
          "Electricity cost",
          "Gas cost",
          "Gross rent as a percentage of income",
          "Property taxes",
          "Property value",
          "Usual hours worked per week"
        ),
        selected = "Total person's income"
      ),
      selectInput(
        inputId = "y_var",
        label = "y Variable",
        choices = list(
          "Travel time to work",
          "Total person's income",
          "Water cost",
          "Electricity cost",
          "Gas cost",
          "Gross rent as a percentage of income",
          "Property taxes",
          "Property value"
        ),
        selected = "Travel time to work"
      ),
      radioButtons(
        inputId = "hhl_corr",
        label = "Household Language",
        choices = c("All", "English Only", "Spanish", "Other"),
        selected = "All"
      ),
      radioButtons(
        inputId = "fs_corr",
        label = "Food Security",
        choices = c("All", "Yes", "No"),
        selected = "All"
      ),
      radioButtons(
        inputId = "schl_corr",
        label = "Educational attainment",
        choices = c("All", "High School not Completed", "High School or GED", "College Degree"),
        selected = "All"
      ),
      h2("Select a Sample Size"),
      sliderInput("corr_n", "Set value range", min = 0, max = 500, value = 20),
      actionButton("corr_sample","Get a Sample!")
    ),
    mainPanel(
      plotOutput("corr_plot"),
      conditionalPanel("input.corr_sample > 0",
                       h2("Guess the correlation!"),
                       column(6, numericInput("corr_guess", "", value = 0, min = -1, max = 1)),
                       column(6, actionButton("corr_submit", "Check Your Guess!"))
      )
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  sample_corr <- reactiveValues(corr_data = NULL, corr_truth = NULL)
  
  numeric_vars <- c("Total person's income", "Water cost", "Electricity cost",
                    "Gas cost", "Gross rent as a percentage of income", 
                    "Property taxes", "Property value", "Usual hours worked per week",
                    "Travel time to work")
  
  observeEvent(input$x_var, {
    corr_x <- input$x_var
    corr_y <- input$y_var
    choices <- numeric_vars
    
    if(corr_x %in% choices){
      choices <- choices[choices != corr_x]
    }
    
    new_y <- if(corr_y %in% choices) corr_y else choices[1]
    
    updateSelectInput(session, "y_var", choices = choices, selected = new_y)
  })
  
  observeEvent(input$corr_sample, {
    
    if (input$hhl_corr == "All") {
      hhl_sub <- HHLvals
    } else if (input$hhl_corr == "English Only") {
      hhl_sub <- HHLvals["1"]
    } else if (input$hhl_corr == "Spanish") {
      hhl_sub <- HHLvals["2"]
    } else {
      hhl_sub <- HHLvals[c("0", "3", "4", "5")]
    }
    
    if (input$fs_corr == "All") {
      fs_sub <- FSvals
    } else if (input$fs_corr == "Yes") {
      fs_sub <- FSvals["1"]
    } else {
      fs_sub <- FSvals["2"]
    }
    
    if (input$schl_corr == "All") {
      schl_sub <- SCHLvals
    } else if (input$schl_corr == "High School not Completed") {
      schl_sub <- SCHLvals[c("0","01","02","03","04","05","06","07","08","09","10","11","12","13","14","15")]
    } else if (input$schl_corr == "High School or GED") {
      schl_sub <- SCHLvals[as.character(16:19)]
    } else {
      schl_sub <- SCHLvals[as.character(20:24)]
    }
    
    corr_vars <- c(input$x_var, input$y_var)
    name_map <- c(
      "Total person's income" = "PINCP",
      "Water cost" = "WATP",
      "Electricity cost" = "ELEP",
      "Gas cost" = "GASP",
      "Gross rent as a percentage of income" = "GRPIP",
      "Property taxes" = "TAXAMT",
      "Property value" = "VALP",
      "Usual hours worked per week" = "WKHP",
      "Travel time to work" = "JWMNP"
    )
    
    corr_vars <- c(name_map[input$x_var], name_map[input$y_var])
    subsetted_data <- my_sample %>%
      filter(HHLfac %in% hhl_sub,
             FSfac %in% fs_sub,
             SCHLfac %in% schl_sub)
    
    if ("WKHP" %in% corr_vars) subsetted_data <- subsetted_data %>% filter(WKHP > 0)
    if ("VALP" %in% corr_vars) subsetted_data <- subsetted_data %>% filter(!is.na(VALP))
    if ("TAXAMT" %in% corr_vars) subsetted_data <- subsetted_data %>% filter(!is.na(TAXAMT))
    if ("GRPIP" %in% corr_vars) subsetted_data <- subsetted_data %>% filter(GRPIP > 0)
    if ("GASP" %in% corr_vars) subsetted_data <- subsetted_data %>% filter(GASP > 0)
    if ("ELEP" %in% corr_vars) subsetted_data <- subsetted_data %>% filter(ELEP > 0)
    if ("WATP" %in% corr_vars) subsetted_data <- subsetted_data %>% filter(WATP > 0)
    if ("PINCP" %in% corr_vars) subsetted_data <- subsetted_data %>% filter(AGEP > 18)
    if ("JWMNP" %in% corr_vars) subsetted_data <- subsetted_data %>% filter(!is.na(JWMNP))
    
    
    output$corr_plot <- renderPlot({
      validate(
        need(!is.null(sample_corr$corr_data), 
             "Please select your variables, subset, and click the 'Get a Sample!' button.")
      )
      
      x_col <- name_map[[isolate(input$x_var)]]
      y_col <- name_map[[isolate(input$y_var)]]
      
      ggplot(sample_corr$corr_data, aes_string(x = x_col, y = y_col)) +
        geom_point() +
        labs(
          x = isolate(input$x_var),
          y = isolate(input$y_var),
          title = "Scatter plot of selected variables"
        ) +
        theme_minimal()
    })
    index <- sample(
      1:nrow(subsetted_data),
      size = input$corr_n,
      replace = TRUE,
      prob = subsetted_data$PWGTP / sum(subsetted_data$PWGTP)
    )
    
    sample_corr$corr_data <- subsetted_data[index, ]
    sample_corr$corr_truth <- cor(sample_corr$corr_data |> select(all_of(corr_vars)))[1, 2]
  })
  
  observeEvent(input$corr_submit, {
    close <- abs(input$corr_guess - sample_corr$corr_truth) <= .05
    if(close){
      shinyalert(title = "Nicely done!",
                 paste0("The sample correlation is ", 
                        round(sample_corr$corr_truth, 4), 
                        "."),
                 type = "success")
    } else {
      if(input$corr_guess > sample_corr$corr_truth){
        shinyalert(title = "Try again!", "Try guessing a lower value.")
      } else {
        shinyalert(title = "Try again!", "Try guessing a higher value.")
      }
    }
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

