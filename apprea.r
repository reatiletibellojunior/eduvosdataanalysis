library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Load and prepare data
graduate_survey <- read.csv("C:/Users/reati/Downloads/graduate_survey.csv")
data <- graduate_survey %>%
  mutate(
    Campus = ifelse(is.na(Campus) | Campus == "", "Unknown", Campus),
    StudyField = ifelse(is.na(StudyField) | StudyField == "", "Unknown", StudyField),
    Employment = ifelse(is.na(Employment) | Employment == "", "Unknown", Employment),
    Role = ifelse(is.na(Role) | Role == "", "Unknown", Role),
    Campus = factor(Campus),
    StudyField = factor(StudyField),
    Employment = factor(Employment),
    Role = factor(Role)
  )

# Function to process multi-select columns
process_column <- function(data, column_name) {
  data %>%
    select(Campus, StudyField, all_of(column_name)) %>%
    separate_rows(!!sym(column_name), sep = ";") %>%
    filter(!!sym(column_name) != "") %>%
    mutate(!!sym(column_name) := trimws(!!sym(column_name)))
}

# UI
ui <- navbarPage("Eduvos Graduate Analysis Dashboard",
                 tabPanel("Programming Languages",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("campus_pl", "Campus:", choices = c("All", levels(data$Campus))),
                              selectInput("field_pl", "Study Field:", choices = c("All", levels(data$StudyField)))
                            ),
                            mainPanel(plotOutput("plot_pl"))
                          )),
                 
                 tabPanel("Databases",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("campus_db", "Campus:", choices = c("All", levels(data$Campus))),
                              selectInput("field_db", "Study Field:", choices = c("All", levels(data$StudyField)))
                            ),
                            mainPanel(plotOutput("plot_db"))
                          )),
                 
                 tabPanel("Web Frameworks",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("campus_wf", "Campus:", choices = c("All", levels(data$Campus))),
                              selectInput("field_wf", "Study Field:", choices = c("All", levels(data$StudyField)))
                            ),
                            mainPanel(plotOutput("plot_wf"))
                          )),
                 
                 tabPanel("Cloud Computing",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("campus_cc", "Campus:", choices = c("All", levels(data$Campus))),
                              selectInput("field_cc", "Study Field:", choices = c("All", levels(data$StudyField)))
                            ),
                            mainPanel(plotOutput("plot_cc"))
                          )),
                 
                 tabPanel("AI Tools",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("campus_ai", "Campus:", choices = c("All", levels(data$Campus))),
                              selectInput("field_ai", "Study Field:", choices = c("All", levels(data$StudyField)))
                            ),
                            mainPanel(plotOutput("plot_ai"))
                          )),
                 
                 tabPanel("Employability",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("campus_emp", "Campus:", choices = c("All", levels(data$Campus))),
                              selectInput("field_emp", "Study Field:", choices = c("All", levels(data$StudyField)))
                            ),
                            mainPanel(
                              plotOutput("plot_employment"),
                              plotOutput("plot_roles")
                            )
                          ))
)

# Server
server <- function(input, output) {
  
  filter_data <- function(data, campus, field) {
    filtered <- data
    if(campus != "All") filtered <- filtered %>% filter(Campus == campus)
    if(field != "All") filtered <- filtered %>% filter(StudyField == field)
    filtered
  }
  
  # Programming Languages
  output$plot_pl <- renderPlot({
    pl_data <- process_column(data, "ProgLang") %>%
      filter_data(input$campus_pl, input$field_pl)
    
    validate(need(nrow(pl_data) > 0, "No data available for selected filters"))
    
    pl_data %>%
      count(ProgLang, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      ggplot(aes(x = reorder(ProgLang, n), y = n)) +
      geom_col(fill = "#1f77b4") +
      coord_flip() +
      labs(title = "Top Programming Languages", x = "", y = "Count") +
      theme_minimal()
  })
  
  # Databases
  output$plot_db <- renderPlot({
    db_data <- process_column(data, "Databases") %>%
      filter_data(input$campus_db, input$field_db)
    
    validate(need(nrow(db_data) > 0, "No data available for selected filters"))
    
    db_data %>%
      count(Databases, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      ggplot(aes(x = reorder(Databases, n), y = n)) +
      geom_col(fill = "#2ca02c") +
      coord_flip() +
      labs(title = "Top Databases", x = "", y = "Count") +
      theme_minimal()
  })
  
  # Web Frameworks
  output$plot_wf <- renderPlot({
    wf_data <- process_column(data, "WebFramework") %>%
      filter_data(input$campus_wf, input$field_wf)
    
    validate(need(nrow(wf_data) > 0, "No data available for selected filters"))
    
    wf_data %>%
      count(WebFramework, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      ggplot(aes(x = reorder(WebFramework, n), y = n)) +
      geom_col(fill = "#9467bd") +
      coord_flip() +
      labs(title = "Top Web Frameworks", x = "", y = "Count") +
      theme_minimal()
  })
  
  # Cloud Computing
  output$plot_cc <- renderPlot({
    cc_data <- process_column(data, "Platform") %>%
      filter_data(input$campus_cc, input$field_cc)
    
    validate(need(nrow(cc_data) > 0, "No data available for selected filters"))
    
    cc_data %>%
      count(Platform, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      ggplot(aes(x = reorder(Platform, n), y = n)) +
      geom_col(fill = "#ff7f0e") +
      coord_flip() +
      labs(title = "Top Cloud Platforms", x = "", y = "Count") +
      theme_minimal()
  })
  
  # AI Tools
  output$plot_ai <- renderPlot({
    ai_data <- process_column(data, "AITool") %>%
      filter_data(input$campus_ai, input$field_ai)
    
    validate(need(nrow(ai_data) > 0, "No data available for selected filters"))
    
    ai_data %>%
      count(AITool, sort = TRUE) %>%
      slice_max(n, n = 10) %>%
      ggplot(aes(x = reorder(AITool, n), y = n)) +
      geom_col(fill = "#d62728") +
      coord_flip() +
      labs(title = "Top AI Tools", x = "", y = "Count") +
      theme_minimal()
  })
  
  # Employment Status
  output$plot_employment <- renderPlot({
    emp_data <- data %>%
      filter_data(input$campus_emp, input$field_emp)
    
    validate(need(nrow(emp_data) > 0, "No data available for selected filters"))
    
    emp_data %>%
      count(Employment) %>%
      ggplot(aes(x = reorder(Employment, n), y = n)) +
      geom_col(fill = "#17becf") +
      coord_flip() +
      labs(title = "Employment Status Distribution", x = "", y = "Count") +
      theme_minimal()
  })
  
  # Job Roles
  output$plot_roles <- renderPlot({
    roles_data <- process_column(data, "Role") %>%
      filter_data(input$campus_emp, input$field_emp)
    
    validate(need(nrow(roles_data) > 0, "No data available for selected filters"))
    
    roles_data %>%
      count(Role) %>%
      slice_max(n, n = 10) %>%
      ggplot(aes(x = reorder(Role, n), y = n)) +
      geom_col(fill = "#bcbd22") +
      coord_flip() +
      labs(title = "Top 10 Job Roles", x = "", y = "Count") +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)



