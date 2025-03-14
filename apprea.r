library(shiny)
library(tidyverse)

graduate_survey <- read.csv("C:/Users/reati/Downloads/graduate_survey.csv")
View(graduate_survey)


data = graduate_survey


ui <- fluidPage(
  titlePanel("Eduvos Graduate Analysis Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("campus", "Select Campus:", 
                  choices = unique(data$Campus)),
      checkboxGroupInput("fields", "Study Fields:",
                         choices = unique(data$StudyField),
                         selected = "IT")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Programming Languages", plotOutput("progPlot")),
        tabPanel("Employment", plotOutput("empPlot")),
        tabPanel("AI Tools", plotOutput("aiPlot"))
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    data %>%
      filter(Campus == input$campus,
             StudyField %in% input$fields)
  })
  
  output$progPlot <- renderPlot({
    filtered_data() %>%
      separate_rows(ProgLang, sep = ";") %>%
      count(ProgLang) %>%
      ggplot(aes(x = reorder(ProgLang, n), y = n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(title = "Programming Language Popularity")
  })
  
  output$empPlot <- renderPlot({
    filtered_data() %>%
      ggplot(aes(x = Employment, fill = StudyField)) +
      geom_bar(position = "dodge") +
      labs(title = "Employment Status")
  })
  
  output$aiPlot <- renderPlot({
    filtered_data() %>%
      separate_rows(AITool, sep = ";") %>%
      count(AITool) %>%
      ggplot(aes(x = reorder(AITool, n), y = n)) +  # Corrected line
      geom_col(fill = "darkorange") +
      coord_flip() +
      labs(title = "AI Tool Usage")
  })
}

shinyApp(ui, server)