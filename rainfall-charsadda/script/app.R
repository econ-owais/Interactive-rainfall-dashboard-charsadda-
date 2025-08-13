# Load required libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(DT)


# UI
ui <- fluidPage(
  titlePanel("🌧️ Rainfall Dashboard – Charsadda City"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_year", "Select Year:", choices = sort(unique(rainfall_data$Year)), selected = 2010),
      selectInput("selected_month", "Select Month:", choices = month.name, selected = "January"),
      checkboxInput("show_table", "Show Raw Data Table", FALSE)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("📅 Monthly Rainfall Trend", plotlyOutput("monthly_trend")),
        tabPanel("📊 Annual Rainfall Summary", plotlyOutput("annual_summary")),
        tabPanel("🌡️ Rainfall Heatmap", plotlyOutput("heatmap")),
        tabPanel("📈 Monthly Climatology", plotlyOutput("climatology")),
        tabPanel("🗃️ Data Table", DTOutput("data_table"))
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  # Monthly Rainfall Trend for selected year
  output$monthly_trend <- renderPlotly({
    df <- rainfall_data %>%
      filter(Year == input$selected_year) %>%
      mutate(Month = month.name[Month])
    
    p <- ggplot(df, aes(x = Month, y = Rainfall)) +
      geom_line(group = 1, color = "#1f77b4", size = 1.2) +
      geom_point(color = "#ff7f0e", size = 2) +
      labs(title = paste("Monthly Rainfall in", input$selected_year),
           x = "Month", y = "Rainfall (mm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Annual Rainfall Summary
  output$annual_summary <- renderPlotly({
    df <- rainfall_data %>%
      group_by(Year) %>%
      summarise(Total_Rainfall = sum(Rainfall))
    
    p <- ggplot(df, aes(x = Year, y = Total_Rainfall)) +
      geom_bar(stat = "identity", fill = "#2ca02c") +
      labs(title = "Total Annual Rainfall", x = "Year", y = "Rainfall (mm)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  # Rainfall Heatmap
  output$heatmap <- renderPlotly({
    df_long <- rainfall_data %>%
      mutate(Month = month.name[Month])
    
    p <- ggplot(df_long, aes(x = Month, y = factor(Year), fill = Rainfall)) +
      geom_tile(color = "white") +
      scale_fill_viridis_c(option = "C") +
      labs(title = "Rainfall Heatmap", x = "Month", y = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Monthly Climatology
  output$climatology <- renderPlotly({
    df <- rainfall_data %>%
      group_by(Month) %>%
      summarise(Avg_Rainfall = mean(Rainfall)) %>%
      mutate(Month = month.name[Month])
    
    p <- ggplot(df, aes(x = Month, y = Avg_Rainfall)) +
      geom_bar(stat = "identity", fill = "#9467bd") +
      labs(title = "Average Monthly Rainfall (Climatology)", x = "Month", y = "Rainfall (mm)") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p)
  })
  
  # Raw Data Table
  output$data_table <- renderDT({
    if (input$show_table) {
      datatable(rainfall_data, options = list(pageLength = 10))
    }
  })
}

# Run the app
shinyApp(ui = ui, server = server)

