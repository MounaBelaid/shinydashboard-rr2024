library(shiny)
library(shinydashboard)
library(ggplot2)
library(palmerpenguins)
library(DT)
library(dplyr)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Penguin Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Summary", tabName = "summary", icon = icon("table"))
    ),
    sidebarMenu(
      checkboxGroupInput("species_filter", "Filter by Species:",
        choices = unique(penguins$species), selected = unique(penguins$species)
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "dashboard",
        fluidRow(
          box(
            title = "Penguin Distribution",
            plotOutput("penguin_plot")
          )
        )
      ),
      tabItem(
        tabName = "summary",
        fluidRow(
          box(
            title = "Penguin Summary",
            dataTableOutput("penguin_table")
          )
        )
      )
    )
  )
)


# Server
server <- function(input, output) {
  # Filter penguins data based on user selection
  filtered_data <- reactive({
    req(input$species_filter)
    if (is.null(input$species_filter)) {
      return(penguins)
    } else {
      filter(penguins, species %in% input$species_filter)
    }
  })

  # Render penguin distribution plot
  output$penguin_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = species, fill = island)) +
      geom_bar(position = "dodge") +
      labs(title = "Penguin Distribution by Species and Island") +
      theme_bw()
  })

  # Render penguin summary table based on filter
  output$penguin_table <- renderDataTable({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

# Run the application
shinyApp(ui, server)
