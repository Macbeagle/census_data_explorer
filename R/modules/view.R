view_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 12,  # 100% of the width
        fluidRow(
          column(
            width = 12,
            tabsetPanel(
              tabPanel("Table Index", dataTableOutput(ns("tableIndex"))),
              tabPanel("Data Table", dataTableOutput(ns("dataTable"))),
              tabPanel("Tab 3", textOutput(ns("tab3"))),
              tabPanel("Tab 4", textOutput(ns("tab4"))),
              tabPanel("Tab 5", textOutput(ns("tab5"))),
              tabPanel("Column Information", dataTableOutput(ns("columnIndex")))
            )
          )
        )
      )
    )
  )
}

view_server <- function(id, parentSession, activeData) {
  moduleServer(id, function(input, output, session) {
    tableIndexData <- reactiveVal()
    ns <- session$ns
    # Render the table index data
    output$tableIndex <- renderDataTable({
      file <- activeData()
      req(file)
      data <- read.csv(here("data", file, "Metadata", "data_def_tables.csv"))
      tableIndexData(data)  # Store the data in the reactive value
      datatable(data, options = list(pageLength = 20), selection = 'single')
    })
    
    # Observe the selected row in the table index
    observeEvent(input$tableIndex_rows_selected, {
      selected_row <- input$tableIndex_rows_selected
      if (length(selected_row)) {
        data <- tableIndexData()  # Retrieve the data from the reactive value
        selected_value <- data[selected_row, 1]
        #combined_data <- retrieve_table(selected_value, data, here("data", "") )
       
        
        handle_table_selection(selected_value)
      }
    })
    
    # Render the main data table
    output$dataTable <- renderDataTable({
      datatable(data, options = list(pageLength = 5))
    })
    
    # Render text outputs for other tabs
    output$tab3 <- renderText({ "Content for Tab 3" })
    output$tab4 <- renderText({ "Content for Tab 4" })
    output$tab5 <- renderText({ "Content for Tab 5" })
    
    output$columnIndex <- renderDataTable({
      file <- activeData()
      req(file)
      data <- read.csv(here("data", file, "Metadata", "data_def_columns.csv"))
      datatable(data, options = list(pageLength = 20), selection = 'single')
    })
  })
}

handle_table_selection <- function(selected_value) {
  # Placeholder function to handle the selected value
  print(selected_value)
}