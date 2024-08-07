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
    table_data <- reactiveVal()  
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
      req(selected_row)
      file <- activeData()
      data <- tableIndexData()
      search <- as.character(data[selected_row, 1])
      files <- list.files(path = here("data", file, "Tables"), pattern = paste0(search, ".*\\.csv$"), full.names = TRUE)
      data_list <- map(files, read.csv)
      
      # Use the first column as the key for joining
      key_column <- names(data_list[[1]])[1]
      
      # Combine all data frames by performing a full join on the first column
      combined_data <- reduce(data_list, full_join, by = key_column)
      if (nrow(combined_data) < 30) {
        combined_data <- as.data.frame(t(combined_data))
      }
      
      # combined_data <- map_dfr(files, read.csv) %>%
      #   bind_rows()
      
      
      
      #find all csvs in the Tables folder than have the search value in their name
      #combine
      
      table_data(combined_data)
    })
    
    # Render the main data table
    output$dataTable <- renderDataTable({
      combined_data <- table_data()
      
      datatable(combined_data, options = list(pageLength = 5))
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