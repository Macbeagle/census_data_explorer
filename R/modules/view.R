view_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    useWaiter(),
    fluidRow(
      column(
        width = 12,  # 100% of the width
        fluidRow(
          column(
            width = 12,
            textOutput(ns("selectedTableName")),
            tabsetPanel(
              tabPanel("Table Index",
                       dataTableOutput(ns("tableIndex"))
                       ),
              tabPanel("Data Table", 
                       dataTableOutput(ns("dataTable")),
                       downloadButton(ns("downloadCombinedData"), "Download Data CSV")
                       ),
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
    table_selected <- reactiveVal()
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
      waiter_show( # show the waiter
        html = spin_3(), 
        color = transparent(.5)
      )
      search <- as.character(data[selected_row, 1])
      table_selected(search)
      files <- list.files(path = here("data", file, "Tables"), pattern = paste0(search, ".*\\.csv$"), full.names = TRUE)
      data_list <- map(files, read.csv)
      # Use the first column as the key for joining
      key_column <- names(data_list[[1]])[1]
      # Combine all data frames by performing a full join on the first column
      combined_data <- reduce(data_list, full_join, by = key_column)
      combined_data <- as.data.frame(t(combined_data))
      #save as csv
      write.csv(combined_data, here("data",file, "combined_data.csv"), row.names = FALSE)
      table_data(combined_data)
      waiter_hide()
      showModal(modalDialog(
        title = paste("Table Loaded:", table_selected()),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    # Render the main data table
    output$dataTable <- renderDataTable({
      combined_data <- table_data()
      datatable(combined_data, options = list(pageLength = 30))
    })
    # Render text outputs for other tab
    output$columnIndex <- renderDataTable({
      file <- activeData()
      req(file)
      selected_table <- table_selected()
      req(selected_table)
      data <- read.csv(here("data", file, "Metadata", "data_def_columns.csv"))
      filtered_data <- data[data[[4]] == selected_table, ]
      datatable(filtered_data, options = list(pageLength = 20), selection = 'single')
    })
    # Render the selected table name
    output$selectedTableName <- renderText({
      paste("Selected Table:", table_selected())
    })
    output$downloadCombinedData <- downloadHandler(
      filename = function() {
        file <- activeData()
        table <- table_selected()
        paste(file,"_", table,"_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        combined_data_path <- here("data", "combined_data.csv")
        tryCatch({
          if (file.exists(combined_data_path)) {
            file.copy(combined_data_path, file)
          } else {
            stop("Combined data file not found at: ", combined_data_path)
          }
        }, error = function(e) {
          # Log the error
          message("Error in downloadHandler: ", e$message)
          # Write the error message to the file so the user can see what went wrong
          writeLines(paste("Error:", e$message), file)
        })
      },
      contentType = "text/csv"
    )
  })
}