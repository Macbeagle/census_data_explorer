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
              tabPanel("Tab 6", textOutput(ns("tab6")))
            )
          )
        )
      )
    )
  )
}
view_server <- function(id, data, table_index_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    output$tableIndex <- renderDataTable({
      datatable(table_index_data, options = list(pageLength = 5), selection = 'single')
    })
    observeEvent(input$tableIndex_rows_selected, {
      selected_row <- input$tableIndex_rows_selected
      if (length(selected_row)) {
        selected_value <- table_index_data[selected_row, 1]
        handle_table_selection(selected_value)
      }
    })
    output$dataTable <- renderDataTable({
      datatable(data, options = list(pageLength = 5))
    })
    output$tab3 <- renderText({
      "Content for Tab 3"
    })
    output$tab4 <- renderText({
      "Content for Tab 4"
    })
    output$tab5 <- renderText({
      "Content for Tab 5"
    })
    output$tab6 <- renderText({
      "Content for Tab 6"
    })
  })
}
handle_table_selection <- function(selected_value) {
  # Placeholder function to handle the selected value
  print(paste("Selected table:", selected_value))
}