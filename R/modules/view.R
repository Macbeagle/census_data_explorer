view_ui <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(
        width = 4,  # 30% of the width
        # Add your content here later
        box(title = "Left Column", status = "primary", solidHeader = TRUE, width = 12,
            "Content for the left column"
        )
      ),
      column(
        width = 8,  # 70% of the width
        fluidRow(
          column(
            width = 12,
            box(title = "Header", status = "primary", solidHeader = TRUE, width = 12,
                "Header content"
            )
          )
        ),
        fluidRow(
          column(
            width = 12,
            box(title = "Data Table", status = "primary", solidHeader = TRUE, width = 12,
                dataTableOutput(ns("dataTable"))
            )
          )
        )
      )
    )
  )
}

view_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    output$dataTable <- renderDataTable({
      datatable(data, options = list(pageLength = 5))
    })
  })
}
