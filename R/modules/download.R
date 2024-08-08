download_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
    autoWaiter(),
    column(
      width = 5,
      tagList(
        box(title = "Search DataPacks", status = "primary", solidHeader = TRUE, width = 8,
            selectInput(ns("censusYear"), "Select Census year", choices = c("2021", "2016", "2011")),
            selectInput(ns("dataPackType"), "Select DataPack type", 
                        choices = c("General Community Profile (GCP)" = "GCP",
                                    "Indigenous Profile (IP)" = "IP",
                                    "Time Series Profile (TSP)" = "TSP",
                                    "Place of Enumeration Profile (PEP)" = "PEP",
                                    "Working Population Profile (WPP)" = "WPP")),
            selectInput(ns("geography"), "Select geography", choices = NULL),  # Initialized with NULL, will be updated reactively
            selectInput(ns("area"), "Select area", 
                        choices = c("Australia" = "AUS",
                                    "New South Wales" = "NSW",
                                    "Victoria" = "VIC",
                                    "Queensland" = "QLD",
                                    "South Australia" = "SA",
                                    "Western Australia" = "WA",
                                    "Tasmania" = "TAS",
                                    "Northern Territory" = "NT",
                                    "Australian Capital Territory" = "ACT",
                                    "Other Territories (OT)" = "OT")),
            actionButton(ns("downloadBtn"), "Download Data"),
            textOutput(ns("downloadMessage"))
        )
      ),
      style = "border-right: 1px solid #ddd;"
    ),
    column(
      width = 5,
      box(title = "Data Files", status = "primary", solidHeader = TRUE, width = 12,
          div(
            class = "file-list-container",
            uiOutput(ns("fileList"))
          )
      )
    )
  )
}
download_server <- function(id, parentSession, activeData) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    files_reactive <- reactiveVal(list.files("data", full.names = TRUE))
    # Dynamic update for geography options based on selected DataPack type
    observe({
      if (input$dataPackType == "TSP") {
        # Update choices for TSP
        updateSelectInput(session, ("geography"),
                          choices = c("All geographies" = "all",
                                      "Australia" = "AUS",
                                      "State/Territory (STE)" = "STE", 
                                      "Statistical Area 4 (SA4)" = "SA4", 
                                      "Statistical Area 3 (SA3)" = "SA3",
                                      "Statistical Area 2 (SA2)" = "SA2",
                                      "Greater Capital City Statistical Areas (GCCSA)" = "GCCSA",
                                      "Local Government Areas (LGA)" = "LGA"))
      } else if (input$dataPackType == "WPP") {
        updateSelectInput(session, ("geography"),
                          choices = c("All geographies" = "all",
                                      "Australia" = "AUS",
                                      "State/Territory (STE)" = "STE", 
                                      "Statistical Area 4 (SA4)" = "SA4", 
                                      "Statistical Area 3 (SA3)" = "SA3",
                                      "Statistical Area 2 (SA2)" = "SA2",
                                      "Greater Capital City Statistical Areas (GCCSA)" = "GCCSA",
                                      "Local Government Areas (LGA)" = "LGA"))
      } else if (input$dataPackType == "IP") {
        updateSelectInput(session, ("geography"),
                          choices = c("All geographies" = "all",
                                      "Australia" = "AUS",
                                      "State/Territory (STE)" = "STE", 
                                      "Statistical Area 4 (SA4)" = "SA4", 
                                      "Statistical Area 3 (SA3)" = "SA3",
                                      "Statistical Area 2 (SA2)" = "SA2",
                                      "Greater Capital City Statistical Areas (GCCSA)" = "GCCSA",
                                      "Indigenous Regions (IREG)" = " IREG",
                                      "Indigenous Areas (IARE)" = "IARE",
                                      "Indigenous Locations (ILOC)" = "ILOC",
                                      "Local Government Areas (LGA)" = "LGA",
                                      "Suburbs and Localities (SAL)" = "SAL",
                                      "Remoteness Area (RA)" = "RA"))
      } else {
        # Full list of choices for other profiles
        updateSelectInput(session, ("geography"),
                          choices = c("All geographies" = "all",
                                      "Australia" = "AUS",
                                      "State/Territory (ST)" = "ST", 
                                      "Statistical Area 4 (SA4)" = "SA4", 
                                      "Statistical Area 3 (SA3)" = "SA3", 
                                      "Statistical Area 2 (SA2)" = "SA2", 
                                      "Statistical Area 1 (SA1)" = "SA1", 
                                      "Greater Capital City Statistical Areas (GCCSA)" = "GCCSA", 
                                      "Local Government Areas (LGA)" = "LGA", 
                                      "Suburbs and Localities (SAL)" = "SAL", 
                                      "Postal Areas (POA)" = "POA", 
                                      "Commonwealth Electoral Divisions (CED)" = "CED", 
                                      "State Electoral Divisions (SED)" = "SED", 
                                      "Section of State (SOS)" = "SOS", 
                                      "Section of State Ranges (SOSR)" = "SOSR", 
                                      "Urban Centres and Localities (UCL)" = "UCL", 
                                      "Significant Urban Areas (SUA)" = "SUA", 
                                      "Remoteness Area (RA)" = "RA"))
      }
    })
    observe({
      invalidateLater(1000, session)
      files_reactive(list.files("data", full.names = TRUE))
    })
    output$fileList <- renderUI({
      files <- files_reactive()
      if (length(files) == 0) {
        return("No files found in the data folder.")
      }
      tagList(
        lapply(files, function(file) {
          file_name <- basename(file)
          div(class = "file-item",
              div(class = "file-name", file_name),
              div(class = "file-actions",
                  actionLink(ns(file_name), ""),
                  actionButton(ns(paste0("delete_", file_name)), "Delete", class = "btn-danger")
              )
          )
        })
      )
    })
    observeEvent(input$downloadBtn, {
      req(input$censusYear, input$dataPackType, input$geography, input$area)  # Ensure all inputs are filled in
      # Set up paths for download and extraction
      # Call your function
      waiter_show( # show the waiter
        html = spin_3(), 
        color = transparent(.5)
      )
      c_year = input$censusYear
      c_pack = input$dataPackType
      c_geo = input$geography
      c_area = input$area
      directory <- paste("Census", c_year, c_pack, c_geo, c_area, sep = "_")
      if (dir.exists(directory)) {
        showModal(modalDialog(
          title = "Error",
          "The folder already exists.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      download_census_data(
        c_year,
        c_pack,
        c_geo,
        c_area,
        dest_path <- here("data", directory),
        extract_path <- here("data", directory)
      )
      
      #check if empty and if so, delete and return error
      files <- list.files("data", full.names = TRUE)
      if (length(files) == 0) {
        unlink(extract_path, recursive = TRUE)
        waiter_hide()
        output$downloadMessage <- renderText("No files found in the data folder.")
        return()
      }
      waiter_hide()
    })
    observe({
      files <- files_reactive()
      lapply(files, function(file) {
        observeEvent(input[[basename(file)]], {
          waiter_show( # show the waiter
            html = spin_3(), 
            color = transparent(.5)
          )
          data_name <- basename(file)
          activeData(data_name)
          metadata_source(data_name)
          waiter_hide()
          updateTabsetPanel(parentSession, inputId = "tabs", selected = "view_tab")
        })
        observeEvent(input[[paste0("delete_", basename(file))]], {
          waiter_show(
            html = spin_3(),
            color = transparent(.5)
          )
          data_name <- basename(file)
          
          dir_path <- here("data", data_name)
          if (dir.exists(dir_path)) {
            unlink(dir_path, recursive = TRUE)
            files_reactive(list.files("data", full.names = TRUE))
          }
          waiter_hide()
        })
      })
    })
  })
  view_server("view", parentSession, activeData)
}