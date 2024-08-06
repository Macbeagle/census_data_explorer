download_ui <- function(id) {
  ns <- NS(id)
  fluidRow(
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
          uiOutput(ns("fileList"))
      )
    )
  )
}

download_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
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
    observeEvent(input$download, {
      output$status <- renderText({
        withProgress(message = 'Downloading data...', value = 0, {
          result <- download_census_data(input$year, input$pack, input$geography, input$area, input$dest_path, input$extract_path, progress = shiny::Progress$new())
          downloadStatus(result)
        })
      })
    })
    observeEvent(input$downloadBtn, {
      req(input$censusYear, input$dataPackType, input$geography, input$area)  # Ensure all inputs are filled in
      # Set up paths for download and extraction
      c_year = input$censusYear
      c_pack = input$dataPackType
      c_geo = input$geography
      c_area = input$area
      directory <- paste("Census", c_year, c_pack, c_geo, c_area, sep = "_")
      # Call your function
      download_census_data(
        c_year,
        c_pack,
        c_geo,
        c_area,
        dest_path <- here("data", directory),
        extract_path <- here("data", directory)
      )
      refresh_file_list(output, ns)
    })
    refresh_file_list(output, ns)
    observe({
      files <- list.files("data", full.names = TRUE)
      lapply(files, function(file) {
        observeEvent(input[[basename(file)]], {
          # showModal(modalDialog(
          #   title = "File Clicked",
          #   paste("You clicked on:", basename(file)),
          #   easyClose = TRUE,
          #   footer = NULL
          # ))
          data_path <- here("data", basename(file))
          print(data_path)
          updateTabsetPanel(, "tabs", selected = "view_tab")
          #metadata_source(data_path)
        })
      })
    })
  })
}