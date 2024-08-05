download_ui <- function(id) {
  ns <- NS(id)
  tagList(
    box(title = "Search DataPacks", status = "primary", solidHeader = TRUE, width = 8,
        selectInput(ns("censusYear"), "Select Census year", choices = c("2021", "2016", "2011")),
        selectInput(ns("dataPackType"), "Select DataPack type", 
                    choices = c("General Community Profile" = "GCP",
                                "Indigenous Profile" = "IP",
                                "Time Series Profile" = "TSP",
                                "Place of Enumeration Profile" = "PEP",
                                "Working Population Profile" = "WPP")),
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
                                "Other Territories" = "OT")),
        actionButton(ns("downloadBtn"), "Download Data"),
        textOutput(ns("downloadMessage"))
    )
  )
}

download_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    downloadStatus <- reactiveVal("Awaitng download...")
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
      downloadStatus(status_message)
    })
    output$downloadMessage <- renderText({
      downloadStatus()
    })
  })
}





