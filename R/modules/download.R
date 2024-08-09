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
    r_year <- reactiveVal(NULL)
    r_pack <- reactiveVal(NULL)
    r_geo <- reactiveVal(NULL)
    r_area <- reactiveVal(NULL)
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
                                      "State/Territory (STE)" = "STE", 
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
              actionLink(ns(file_name), file_name, class = "file-name"),
              div(class = "file-actions",
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
      c_year = input$censusYear
      c_pack = input$dataPackType
      c_geo = input$geography
      c_area = input$area
      r_year(c_year)
      r_pack(c_pack)
      r_geo(c_geo)
      r_area(c_area)
      directory <- paste("Census", c_year, c_pack, c_geo, c_area, sep = "_")
      print(directory)
      if (dir.exists(directory)) {
        showModal(modalDialog(
          title = "Error",
          "The folder already exists.",
          easyClose = TRUE,
          footer = NULL
        ))
        return()
      }
      if (c_geo %in% c("SA1", "MB")) {
        showModal(modalDialog(
          title = "Warning",
          "You are about to download data for SA1 or Mesh Block level. This data make take a significant amount of time to load in the view tab. Do you wish to continue?",
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("okDownload"), "OK")
          )
        ))
      } else {
        # If not SA1 or MB, proceed with download directly
        download_census_data(
          input$censusYear,
          input$dataPackType,
          input$geography,
          input$area,
          dest_path = here("data", directory),
          extract_path = here("data", directory)
        )
      }
    })
    observeEvent(input$okDownload, {
      c_year = r_year()
      c_pack = r_pack()
      c_geo = r_geo()
      c_area = r_area()
      print("test")
      directory <- paste("Census", c_year, c_pack, c_geo, c_area, sep = "_")
      removeModal()
      download_census_data(
        c_year,
        c_pack,
        c_geo,
        c_area,
        dest_path = here("data", directory),
        extract_path = here("data", directory)
      )
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