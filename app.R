invisible(sapply(list.files("R", full.names = TRUE, recursive = TRUE), function(i) {
  source(paste0(i))
}))
ui <- fluidPage(
  tags$style(
    ".file-list-container {
  display: flex;
  flex-direction: column;
  align-items: flex-start;
  width: 100%;
}

.file-item {
  display: flex;
  justify-content: space-between;
  align-items: center;
  width: 100%;
  margin-bottom: 10px;
}

.file-name {
  margin-right: 10px;
  text-decoration: none;
  color: #337ab7;
}

.file-actions {
  display: flex;
  align-items: center;
}

.file-actions .btn-danger {
  margin-left: 10px;
}"
  ),
  autoWaiter(),
  # rest of your UI definition
  # HTML("<a id='title_logo'
  #           alt='Your Logo'
  #           href='https://www.yourwebsite.com'>
  #           <img id='yourlogo' src='your-logo.png'>
  #       </a>"),
  tabsetPanel(id = "tabs", selected = "download_tab",
              # Home tab
              # Download tab
              tabPanel(
                value = "download_tab",
                h4(id = "nav_title", "Download"),
                download_ui(id = "download")
              ),
              # View tab
              tabPanel(
                value = "view_tab",
                h4(id = "nav_title", "View"),
                view_ui(id = "view")
              ),
              tabPanel(
                value = "home_tab", 
                h4(id = "nav_title", "About"),
                home_ui(id = "home")
              ),
              # Map tab
              # tabPanel(
              #   value = "map_tab",
              #   h4(id = "nav_title", "Map"),
              #   map_ui(id = "map")
              # )
  ),
  # Render footer
  hr(),
  box(
    # Footer content here
  )
)
# Define server logic
server <- function(input, output, session) {
  activeData <- reactiveVal(NULL)
  # Pass the main session to modules that need to control the tabs
  home_server("home", session)
  download_server("download", session, activeData)
  view_server("view", session, activeData)
  # map_server("map", session)
}
# Run the application 
shinyApp(ui = ui, server = server)