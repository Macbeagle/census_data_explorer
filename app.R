invisible(sapply(list.files("R", full.names = TRUE, recursive = TRUE), function(i) {
  source(paste0(i))
}))
ui <- fluidPage(
  HTML("<a id='title_logo'
            alt='Your Logo'
            href='https://www.yourwebsite.com'>
            <img id='yourlogo' src='your-logo.png'>
        </a>"),
  tabsetPanel(selected = "home_tab",
              # Home tab
              tabPanel(
                value = "home_tab", 
                h4(id = "nav_title", "Home"),
                home_ui(id = "home")
              ),
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
              # Map tab
              tabPanel(
                value = "map_tab",
                h4(id = "nav_title", "Map"),
                map_ui(id = "map")
              )
  ),
  # Render footer
  hr(),
  box(
    # Footer content here
  )
)
# Define server logic
server <- function(input, output, session) {
  # Placeholder for home tab server logic
  home_server("home")
  
  # Placeholder for download tab server logic
  download_server("download")
  
  # Placeholder for view tab server logic
  view_server("view")
  
  # Placeholder for map tab server logic
  map_server("map")
}
# Run the application 
shinyApp(ui = ui, server = server)