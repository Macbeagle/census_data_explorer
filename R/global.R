pkgLoad <- function( packages = "required" ) {
  if( length( packages ) == 1L && packages == "required" ) {
    packages <- c( "readxl", "tidyverse", "openssl", "dplyr", "data.table",
                   "here", "httr","purrr","reshape2", "repr","plotly","scales",
                   "rsample", "recipes", "caret", "echarts4r", "shiny",
                   "shinydashboard", "shinybrowser", "shinyWidgets"
    )
  }
  packagecheck <- match( packages, utils::installed.packages()[,1] )
  packagestoinstall <- packages[ is.na( packagecheck ) ]
  if( length( packagestoinstall ) > 0L ) {
    utils::install.packages( packagestoinstall,
                             repos = "http://cran.csiro.au"
    )
  } else {
    print( "All requested packages already installed" )
  }
  for( package in packages ) {
    suppressPackageStartupMessages(
      library( package, character.only = TRUE, quietly = TRUE )
    )
  }
}
pkgLoad()
base_path <- here()
dest_path <- here("data")
extract_path <- here("data")
output_path <- here("data")
#load data
# invisible(sapply(
#   list.files('R/pubr-source', full.names = TRUE, recursive = TRUE), function(i) {
#     source(paste0(i))
# }))