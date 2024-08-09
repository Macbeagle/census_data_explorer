unzip_file <- function(zip_path, dest_path) {
  tryCatch({ 
    unzip(zip_path, exdir = dest_path)
  }, error = function(e) {
    showModal(modalDialog(
      title = "Error",
      paste("Error unzipping file:", e$message),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}
download_file <- function(url, dest_path, extract_path, file_name) {
  # Ensure the destination directory exists
  if (!dir.exists(dest_path)) {
    dir.create(dest_path, recursive = TRUE)
  }
  full_dest_path <- file.path(dest_path, file_name)
  # Adjust extract_path to not append file_name directly
  tryCatch({
    GET(url, write_disk(full_dest_path, overwrite = TRUE))
    if (grepl(".zip$", full_dest_path)) {
      unzip_file(full_dest_path, extract_path)
      file.remove(full_dest_path)
    }
    files <- list.files("data",extract_path, full.names = TRUE)
    showModal(modalDialog(
      title = "Success",
      "File downloaded and processed successfully.",
      easyClose = TRUE,
      footer = NULL
    ))
  }, error = function(e) {
    #delete the folder if the download fails
    if (dir.exists(extract_path)) {
      unlink(extract_path, recursive = TRUE)
    }
    showModal(modalDialog(
      title = "Error",
      paste("Error downloading file:", e$message),
      easyClose = TRUE,
      footer = NULL
    ))
  })
}
download_census_data <- function(c_year, c_pack, c_geo, c_area, dest_path, extract_path){
  waiter_show(html = spin_3(), color = transparent(.5))
  url <- paste0("https://www.abs.gov.au/census/find-census-data/datapacks/download/", c_year, "_", c_pack, "_", c_geo, "_for_", c_area, "_short-header.zip")
  print(url)
  file_name <- paste0(c_year, "_", c_pack, "_", c_geo, "_for_", c_area, "_short-header.zip")
  download_file(url, dest_path, extract_path, file_name)
  files <- list.files(extract_path, full.names = TRUE) # Get full path
  if (length(files) == 0) {
    unlink(extract_path, recursive = TRUE)
    waiter_hide()
    showModal(modalDialog(
      title = "Failure",
      "File downloaded failed.",
      easyClose = TRUE,
      footer = NULL
    ))
  }
  file.rename(files[1], file.path(extract_path, "Tables"))
  waiter_hide()
}
read_excel_sheets <- function(file) {
  tables <- excel_sheets(file)
  dataframes <- list()
  for (table in tables) {
    dataframes[[table]] <- read_excel(file, sheet = table)
  }
  return(dataframes)
}
setColumns <- function(df) {
  for (col in 1:ncol(df)) {
    # Find the first non-NA value in the column
    firstNonNAIndex <- which(!is.na(df[[col]]))[1]
    # Check if there is a non-NA value
    if (!is.na(firstNonNAIndex)) {
      # Set the column name to the first non-NA value
      colnames(df)[col] <- as.character(df[firstNonNAIndex, col])
    }
  }
  return(df)
}