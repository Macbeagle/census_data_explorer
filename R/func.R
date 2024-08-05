unzip_file <- function(zip_path, dest_path){
  unzip(zip_path, exdir = dest_path)
}

download_file <- function(url, dest_path, extract_path, file_name){
  # Ensure the destination directory exists
  if (!dir.exists(dest_path)) {
    dir.create(dest_path, recursive = TRUE)
  }
  
  full_dest_path <- file.path(dest_path, file_name)
  # Adjust extract_path to not append file_name directly
  GET(url, write_disk(full_dest_path, overwrite = TRUE))
  
  if (grepl(".zip$", full_dest_path)){
    unzip_file(full_dest_path, extract_path)
    file.remove(full_dest_path)
  }
}

download_census_data <- function(c_year, c_pack, c_geo, c_area, dest_path, extract_path){
  url <- paste0("https://www.abs.gov.au/census/find-census-data/datapacks/download/", c_year, "_", c_pack, "_", c_geo, "_for_", c_area, "_short-header.zip")
  file_name <- paste0(c_year, "_", c_pack, "_", c_geo, "_for_", c_area, "_short-header.zip")
  download_file(url, dest_path, extract_path, file_name)
  files <- list.files(extract_path, full.names = TRUE) # Get full paths
  tryCatch({
    download.file(url, destfile = file.path(dest_path, file_name), mode = "wb")
    unzip(file.path(dest_path, file_name), exdir = extract_path)
    return("Download successful!")
  }, error = function(e) {
    return(paste("Failed to download:", e$message))
  })
}

retrieve_table <- function(search_value, table_index, datafiles) {
  # Search for the index for the dynamic value
  index <- grep(search_value, table_index$V1, fixed = TRUE, ignore.case = FALSE)
  file_paths <- file.path(datafiles, table_index$V1[index])
  
  # Read and combine all CSVs that match
  combined_df <- map_dfr(file_paths, read.csv)
  
  # Return the combined dataframe
  return(combined_df)
}
# combine these two
# search_long <- function(search_value, table_desc) {
#   # Search for the index for the dynamic value in the second column
#   index <- grep(search_value, table_desc[, 2], fixed = TRUE, ignore.case = FALSE)
#   # Return the 2nd and 3rd columns of matching rows
#   result <- table_desc[index, c(2, 3)]
#   return(result)
# }
# 
# search_short <- function(search_value, table_desc) {
#   # Replace spaces with underscores in the search value
#   search_value <- gsub(" ", "_", search_value)
# 
#   # Search for the index for the dynamic value
#   index <- grep(search_value, table_desc[, 3], fixed = TRUE, ignore.case = FALSE)
# 
#   # Return the 2nd and 3rd columns of matching rows
#   result <- table_desc[index, c(2, 3)]
#   return(result)
# }

# search <- function(search_value, table_desc) {
#   # Replace spaces with underscores in the search value for the short search
#   search_value_short <- gsub(" ", "_", search_value)
#   
#   # Search for the index for the dynamic value in both columns
#   index_long <- grep(search_value, table_desc[, 2], ignore.case = TRUE)
#   index_short <- grep(search_value_short, table_desc[, 3], ignore.case = TRUE)
#   
#   # Combine the indices and get unique values
#   index <- unique(c(index_long, index_short))
#   
#   # Return the 2nd and 3rd columns of matching rows
#   result <- table_desc[index, c(2, 3)]
#   return(result)
# }