retrieve_table <- function(search_value, activeData) {
  datafiles <- here("data", activeData, "Tables")
  search_2 <- search_value
  file_names <- list.files(datafiles)
  table_index <- data.table(V1 = file_names)
  # Get a list of all files in the Tables directory
  table_index <- tibble(V1 = list.files(datafiles))
  # Check the lengths of the vectors
  print(paste("Length of table_index$V1:", length(table_index$V1)))
  print(paste("Length of search_value:", length(search_value)))
  
  # Search for the search_value in the table_index
  print(table_index, n = Inf)
  print(search_value)
  #search for the search_value in the table_index
  index <- str_detect(table_index$V1, search_2)
  
  #index <- grep(search_value, table_index$V1, fixed = TRUE)
  print("Index:")
  print(index)
  file_paths <- file.path(datafiles, table_index$V1[index])
  print("file paths:")
  print(file_paths)
  combined_df <- map_dfr(file_paths, read.csv)
  return(combined_df)
}
file <- "Census_2021_IP_IARE_TAS"
data <- read.csv(here("data", file, "Metadata", "data_def_tables.csv"))
search <- as.character(data[4, 1])
print(search)
combined_data <- retrieve_table(search, file)
print(combined_data)
