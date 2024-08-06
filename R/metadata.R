metadata_source <- function (file) {
  #read each table in the excel file and create dataframe based on tablename
  metadata_path <- here("data", file, "Metadata")
  files <- list.files(metadata_path, full.names = TRUE)
  data_def_source <- files[grepl("DataPack", files)]
  data_def <- read_excel_sheets(data_def_source)
  #process data definitions 
  data_def_process <- function(data_def) {
    #process table 1
    data_def[[1]] <- data_def[[1]][!is.na(data_def[[1]][,2]),]
    data_def[[1]] <- setColumns(data_def[[1]])
    data_def[[1]] <- data_def[[1]][-1,]
    #process table 2
    data_def[[2]] <- data_def[[2]][!is.na(data_def[[2]][,2]),]
    data_def[[2]] <- setColumns(data_def[[2]])
    data_def[[2]] <- data_def[[2]][-1,]
    return(data_def)
  }
  data_def <- data_def_process(data_def)
  data_def_tables <- as.data.frame(data_def[[1]])
  data_def_columns <- as.data.frame(data_def[[2]])
  print(data_def_tables[,1])
  print(data_def_columns[,5])
  values_match <- data_def_tables[,1]
  print(values_match)
  data_def_columns <- data_def_columns[data_def_columns[, 5] %in% values_match,]
  data_def_columns <- data_def_columns[, -4]
  output_path <- here("data", file, "Metadata")
  write.csv(data_def_tables,here(output_path,"data_def_tables.csv"), row.names = FALSE)
  write.csv(data_def_columns,here(output_path,"data_def_columns.csv"), row.names = FALSE)
}

