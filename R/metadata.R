metadata_source <- function (files) {
  #read each table in the excel file and create dataframe based on tablename
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
  output_path <- here("data", directory, "Metadata")
  write.csv(data_def[1],here(output_path,"data_def_tables.csv"), row.names = FALSE)
  write.csv(data_def[2],here(output_path,"data_def_columns.csv"), row.names = FALSE)
}