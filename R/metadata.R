metadata_source <- function(file) {
  metadata_path <- here("data", file, "Metadata")
  data_def_source <- list.files(metadata_path, full.names = TRUE, pattern = "DataPack")
  data_def <- read_excel_sheets(data_def_source)
  
  process_table <- function(table) {
    table <- table[!is.na(table[, 2]), ]
    table <- setColumns(table)
    table[-1, ]
  }
  
  data_def <- lapply(data_def, process_table)
  data_def_tables <- as.data.frame(data_def[[1]])
  data_def_columns <- as.data.frame(data_def[[2]])
  
  values_match <- data_def_tables[, 1]
  data_def_columns <- data_def_columns[data_def_columns[, 5] %in% values_match, -4]
  
  output_path <- here("data", file, "Metadata")
  write.csv(data_def_tables, file.path(output_path, "data_def_tables.csv"), row.names = FALSE)
  write.csv(data_def_columns, file.path(output_path, "data_def_columns.csv"), row.names = FALSE)
}