metadata_source <- function (files) {
  #read each table in the excel file and create dataframe based on tablename
  geog_def_source <- files[grepl("geog", files)]
  data_def_source <- files[grepl("DataPack", files)]
  geog_def <- read_excel_sheets(geog_def_source)
  data_def <- read_excel_sheets(data_def_source)
  split_data_frames <- function(df_list) {
    split_dfs <- list()
    for (name in names(df_list)) {
      df <- df_list[[name]]
      print(paste("Processing data frame:", name))
      # Ensure df is a data.table
      setDT(df)
      # Split the data frame by the first column (assuming it's the category column)
      split_list <- split(df, df[[1]])
      for (category_name in names(split_list)) {
        cat_df <- split_list[[category_name]]
        # Check if this category already exists in split_dfs
        if (category_name %in% names(split_dfs)) {
          # If so, Combine existing and new data frames, then remove duplicates
          combined_df <- rbind(split_dfs[[category_name]], cat_df)
          # Ensure the combined data frame is unique
          unique_combined_df <- unique(combined_df)
          split_dfs[[category_name]] <- unique_combined_df
        } else {
          #if this is first occurence of category, add it to the list
          split_dfs[[category_name]] <- cat_df
        }
      }
    }
    return(split_dfs)
  }
  split_list_of_dfs <- split_data_frames(geog_def)
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
}