process_data <- function(data, group_var) {
  
  # Identify numeric columns
  numeric_cols <- names(select(data, where(is.numeric)))
  
  data %>% 
    pivot_longer(cols = all_of(numeric_cols)) %>%  # Reshape from wide to long
    drop_na(value) %>%  # Remove missing values
    group_by(.data[[group_var]], name) %>%  # Group by chosen variable and name
    summarise(value = sum(value), .groups = 'drop') %>%  # Sum values
    pivot_wider(names_from = name, values_from = value) %>%  # Reshape back to wide
    rowwise() %>%  # Apply row-wise operation
    mutate(total = sum(across(where(is.numeric)), na.rm = TRUE))  # Compute total
}







# how to call the functions

# if total row is required for the table the function will be call as a follows 
## create_summary_table(data = data, group_var = "group_var", response_var = "response_var", sum = TRUE)
# if not 
## create_summary_table(data = data, group_var = "group_var", response_var = "response_var", sum = FALSE)













create_summary_table3 <- function(data, group_vars, response_var, sum = FALSE) {
  # Initialize an empty list to store tables for each group variable
  tables_list <- list()
  
  for (group_var in group_vars) {
    table <- data %>%
      group_by(.data[[group_var]]) %>%
      count(.data[[response_var]]) %>%
      drop_na(.data[[response_var]]) %>%
      pivot_wider(names_from = .data[[response_var]], values_from = n, values_fill = list(n = 0)) %>%
      mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
      mutate(across(-total, ~ round(. / total * 100, 2), .names = "{.col}_percent")) %>%
      rename(Background_characteristic = !!group_var) %>%
      select(Background_characteristic, ends_with("_percent"), total)
    
    # Store table in list
    tables_list[[group_var]] <- table
  }
  
  # Combine all tables into one
  final_table <- bind_rows(tables_list)
  
  # Apply total row if sum = TRUE
  if (sum) {
    # Calculate the total row based on the whole dataset (not just per group)
    total_counts <- data %>%
      count(.data[[response_var]]) %>%
      drop_na(.data[[response_var]]) %>%
      pivot_wider(names_from = .data[[response_var]], values_from = n, values_fill = list(n = 0)) %>%
      mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
      mutate(across(-total, ~ round(. / total * 100, 2), .names = "{.col}_percent")) %>%
      select(ends_with("_percent"), total)
    
    # Create the total row as a summary of all response categories
    total_row <- tibble(
      Background_characteristic = "Total",
      total = total_counts$total
    )
    
    # Add the percentages for each response category
    percent_columns <- names(total_counts)[grepl("_percent$", names(total_counts))]
    
    for (col_name in percent_columns) {
      total_row[[col_name]] <- total_counts[[col_name]]
    }
    
    # Bind total row to the final table
    final_table <- bind_rows(final_table, total_row)
  }
  
  return(final_table)
}


