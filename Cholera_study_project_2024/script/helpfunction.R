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




# Generic function to create a summary table
create_summary_table <- function(data, group_var, response_var, adorn_totals = FALSE) {
  table <- data %>%
    group_by(.data[[group_var]]) %>%
    count(.data[[response_var]]) %>%
    drop_na(.data[[response_var]]) %>%
    pivot_wider(names_from = .data[[response_var]], values_from = n, values_fill = list(n = 0)) %>%
    mutate(total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
    mutate(across(-total, ~ round(. / total * 100, 2), .names = "{.col}_percent")) %>%  # Convert only counts to percentages
    rename(Background_characteristic = !!group_var) %>%
    select(Background_characteristic, ends_with("_percent"), total)  # Exclude total_percent
  
  # Apply adorn_totals if TRUE
  if (adorn_totals) {
    table <- table %>% adorn_totals("row")
  }
  
  return(table)
}