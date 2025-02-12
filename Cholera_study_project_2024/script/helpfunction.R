process_data <- function(data, group_var) {
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
