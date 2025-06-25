# Set a seed for reproducibility
set.seed(250531)

#Ensure your data frame is named `df`
# Step 1: Define the sampling function
sample_ea_by_district <- function(data, n_ea = 6) {
  data %>%
    group_by(dist_name,  # Group by district name sample 12 EA 
             residence # sample by residence type 6 rural and 6 urban
             ) %>%
    group_modify(~ {
      .x %>%
        slice_sample(n = n_ea, weight_by = hh)
    }) %>%
    ungroup()
}

# Step 2: Apply the sampling function to the data frame

sampled_df <- sample_ea_by_district(df)