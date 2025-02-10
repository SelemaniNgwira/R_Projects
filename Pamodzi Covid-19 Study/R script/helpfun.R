

# this function will run t.tset or Wilcoxon test based on shapiro test p-value
my_fun <- function(data, x, y) {
  
          # Extract the paired values
          x_values <- data[[x]]
          y_values <- data[[y]]
          
          # Check for normality using Shapiro-Wilk test on the differences
          diff_scores <- x_values - y_values
          shapiro_test <- shapiro.test(diff_scores)
          
          if (shapiro_test$p.value > 0.05) {
            
            # If normal, run paired t-test
            t_test_result <- t.test(x_values, y_values, paired = TRUE)
            return(t_test_result)
            
          } else {
            
            # If not normal, remove zero-difference pairs
            non_zero_indices <- x_values != y_values
            x_filtered <- x_values[non_zero_indices]
            y_filtered <- y_values[non_zero_indices]
            
            # Run Wilcoxon signed-rank test
            wilcoxon_result <- wilcox.test(x_filtered, y_filtered, 
                                           paired = TRUE,
                                           conf.int = TRUE,
                                           exact = FALSE)
            return(wilcoxon_result)
          }
}

       
# how to use the function

# my_fun(data = , "x", "y")
 