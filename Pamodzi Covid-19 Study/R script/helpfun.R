

# this function will run t.tset or Wilcoxon test based on shapiro test p-value
my_fun <- function(df_paired) {
  
  # Check for normality using Shapiro-Wilk test on the differences
  diff_scores <- df_paired$moh_score - df_paired$gen_score
  shapiro_test <- shapiro.test(diff_scores)
  
  if (shapiro_test$p.value > 0.05) {
    
    # If normal, run paired t-test
    t_test_result <- t.test(df_paired$moh_score, df_paired$gen_score, paired = TRUE)
    return(t_test_result)
    
  } else {
    
    # If not normal, run Wilcoxon signed-rank test
    df_filtered <- df_paired[df_paired$moh_score != df_paired$gen_score, ]
    wilcoxon_result <- wilcox.test(df_filtered$moh_score, df_filtered$gen_score, 
                                   paired = TRUE, conf.int = TRUE, exact = FALSE)
    return(wilcoxon_result)
  }
}

       
        
 