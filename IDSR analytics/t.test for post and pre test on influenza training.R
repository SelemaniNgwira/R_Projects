
## loading data set

influenza_test <- read_excel("influenza_test.xlsx",
                             sheet = "BT Venue PRETEST 2")

## selecting the post and pre column 
influe_score<- influenza_test |>
  dplyr::select(`Pre-score`, `Post Score`)


## running the test 
t.test(influe_score$`Post Score`, 
       influe_score$`Pre-score`, 
       paired = TRUE,
       alternative = "two.sided")



shapiro.test(influe_score$`Post Score`)


wilcox.test(influe_score$`Post Score`, 
            influe_score$`Pre-score`, 
            paired = TRUE,
            alternative = "two.sided")