library(tidyverse)
library(rio)
library(here)


# Load the data

mchinji_hf<- import(
  here("data/mchinji_hf_pop.xlsx")
) 

# sample 7 health facilities from Mchinji district

set.seed(25236)

sample(mchinji_hf$health_facility, 
       size = 3, 
       replace = FALSE, 
       prob = mchinji_hf$weight
)

