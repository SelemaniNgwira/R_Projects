
install.packages("epiR")
library(epiR)


epi.sscc(OR = 2, p1 = NA, p0 = 0.306, n = NA, power = 0.80, 
         r = 2, phi.coef = 0, design = 1, sided.test = 2, conf.level = 0.95, 
         method = "unmatched", nfractional = FALSE, fleiss = FALSE)

# OR =2.0, based on Bwenje et al 2024 estimate of o.306 for the control 
# group were vaccinated and a ratio of 1:2