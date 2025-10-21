#Question1a
matrix <- matrix(c(14, 6, 7,
                7, 7, 1),
              nrow = 2, byrow = TRUE)
rownames(matrix) <- c("Upper", "Lower")
colnames(matrix) <- c("Not_Stopped", "Bribe_Requested", "Stopped_Warning")
matrix
row_totals <- rowSums(matrix)
col_totals <- colSums(matrix)
grand_total <- sum(matrix)
expected <- outer(row_totals, col_totals) / grand_total
expected
chi_sq_stat <- sum((matrix - expected)^2 / expected)
chi_sq_stat

#Question1b
df = 2
p_value <- 1 - pchisq(chi_sq_stat, df)
p_value
#Question1c
#Standardized Residuals 
chi.test <- chisq.test(matrix)
residuals <- chi.test$residuals 
residuals 

#Question2b
library(stargazer)
women <- read.csv("women.csv")
model <- lm(water ~ reserved, data = women)
stargazer(model,
          type = "latex",   #
          title = "Effect of Reservation Policy on Drinking-Water Facilities",
          dep.var.labels = "Number of New/Repaired Water Facilities",
          covariate.labels = c("Reserved for Women"),
          table.placement = "!htbp",         
          digits = 3,                         
          align = TRUE,
          no.space = TRUE)


