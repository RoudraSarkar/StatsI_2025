#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

#####################
# Problem 1
#####################

y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#Question1.1
#Sample size
n <- length(y)
# Sample mean and standard deviation
mean_iq <- mean(y)
sd_iq   <- sd(y)
# Standard error of the mean
se <- sd_iq / sqrt(n)
#Confidence Intervals 
# t critical value for 90% CI (two-tailed, df = n-1)
t_crit <- qt(0.90, df = n - 1)   
# Confidence interval
lower <- mean_iq - t_crit * se
upper <- mean_iq + t_crit * se
cat("90% Confidence Interval for mean IQ: [",lower,",",upper,"]\n")

#Question1.2
# One-sided test: is the mean greater than 100?
mu0 <- 100 #hypothesized mean
test_statistic <- (mean_iq - mu0)/se
test_statistic
#finding t_crit using alpha = 0.05
t_crit <- qt(0.05, df = n - 1, lower.tail = FALSE) 
t_crit
# comparison and decision
if(test_statistic > t_crit){
  decision <- "Reject H0: The school's mean IQ is significantly higher than 100."
} else {
  decision <- "Fail to Reject H0: No evidence that the school's mean IQ is higher than 100."
}
list(
  decision = decision
)

#####################
# Problem 2
#####################

#Question 2.1
#read expenditure data
expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/expenditure.txt", header=T)
#Explore Data 
str(expenditure)          # Check structure and variable types
summary(expenditure)      # Get descriptive statistics
head(expenditure)         # Preview first few rows
# Select only the variables of interest
exp_data <- expenditure[, c("Y", "X1", "X2", "X3")]
# Create the pairs plot
pairs(exp_data)
#Question2.2
#region and plot 
boxplot(Y ~ Region, data = expenditure,
        xlab = "Region",
        ylab = "Per Capita Expenditure",
        names= c("North East", "North Central","South", "West"))
#Question2.3 
#Colours and Symbols
region_colors <- c("pink", "cyan", "darkgreen", "yellow")
region_symbols <- c(17, 18, 19, 20) 

#plot
expenditure$Region <- factor(expenditure$Region,
                      levels = c(1,2,3,4),
                      labels = c("Northeast","North Central","South","West"))
plot(expenditure$X1, expenditure$Y,
     main = "Per Capita Expenditure vs Personal Income by Region",
     xlab = "Per Capita Personal Income (X1)",
     ylab = "Per Capita Expenditure (Y)",
     col = region_colors[expenditure$Region],
     pch = region_symbols[expenditure$Region])
#legend
legend("topleft",
       legend = levels(expenditure$Region),
       col = region_colors,
       pch = region_symbols,
       title = "Region")


      






      
      