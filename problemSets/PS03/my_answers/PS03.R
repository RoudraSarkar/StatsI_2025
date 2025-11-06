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

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Question1.1 
# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2025/main/datasets/incumbents_subset.csv")
head(inc.sub)
summary(inc.sub)
model <- lm(voteshare ~ difflog, data = inc.sub)
summary(model)

#Question1.2 
ggplot(inc.sub, aes(x = difflog, y = voteshare)) +
  geom_point(color = "blue", alpha = 0.6) +             
  geom_smooth(method = "lm", se = FALSE, color = "red") +  
  labs(
    title = "Incumbent Vote Share vs. Spending Difference",
    x = "Difference in Log Spending (difflog)",
    y = "Incumbent Vote Share"
  ) +
  theme_minimal()

#Question1.3 
residuals_model <- resid(model)
head(residuals_model)

#Question1.4
coef(model)

#Question2.1
model2 <- lm(presvote ~ difflog, data = inc.sub)
summary(model2)

#Question2.2
ggplot(inc.sub, aes(x = difflog, y = presvote)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Presidential Vote Share vs. Spending Difference",
    x = "Difference in Log Spending (difflog)",
    y = "Presidential Vote Share"
  ) +
  theme_minimal()

#Question2.3
residuals_model2 <- resid(model2)
head(residuals_model2)

#Question2.4
coef(model2)

#Question3.1 
model3 <- lm(voteshare ~ presvote, data = inc.sub)
summary(model3)

#Question3.2
ggplot(inc.sub, aes(x = presvote, y = voteshare)) +
  geom_point(color = "blue", alpha = 0.6) +                
  geom_smooth(method = "lm", se = FALSE, color = "red") +   
  labs(
    title = "Incumbent Vote Share vs. Presidential Vote Share",
    x = "Presidential Vote Share (presvote)",
    y = "Incumbent Vote Share (voteshare)"
  ) +
  theme_minimal()

#Question3.3
coef(model3)

#Question4.1
residuals_model_reg <- lm(residuals_model ~ residuals_model2)
summary(residuals_model_reg)

#Question4.2
ggplot(data = inc.sub, aes(x = residuals_model2, y = residuals_model)) +
  geom_point(color = "blue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Add-Variable Plot: voteshare residuals vs presvote residuals",
    x = "Residuals of presvote ~ difflog",
    y = "Residuals of voteshare ~ difflog"
  ) +
  theme_minimal()

#Question4.3 
coef(residuals_model_reg)
round(coef(residuals_model_reg), 4)

#Question5.1
model4 <- lm(voteshare ~ difflog + presvote, data = inc.sub)
summary(model4)

#Question5.2 
coef(model4)



