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
lapply(c("car"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Run Packages 
install.packages("car")
library(car)
data(Prestige)
help(Prestige)

#Question1.1 
# Create new dummy variable
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
# Check the result
table(Prestige$type, Prestige$professional)

#Question1.2
model <- lm(prestige ~ income * professional, data = Prestige)
summary(model)
install.packages("stargazer")
library(stargazer)
stargazer(
  model,
  type = "latex",
  title = "Linear Model of Prestige on Income, Professional Status, and Interaction",
  label = "tab:prestige_model",
  dep.var.labels = c("Prestige"),
  covariate.labels = c("Income", "Professional", "Income * Professional"),
  digits = 3,
  no.space = TRUE)

#Question1.3
coef(model)

