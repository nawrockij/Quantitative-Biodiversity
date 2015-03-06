---
title: "Quantitative Biodiversity Functions Source Code"
author: "Jenna Nawrocki"
date: "`r format(Sys.time(), '%d %B, %Y')`"
---
# Note: This file contains functions to calculate metrics of taxonomic and phylogenetic diversity. 


require("vegan")||install.packages("vegan");require("vegan")


# STANDARD ERROR OF THE MEAN
# Inputs: x (vector)

sem <- function(x){sd(na.omit(x))/sqrt(length(na.omit(x)))}


# CORRELATION
# Inputs: x (matrix)

# Parametric:
pairs(x)
cor1 <- cor(x)

require("psych")||install.packages("psych");require("psych")
cor2 <- corr.test(x, method = "pearson", adjust = "BH")
print(cor2, digits = 3)

# Non-parametric:
cor3 <- corr.test(x, method = "spearman", adjust = "BH")
print(cor3, digits = 3)


# LINEAR REGRESSION
# Inputs: x and y (vectors)

fitreg <- lm(ZP ~ TN, data = meso)
summary(fitreg)
plot(meso$TN, meso$ZP, ylim = c(0, 10), xlim = c(500, 5000), 
     xlab = expression(paste("Total Nitrogen (", mu,"g/L)")), 
     ylab = "Zooplankton Biomass (mg/L)", las = 1)
text(meso$TN, meso$ZP,meso$NUTS,pos=3,cex=0.8)
newTN <- seq(min(meso$TN), max(meso$TN), 10)
regline <- predict(fitreg, newdata = data.frame(TN = newTN))
lines(newTN, regline)
conf95 <- predict(fitreg, newdata = data.frame(TN = newTN), 
                  interval = c("confidence"), level = 0.95, type = "response")
matlines(newTN, conf95[, c("lwr", "upr")], type="l", lty = 2, lwd = 1, col = "black")
