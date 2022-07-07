library(faraway)
library(GGally)
library(tidyverse)

data("gala")
# Load dataset into variable
mydata = gala
print(mydata)
#-----------------------------Data Visualization To understand data -------------------------#
# Descriptive Analysis data
summary(mydata)

hist(mydata$Species, breaks = 10, xlab = "Species count", main = "Distribution of Species", prob = TRUE)
lines(density(mydata$Species, na.rm = T), col = "red", lwd = 4)

ggpairs(mydata %>% select(Area, Elevation, Nearest,
                        Scruz, Adjacent, Species))

#=====================================Linear Regression Model===============================================#
model1 <- glm(mydata$Species ~ ., data = mydata, family = poisson())
summary(model1)