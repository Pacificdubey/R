library(ggplot2)
library(tidyverse)
library(splines)
library(modEvA)
# load the dataset
ddat = diamonds


# check dataset
dim(ddat)


# view the dataset
View(ddat)
print(ddat)


# make a new variable dataset called "luxury" that assigns a value of 1 and 0.
ddat = ddat %>%
  mutate(luxury = case_when(price>=10000~1,
                                price<10000~0))


# use table function
table(ddat$luxury)




# glm model 
mod1 <- glm(luxury ~ carat , data = ddat , family = "binomial")

# summary
summary(mod1)

# to check for change in price over $10000
range(ddat$carat)
xweight <- seq(0, 6, 0.01)
yweight <- predict(mod1, list(carat=xweight),type="response")
plot(ddat$carat, ddat$luxury, pch = 16, xlab = "Carat", ylab = "Luxury")
lines(xweight, yweight)

#-------------------------------------------------------------------------------#
# Question 2
# scatter plot
g <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point()
  

# geom_smooth with df = 3,6,9
g+ stat_smooth(method="lm",formula = y ~ splines::ns(x, df = 3),aes(color = "DF_3")) +
  stat_smooth(method="lm",formula = y ~ splines::ns(x, df = 6),aes(color = "DF_6")) +
  stat_smooth(method="lm",formula = y ~ splines::ns(x, df = 9),aes(color = "DF_9")) +
  scale_color_manual("Degree of Freedom", values = c("DF_3" = "red", "DF_6" = "blue", "DF_9" = "green"))




