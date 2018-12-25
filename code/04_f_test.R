# 04 - multivariate regression

library(car)

# [, 1]	mpg	Miles/(US) gallon
# [, 2]	cyl	Number of cylinders
# [, 3]	disp	Displacement (cu.in.)
# [, 4]	hp	Gross horsepower
# [, 5]	drat	Rear axle ratio
# [, 6]	wt	Weight (1000 lbs)
# [, 7]	qsec	1/4 mile time
# [, 8]	vs	Engine (0 = V-shaped, 1 = straight)
# [, 9]	am	Transmission (0 = automatic, 1 = manual)
# [,10]	gear	Number of forward gears
# [,11]	carb	Number of carburetors

car_ur <- lm(mpg ~ cyl + disp + hp + wt, data = mtcars)
summary(car_ur)

# disp and hp closely correlated
cor(mtcars$disp,mtcars$hp)

# Neither are statistically significant but
# are they jointly significant?

# Restricted model
car_r <- lm(mpg ~ cyl + wt, data = mtcars)
summary(car_r)

# The R-squared version
r2_ur <- summary(car_ur)$r.squared 
r2_r <- summary(car_r)$r.squared 
q <- length(car_ur$coefficients) - 
  length(car_r$coefficients)
n_k_1 <- (length(car_r$residuals) - 
            length(car_ur$coefficients)) 
      # no need for -1 since this counts all coefficients
((r2_ur - r2_r)/q)/((1 - r2_ur) / n_k_1)
qf(.9, df1 = q, df2 = n_k_1) # CV for 10% sig level  

# The function version
library(car)
Hnull <- c("disp = 0", "hp = 0")
linearHypothesis(car_ur, Hnull)

# The silly equal to version
Hnull <- c("disp = hp")
linearHypothesis(car_ur, Hnull)

