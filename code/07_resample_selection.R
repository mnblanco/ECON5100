# 07_resample_selection.R

library(tidyverse)
library(here)
library(readxl)
library(boot)

# Data ----

gss <- read_excel(here("raw_data", "GSS2008.xls")) 
# Quick and dirty way to get lower case names
# Could make this one line
var.names <- tolower(colnames(gss))
colnames(gss) <- var.names

gss_income <- gss %>% 
  mutate( 
    female = sex == 2,
    inc_1000 = income / 1000
  ) %>% 
  filter(
    educ != 0,
    age <= 65
  ) %>% 
  filter(
    !is.na(income),
    !is.na(hrs)
  )


# Validation set ----

summary(gss_income[c("female", "age", "educ", "income", "hrs")])

set.seed(2)
train <- sample(956, 478)

val_model_1 <- lm(log(income) ~ female + educ + age + hrs, 
                  data = gss_income, subset = train)
summary(val_model_1)

val_model_2 <- lm(log(income) ~ female + educ + age + I(age^2) + hrs, 
                  data = gss_income, subset = train)
summary(val_model_2)

val_model_3 <- lm(log(income) ~ female + educ + log(age) + hrs, 
                  data = gss_income, subset = train)
summary(val_model_3)

# Time for a function
mse <- function(model_name) {
  mean((log(gss_income$income) - 
          predict(model_name, gss_income))[-train]^2)
}

mse(val_model_1)
mse(val_model_2)
mse(val_model_3)

# The sapply way
models <- list(val_model_1, val_model_2, val_model_3)
sapply(models, mse)

# LOOCV ----

loocv_model_1 <- glm(log(income) ~ female + educ + age + hrs, data = gss_income)
summary(loocv_model_1)

loocv_model_2 <- glm(log(income) ~ female + educ + age + I(age^2) + hrs, data = gss_income)
summary(loocv_model_2)

loocv_model_3 <- glm(log(income) ~ female + educ + log(age) + hrs, data = gss_income)
summary(loocv_model_3)

models <- list(loocv_model_1, loocv_model_2, loocv_model_3)
sapply(models, function(x) cv.glm(gss_income, x)$delta[1])

# k-fold CV ----

sapply(models, function(x) cv.glm(gss_income, x, K = 10)$delta[1])


# Bootstrapping ----

# Effect of education on kids
# Women aged 46 up
gss_kids <- gss %>% 
  filter(
    sex == 2, 
    age >= 46,
    !is.na(childs),
    !is.na(educ)
    )

summary(gss_kids[c("childs", "educ")])

elas_mod_1 <- lm(childs ~ educ, data = gss_kids)
summary(elas_mod_1)

elas_mod_2 <- lm(log(childs) ~ log(educ), data = gss_kids)
summary(elas_mod_2)





