# 06_model_specification.R

library(tidyverse)
library(here)
library(readxl)

# Dummies ----

gss <- read_excel(here("raw_data", "GSS2008.xls")) %>% 
  rename( # I like lower case names better
    educ = EDUC,
    income = INCOME,
    sex = SEX,
    hrs = HRS,
    age = AGE,
    race = RACE,
    childs = CHILDS
  ) %>% 
  mutate( 
    female = sex == 2,
    inc_1000 = income / 1000
  ) %>% 
  filter(
    educ != 0,
    age <= 65
  )

mod_1 <- lm(inc_1000 ~ educ + age + hrs, data = gss)
summary(mod_1)

mod_2 <- lm(inc_1000 ~ female + educ + age + hrs, data = gss)
summary(mod_2)

mod_3 <- lm(inc_1000 ~ female * educ + age + hrs, data = gss)
summary(mod_3)

# Polynomials ----

# Effect of age on income
# White women aged 20 to 60
gss_women <- gss %>% 
  filter(female == TRUE, age >= 20, age <= 60, race == 1)

summary(gss_women[c("age", "educ", "income", "hrs")])

women_mod_1 <- lm(inc_1000 ~ educ + age + hrs, data = gss_women)
summary(women_mod_1)

women_mod_2 <- lm(inc_1000 ~ educ + age + I(age^2) + hrs, data = gss_women)
summary(women_mod_2)

# Effect of education on kids
# Women aged 46 to 55
gss_kids <- gss %>% 
  filter(female == T, age >= 46, age <= 55)

summary(gss_kids[c("childs", "educ")])

kids_mod_1 <- lm(childs ~ educ, data = gss_kids)
summary(kids_mod_1)

kids_mod_2 <- lm(childs ~ educ + I(educ^2), data = gss_kids)
summary(kids_mod_2)

gss_kids_small <- gss_kids %>% 
  mutate(obs_row = row_number(educ)) %>% 
  filter(obs_row > 5)

kids_mod_3 <- lm(childs ~ educ + I(educ^2), data = gss_kids_small)
summary(kids_mod_3)


# Logarithms ----

log_model_1 <- lm(log(income) ~ female + educ + age + hrs, data = gss)
summary(log_model_1)

log_model_2 <- lm(log(income) ~ female + log(educ) + age + hrs, data = gss)
summary(log_model_2)

log_model_3 <- lm(log(income) ~ female*educ + age + hrs, data = gss)
summary(log_model_3)
