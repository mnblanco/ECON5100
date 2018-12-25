# Code for lecture 3
library(tidyverse)
library(here)

rate = c(8.5, 7.8, 7.6, 8.0, 8.4)
starts = c(115, 111, 185, 206, 167)
df = data.frame(rate, starts)

housing_results <- lm(starts ~ rate,
                      data = df)

summary(housing_results)

# Correlation

housing_starts <- read_csv(here("raw_data", "housing.csv"))

housing_starts %>% 
  summarise(
    sd_rate = sd(rate),
    sd_starts = sd(starts)
  )

cor(housing_starts$rate, housing_starts$starts)

# Alumni giving rate

alumni <- read_csv(here("raw_data", "Alumni.csv"))

alumni_results <- lm(alumnigivingrate ~ sfratio,
                     data = alumni)

summary(alumni_results)

ggplot(alumni, 
  aes(x = sfratio, y = alumnigivingrate)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  xlab("Student / Faculty Ratio") +
  ylab("Percent Alumni who Donate") +
  ggtitle("Relationship between Student/Faculty Ratio and Donations")

ggsave(here("figures", "alumni_regression.png"))

ggplot(alumni, 
       aes(x = sfratio, y = alumnigivingrate)) +
  geom_jitter() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  xlab("Student / Faculty Ratio") +
  ylab("Percent Alumni who Donate") +
  ggtitle("Relationship between Student/Faculty Ratio and Donations with Jitter")

ggsave(here("figures", "alumni_regression_jitter.png"))

