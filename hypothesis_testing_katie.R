library(tidyverse)

df <- read.csv("data/cleaned/natality_cleaned.csv")

#detailed birth weight 
outcome <- "dbwt"

#creating chronic condition flag based on columns (true = chronic)
df$chronic_condition <- ifelse(
  df$rf_pdiab == 1 | df$rf_gdiab == 1 |
  df$rf_phype == 1 | df$rf_ghype == 1 |
  df$rf_ehype == 1,
  1, 0
)

#splitting into chronic vs non-chronic groups 
group_chronic <- df %>%
  filter(chronic_condition == 1) %>%
  pull(outcome)


group_nonchronic <- df %>%
  filter(chronic_condition == 0) %>%
  pull(outcome)

#running independent samples t-test 
t_test_result <- t.test(group_chronic, group_nonchronic)

print("T-Test Results:")
print(t_test_result)