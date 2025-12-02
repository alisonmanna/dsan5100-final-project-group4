# ANOVA
# > *Question: _____*
# - **Null hypothesis** ($H_0$)
# - **Alternative hypothesis** ($H_1$
# Hypothesis test: ANOVA

library(dplyr)
library(ggplot2)
library(car)
library(effectsize)

# load data
births <- read.csv("data/cleaned/natality_cleaned.csv")
# head(births)

# Create condition groups for analysis
births_clean <- births |>
  mutate(
    # CBinary indicators for any diabetes
    has_diabetes = ifelse(rf_pdiab == TRUE | rf_gdiab == TRUE, 1, 0),
    
    # Binary indicator for any hypertension
    has_hypertension = ifelse(rf_phype == TRUE | rf_ghype == TRUE | 
                              rf_ehype == TRUE, 1, 0),
    
    # Create 4 condition groups
    condition_group = case_when(
      has_diabetes == 0 & has_hypertension == 0 ~ "No Conditions",
      has_diabetes == 1 & has_hypertension == 0 ~ "Diabetes Only",
      has_diabetes == 0 & has_hypertension == 1 ~ "Hypertension Only",
      has_diabetes == 1 & has_hypertension == 1 ~ "Both Conditions"
    ),
    
    # Convert to factor w/ ordered levels
    condition_group = factor(condition_group, 
                            levels = c("No Conditions", "Diabetes Only", 
                                     "Hypertension Only", "Both Conditions"))
  )

# Check sample sizes
table(births_clean$condition_group)

# Summary statistics by condition group
summary_stats <- births_sample %>%
  group_by(condition_group) %>%
  summarise(
    n = n(),
    mean_bwt = round(mean(dbwt, na.rm = TRUE), 1),
    sd_bwt = round(sd(dbwt, na.rm = TRUE), 1),
    median_bwt = median(dbwt, na.rm = TRUE),
    min_bwt = min(dbwt, na.rm = TRUE),
    max_bwt = max(dbwt, na.rm = TRUE)
  )

print(summary_stats)