library(tidyverse)
library(ggplot2)
library(dplyr)

df <- read.csv("data/cleaned/natality_cleaned.csv")
head(df)

#histograms for numeric columns 
number_columns <- df %>% select(where(is.numeric)) %>% colnames()

for (col in number_columns) {
  print(
    ggplot(df, aes_string(x=col)) +
      geom_histogram(bins = 30, fill = "skyblue", color = "black") + 
      ggtitle(paste("Histogram of Infant Birthweight")) + 
      theme_minimal()
  )
}

# to save to png
for (col in number_columns) {

  p <- ggplot(df, aes_string(x = col)) +
    geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    ggtitle(paste("Histogram of Infant Birthweight")) +
    theme_minimal()

  print(p)

  # Save to PNG
  ggsave(
    filename = paste0("../plots/eda/histogram_.png", col, ".png"),
    plot = p,
    width = 6,
    height = 4,
    dpi = 300
  )
}

#density KDE plots 
for (col in number_cols) {
  print(
    ggplot(df, aes_string(x = col)) + 
      geom_density(fill = "lightgreen", alpha = 0.5) + 
      ggtitle(paste("Density Plot of", col)) + 
      theme_minimal()
  )
}

#violin plots (outcome vs. chronic condition)
outcome_columns <- c("birth_weight")

for (var in outcome_columns) {
  print(
    ggplot(df, aes(x=factor(chronic_condition), y = .data[[var]])) +
      geom_violin(fill = "plum", alpha = 0.6) + 
      geom_boxplot(width = 0.15, outlier.color = "red") + 
      labs(
        x = "Chronic Condition (0 = No, 1 = Yes)", 
        y = var,
        title = paste("Violin Plot of", var, "by Chronic Condition")
      ) + 
      theme_minimal()
  )
}

#t-test: chronic vs non-chronic
outcome <- "birth_weight"

group1 <- df %>% filter(chronic_condition == 1) %>% pull(outcome)
group2 <- df %>% filter(chronic_condition == 0) %>% oull(outcome)

t_test_result <- t.test(group1, group2)

print("T-test Results:")
print(t_test_result)


