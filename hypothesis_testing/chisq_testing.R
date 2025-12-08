# chi sq tests
library(tidyverse)

df <- read_csv("data/cleaned/natality_cleaned.csv")

head(df)

chisq <- function(A, correct = FALSE){
  chisq.test(A, correct = correct)
}


set.seed(5100)
# are different chronic conditions independent of admission to nicu?

# pre exsting diabetes
tab1 <- table(df$rf_pdiab, df$ab_nicu, dnn = c("Pre-existing Diabetes", "NICU Admission"))
tab1

chi_tab1 <- chisq(tab1)
chi_tab1

chi_tab1$expected


# gestational diabetes
tab2 <- table(df$rf_gdiab, df$ab_nicu, dnn = c("Gestational Diabetes", "NICU Admission"))
tab2

chi_tab2 <- chisq(tab2)
chi_tab2

chi_tab2$expected

# pre-pregnancy hypertension
tab3 <- table(df$rf_phype, df$ab_nicu, dnn = c("Pre-Pregnancy Hypertension", "NICU Admission"))
tab3

chi_tab3 <- chisq(tab3)
chi_tab3

chi_tab3$expected


# gestational hypertension
tab4 <- table(df$rf_ghype, df$ab_nicu, dnn = c("Gestational Hypertension", "NICU Admission"))
tab4

chi_tab4 <- chisq(tab4)
chi_tab4

chi_tab4$expected


# eclampsia - needs permutation test
tab5 <- table(df$rf_ehype, df$ab_nicu, dnn = c("Eclampsia", "NICU Admission"))
tab5

chi_tab5 <- chisq(tab5)
chi_tab5

chi_tab5$expected
chi_tab5$statistic

chi_tab5_perm <- chisq.test(tab5, correct = FALSE, simulate.p.value = TRUE, B = 10000) 
chi_tab5_perm


# no risks 
tab6 <- table(df$no_risks, df$ab_nicu, dnn = c("No Risks", "NICU Admission"))
tab6

chi_tab6 <- chisq(tab6)
chi_tab6

chi_tab6$expected

df$chronic_condition <- ifelse(
  df$rf_pdiab == 1 | df$rf_gdiab == 1 |
  df$rf_phype == 1 | df$rf_ghype == 1 |
  df$rf_ehype == 1,
  1, 0
)

df$chronic_condition <- factor(
  df$chronic_condition,
  levels = c(0, 1),
  labels = c("No chronic condition", "Chronic condition")
)

tab_chronic_condition <- table(
  ChronicCondition = df$chronic_condition,
  NICU = factor(df$ab_nicu,
                levels = c(FALSE, TRUE),
                labels = c("No NICU", "NICU"))
)
tab_chronic_condition

chi_tab_chronic_condition <- chisq(tab_chronic_condition)
chi_tab_chronic_condition

chi_tab_chronic_condition$expected
