# chi sq tests
library(tidyverse)

df <- read_csv("data/cleaned/natality_cleaned.csv")

head(df)

chisq <- function(A){
  chisq.test(A)
}


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


