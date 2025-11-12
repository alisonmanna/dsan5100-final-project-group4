
# rf_pdiab - Pre-pregnancy (pre-existing) diabetes
# rf_gdiab - Gestational diabetes
# rf_phype - Pre-pregnancy (chronic) hypertension
# rf_ghype - Gestational hypertension
# rf_ehype - Eclampsia hypertension

# combgest - Combined gestation (gestational age in weeks)
# dbwt - Birth weight in grams
# dmeth_rec - Delivery method recode (1=vaginal, 2=cesarean)
# apgar5 - 5-minute APGAR score (APGAR = Appearance, Pulse, Grimace, Activity, Respiration)
# ab_nicu - NICU admission (Y/N)

# mager - Mother's age (continuous)
# mrace6 or mracehisp - Mother's race/ethnicity
# meduc - Mother's education
# bmi or bmi_r - Body mass index (continuous or categorized)
# priorlive - Number of prior live births (parity)
# sex - Baby's sex
# dplural - Plurality (singleton vs. twins/multiples)

# precare - Month prenatal care began
# cig_rec - Smoking status, recoded
# restatus - Residence status, useful for geographic analysis?

# Full dataset: 3638436 rows, 237 columns
# Reduced dataset: 
# --------------------------------------------

# Read data
births <- read.csv("../data/raw/natality2024us.csv")
# head(births)
# summary(births)

# Check dimensions
dim(births)

# Select relevant variables for analysis
births_subset <- births[, c(
  # Exposures
  "rf_pdiab", "rf_gdiab", "rf_phype", "rf_ghype", "rf_ehype",
  # Outcomes
  "combgest", "dbwt", "dmeth_rec", "apgar5", "ab_nicu",
  # Covariates/controls
  "mager", "mracehisp", "meduc", "bmi_r", "priorlive", 
  "sex", "dplural",
  # Additional useful variables ?
  "precare", "cig_rec", "restatus"
)]

# Check the structure
str(births_subset)
summary(births_subset)

# save
write.csv(births_subset, "../data/cleaned/nber_births_subset.csv", row.names = FALSE)