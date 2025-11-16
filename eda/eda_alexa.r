library(tidyverse)

# columns to select
cols <- c(
    "bfacil", # birth place (hospital, home, etc.)
    "mager", # mother's single years of age
    "mrace6", # main race category
    "dmar", # marital status
    "meduc", # mother's education
    "precare", # month prenatal care began
    "bmi", # body mass index
    # this is the start of risk factors up until NO_RISKS. Not all risk factors were selected
    "rf_pdiab", # pre-pregnancy diabetes
    "rf_gdiab", # gestational diabetes
    "rf_phype", # pre-pregnancy hypertension
    "rf_ghype", # gestational hypertesnion
    "rf_ehype", # hypertension eclampsia
    "no_risks", # no risk factors reported
    "ip_gon", # gonorrhea
    "ip_syph", # syphilis
    "ip_chlam", # chlamydia
    "ip_hepb", # hep b
    "ip_hepc", # hepc
    "no_infec", # no infections reported
    "ld_indl", # induction of labor
    "ld_augm", # labor augmentation
    "ld_ster", # steroids
    "ld_antb", # antibiotics
    "ld_chor", #chorioamniotis
    "ld_anes", # anesthesia
    "me_rout", # final route & method of delivery
    # here to MM_AICU is under the categery of maternal morbidity
    "mm_mtr", # maternal transfusion 
    "mm_plac", # perineal laceration
    "mm_rupt", # ruptured uterus
    "mm_uhyst", # unplanned hysterectomy
    "mm_aicu", # admit to intensive care
    # infant metrics
    "apgar5", # 5 minute APGAR score
    "dplural", # how many babies
    "sex", # infant sex
    "combgest", # combined gestation in weeks
    "dbwt", # birth weight in grams
    # abnormal conditions of the newborn
    "ab_aven1", # assisted ventilation immediate
    "ab_aven6", # assisted ventilation > 6 hours
    "ab_nicu", # admission to nicu
    "ab_surf", # surfactant
    "ab_anti", #antibiotics for newborn
    "ab_seiz", # seizures
    "no_abnorm" #no abnormal conditions
    # congenital anomalies of the newborn (very few)
    # "ca_anen", # anencephaly
    # "ca_mnsb", # meningomyelcole / spina bifida
    # "ca_cchd", # cyanotic congenital heart disease
    # "ca_cdh", # congenital diaphragmatic hernia
    # "ca_omph", # omphalocele
    # "ca_gast", #gastroschisis
    # "ca_limb", # cleft lip w/ or w/o cleft palate
    # "ca_clpal", # cleft palate alone
    # "ca_down", # down syndrome
    # "ca_disor", # suspected chromosomal disorder
    # "ca_hypo", # hypospadias
    # "no_congen" # no congenital anomalies checked")
)

# read selected columns and filter out unknown values
natality_all <- read_csv("data/all/natality2024us.csv", 
                         col_select = all_of(cols)) %>%
  filter(
    bfacil != 9 &
    meduc != 9 &
    precare != 99 &
    bmi != 99.9 &
    rf_pdiab != "U" &
    rf_gdiab != "U" &
    rf_phype != "U" &
    rf_ghype != "U" &
    rf_ehype != "U" &
    no_risks != 9 &
    ip_gon != "U" &
    ip_syph != "U" &
    ip_chlam != "U" &
    ip_hepb != "U" &
    ip_hepc != "U" &
    no_infec != 9 &
    ld_indl != "U" &
    ld_augm != "U" &
    ld_ster != "U" &
    ld_antb != "U" &
    ld_chor != "U" &
    ld_anes != "U" &
    me_rout != 9 &
    mm_mtr != "U" &
    mm_plac != "U" &
    mm_rupt != "U" &
    mm_uhyst != "U" &
    mm_aicu != "U" &
    apgar5 != 99 &
    combgest != 99 &
    dbwt != 9999 &
    ab_aven1 != "U" &
    ab_aven6 != "U" &
    ab_nicu != "U" &
    ab_surf != "U" &
    ab_anti != "U" &
    ab_seiz != "U" &
    no_abnorm != 9
    # ca_anen != "U" &
    # ca_mnsb != "U" &
    # ca_cchd != "U" &
    # ca_cdh != "U" &
    # ca_omph != "U" &
    # ca_gast != "U" &
    # ca_limb != "U" &
    # ca_clpal != "U" &
    # ca_down != "U" &
    # ca_down != "P" &
    # ca_disor != "U" &
    # ca_disor != "P" &
    # ca_hypo != "U" &
    # no_congen != 9
) %>%
  drop_na() 

# numeric cols without a set range
outlier_cols <- c("mager", "bmi", "dbwt")

is_outlier <- function(x) {
  Q1  <- quantile(x, 0.25, na.rm = TRUE)
  Q3  <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  lower <- Q1 - 1.5 * IQR
  upper <- Q3 + 1.5 * IQR
  x < lower | x > upper
}

outlier_flags <- natality_all %>%
  mutate(across(all_of(outlier_cols), is_outlier, .names = "out_{col}"))

outlier_flags <- outlier_flags %>%
  mutate(outlier_count = rowSums(select(., starts_with("out_"))))

natality_no_outliers <- outlier_flags %>%
  filter(outlier_count == 0) %>%
  select(-starts_with("out_"), -outlier_count)

nrow(natality_all)
nrow(natality_no_outliers)



# sample 10,000
natality_sample <- natality_no_outliers %>% sample_n(10000)


# Y/N columns
yn_cols <- natality_sample %>%
  select(where(~ all(.x %in% c("Y", "N")))) %>%
  names()

yn_cols

# confirmed/no columns
cn_cols <- natality_sample %>%
  select(where(~ all(.x %in% c("C", "N")))) %>%
  names()

cn_cols

# "no" columns (no infections, no risks, etc). 1 = True, 0 = False
no_cols <- natality_sample %>%
  select(where(~ all(.x %in% c(1, 0)))) %>%
  names()

no_cols


natality_clean <- natality_sample %>%
  mutate(across(yn_cols,
                ~ case_when(
                    .x == "Y" ~ TRUE,
                    .x == "N" ~ FALSE,
                    TRUE ~ NA
                ))) %>%
  mutate(across(cn_cols,
                ~ case_when(
                    .x == "C" ~ TRUE,
                    .x == "N" ~ FALSE,
                    TRUE ~ NA
                ))) %>%
  mutate(across(no_cols,
                ~ case_when(
                    .x == 1 ~ TRUE,
                    .x == 0 ~ FALSE,
                    TRUE ~ NA
                )))



# recode character columns
numeric_cols <- natality_clean %>% 
  select(where(is.numeric)) %>% 
  colnames()
numeric_cols

natality_clean <- natality_clean %>%
  mutate(
    bfacil = recode(bfacil,
                    `1` = "Hospital",
                    `2` = "Freestanding Birth Center",
                    `3` = "Home (intended)",
                    `4` = "Home (not intended)",
                    `5` = "Home (unknown if intended)",
                    `6` = "Clinic / Doctor's Office",
                    `7` = "Other")
  ) %>%
  mutate(
    mrace6 = recode(mrace6,
                    `1` = "White",
                    `2` = "Black",
                    `3` = "American Indian or Alaska Native",
                    `4` = "Asian",
                    `5` = "Native Hawaiian and Other Pacific Islander",
                    `6` = "More than one race")
  ) %>%
  mutate(
    dmar = recode(dmar,
                    `1` = "Married",
                    `2` = "Unmarried" )
  ) %>%
  mutate(
    meduc = recode(meduc,
                    `1` = "8th grade or less",
                    `2` = "9th-12th no diploma",
                    `3` = "High school graduate or GED completed",
                    `4` = "Some college credit, not a degree",
                    `5` = "Associate degree",
                    `6` = "Bachelor's degree",
                    `7` = "Master's degree",
                    `8` = "Doctorate")
  ) %>%
  mutate(
    me_rout = recode(me_rout,
                    `1` = "Spontaneous",
                    `2` = "Forceps",
                    `3` = "Vacuum",
                    `4` = "Cesarean")
  )

write_csv(natality_clean, "data/cleaned/natality_cleaned.csv")


## summary statistics

summary(natality_clean)

# numeric
natality_clean %>%
  reframe(across(
    where(is.numeric),
    \(x) c(
      mean = mean(x, na.rm = TRUE),
      sd   = sd(x, na.rm = TRUE),
      min  = min(x, na.rm = TRUE),
      max  = max(x, na.rm = TRUE)
    )
  ))

# categorical summary
natality_clean %>% 
  select(where(is.character)) %>%
  map(table)