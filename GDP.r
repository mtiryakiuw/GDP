# ============================================================================
# GENDER PAY GAP - THREE PANEL DATA MODELS (2010-2022)
# 4 Time Periods: 2010, 2014, 2018, 2022
# Model 1: Age × Occupation
# Model 2: Sector × Occupation (18 detailed sectors)
# Model 3: Sector × Education
# ============================================================================

library(tidyverse)
library(eurostat)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)

cat("============================================\n")
cat("GENDER PAY GAP PANEL DATA ANALYSIS\n")
cat("2010-2022 | Four Time Periods\n")
cat("Three Complementary Models\n")
cat("============================================\n\n")

# ============================================================================
# LOAD ALL DATASETS (4 YEARS)
# ============================================================================

cat("📥 STEP 1: LOADING DATA (2010, 2014, 2018, 2022)...\n")
cat("-----------------------------------------------------\n")

# Panel 1: Age × Occupation
cat("  Loading Age × Occupation...\n")
age_occ_2022 <- get_eurostat("earn_ses22_28", time_format = "num")
age_occ_2018 <- get_eurostat("earn_ses18_28", time_format = "num")
age_occ_2014 <- get_eurostat("earn_ses14_28", time_format = "num")
age_occ_2010 <- get_eurostat("earn_ses10_28", time_format = "num")

age_occ_panel <- bind_rows(
  age_occ_2022 %>% mutate(year = 2022),
  age_occ_2018 %>% mutate(year = 2018),
  age_occ_2014 %>% mutate(year = 2014),
  age_occ_2010 %>% mutate(year = 2010)
)
cat("    ✅ Age × Occupation loaded:", nrow(age_occ_panel), "rows\n")

# Panel 2: Sector × Occupation
cat("  Loading Sector × Occupation...\n")
sec_occ_2022 <- get_eurostat("earn_ses22_49", time_format = "num")
sec_occ_2018 <- get_eurostat("earn_ses18_49", time_format = "num")
sec_occ_2014 <- get_eurostat("earn_ses14_49", time_format = "num")
sec_occ_2010 <- get_eurostat("earn_ses10_49", time_format = "num")

sec_occ_panel <- bind_rows(
  sec_occ_2022 %>% mutate(year = 2022),
  sec_occ_2018 %>% mutate(year = 2018),
  sec_occ_2014 %>% mutate(year = 2014),
  sec_occ_2010 %>% mutate(year = 2010)
)
cat("    ✅ Sector × Occupation loaded:", nrow(sec_occ_panel), "rows\n")

# Panel 3: Sector × Education
cat("  Loading Sector × Education...\n")
sec_edu_2022 <- get_eurostat("earn_ses22_30", time_format = "num")
sec_edu_2018 <- get_eurostat("earn_ses18_30", time_format = "num")
sec_edu_2014 <- get_eurostat("earn_ses14_30", time_format = "num")
sec_edu_2010 <- get_eurostat("earn_ses10_30", time_format = "num")

sec_edu_panel <- bind_rows(
  sec_edu_2022 %>% mutate(year = 2022),
  sec_edu_2018 %>% mutate(year = 2018),
  sec_edu_2014 %>% mutate(year = 2014),
  sec_edu_2010 %>% mutate(year = 2010)
)
cat("    ✅ Sector × Education loaded:", nrow(sec_edu_panel), "rows\n")

cat("\n  ✅ All data loaded successfully\n\n")

# ============================================================================
# MAPPINGS
# ============================================================================

cat("📋 STEP 2: SETTING UP MAPPINGS...\n")
cat("------------------------------------\n")

# Age groups (harmonized across years)
age_mapping <- c(
  'Y_LT30' = 'Young',
  'Y30-49' = 'Mid Career',
  'Y_GE50' = 'Senior'
)

# Education levels (harmonized)
# 2010 uses isced97, 2014+ uses isced11
education_mapping_11 <- c(
  'ED0-2' = 'Low Education',
  'ED3_4' = 'Medium Education',
  'ED5-8' = 'High Education'
)

education_mapping_97 <- c(
  'ISCED0-2' = 'Low Education',
  'ISCED3-4' = 'Medium Education',
  'ISCED5-6' = 'High Education'
)

# Occupations (ISCO-08, consistent across 2010-2022)
occupation_mapping <- c(
  'OC1' = 'Managers', 'OC2' = 'Professionals', 'OC3' = 'Technicians',
  'OC4' = 'Clerical', 'OC5' = 'Service', 'OC6' = 'Agriculture',
  'OC7' = 'Craft', 'OC8' = 'Operators', 'OC9' = 'Elementary'
)

# Sectors (18 detailed sectors, NACE Rev 2)
sector_mapping <- c(
  'B' = 'Mining', 'C' = 'Manufacturing', 'D' = 'Electricity',
  'E' = 'Water', 'F' = 'Construction', 'G' = 'Trade',
  'H' = 'Transport', 'I' = 'Hospitality', 'J' = 'IT',
  'K' = 'Finance', 'L' = 'Real Estate', 'M' = 'Professional',
  'N' = 'Admin Services', 'O' = 'Public Admin', 'P' = 'Education Sector', 
  'Q' = 'Health', 'R' = 'Arts', 'S' = 'Other Services'
)

# EU aggregations to exclude
eu_agg <- c("EU27_2020", "EU28", "EU27_2007", "EA19", "EA20", "EA18", "EA17")

cat("  ✅ Mappings ready\n\n")

# ============================================================================
# PROCESS PANEL 1: AGE × OCCUPATION
# ============================================================================

cat("📊 STEP 3: PROCESSING PANEL 1 (Age × Occupation)...\n")
cat("------------------------------------------------------\n")

panel1_clean <- age_occ_panel %>%
  filter(
    !is.na(values),
    sex %in% c("M", "F"),
    indic_se == "ERN",
    age %in% names(age_mapping),
    isco08 %in% names(occupation_mapping),
    isco08 != "OC0", isco08 != "TOTAL",
    !str_detect(isco08, "-"),
    !geo %in% eu_agg
  ) %>%
  mutate(
    country = geo,
    age_group = age_mapping[age],
    occupation = occupation_mapping[isco08]
  ) %>%
  group_by(country, year, age_group, occupation, sex) %>%
  summarise(earnings = mean(values, na.rm = TRUE), .groups = 'drop')

# Calculate gender pay gap
gap_panel1 <- panel1_clean %>%
  pivot_wider(names_from = sex, values_from = earnings, names_prefix = "earn_") %>%
  filter(!is.na(earn_M), !is.na(earn_F)) %>%
  mutate(
    gender_pay_gap = ((earn_M - earn_F) / earn_M) * 100,
    panel_id = paste(country, age_group, occupation, sep = "_"),
    
    # Dummy variables
    young = as.numeric(age_group == "Young"),
    senior = as.numeric(age_group == "Senior"),
    high_skill = as.numeric(occupation %in% c("Managers", "Professionals")),
    managerial = as.numeric(occupation == "Managers"),
    
    # Year trends
    year_2014 = as.numeric(year == 2014),
    year_2018 = as.numeric(year == 2018),
    year_2022 = as.numeric(year == 2022)
  ) %>%
  filter(is.finite(gender_pay_gap))

cat("  Observations:", nrow(gap_panel1), "\n")
cat("  Panels:", length(unique(gap_panel1$panel_id)), "\n")
cat("  Countries:", length(unique(gap_panel1$country)), "\n")
cat("  Years:", paste(sort(unique(gap_panel1$year)), collapse = ", "), "\n")
cat("  Mean gap:", round(mean(gap_panel1$gender_pay_gap), 2), "%\n\n")

# ============================================================================
# PROCESS PANEL 2: SECTOR × OCCUPATION
# ============================================================================

cat("📊 STEP 4: PROCESSING PANEL 2 (Sector × Occupation)...\n")
cat("--------------------------------------------------------\n")

panel2_clean <- sec_occ_panel %>%
  filter(
    !is.na(values),
    sex %in% c("M", "F"),
    indic_se == "ERN",
    nchar(nace_r2) == 1,
    nace_r2 %in% names(sector_mapping),
    isco08 %in% names(occupation_mapping),
    isco08 != "OC0", isco08 != "TOTAL",
    !str_detect(isco08, "-"),
    !geo %in% eu_agg
  ) %>%
  mutate(
    country = geo,
    sector = sector_mapping[nace_r2],
    occupation = occupation_mapping[isco08]
  ) %>%
  group_by(country, year, sector, occupation, sex) %>%
  summarise(earnings = mean(values, na.rm = TRUE), .groups = 'drop')

# Calculate gender pay gap
gap_panel2 <- panel2_clean %>%
  pivot_wider(names_from = sex, values_from = earnings, names_prefix = "earn_") %>%
  filter(!is.na(earn_M), !is.na(earn_F)) %>%
  mutate(
    gender_pay_gap = ((earn_M - earn_F) / earn_M) * 100,
    panel_id = paste(country, sector, occupation, sep = "_"),
    
    # Sector groups
    industry = as.numeric(sector %in% c("Mining", "Manufacturing", "Electricity", "Water")),
    construction = as.numeric(sector == "Construction"),
    services = as.numeric(sector %in% c("Trade", "Transport", "Hospitality", "IT", "Finance", "Real Estate", "Professional", "Admin Services")),
    public_sector = as.numeric(sector %in% c("Public Admin", "Education Sector", "Health")),
    
    # Occupation
    high_skill = as.numeric(occupation %in% c("Managers", "Professionals")),
    managerial = as.numeric(occupation == "Managers"),
    
    # Year trends
    year_2014 = as.numeric(year == 2014),
    year_2018 = as.numeric(year == 2018),
    year_2022 = as.numeric(year == 2022)
  ) %>%
  filter(is.finite(gender_pay_gap))

cat("  Observations:", nrow(gap_panel2), "\n")
cat("  Panels:", length(unique(gap_panel2$panel_id)), "\n")
cat("  Countries:", length(unique(gap_panel2$country)), "\n")
cat("  Years:", paste(sort(unique(gap_panel2$year)), collapse = ", "), "\n")
cat("  Mean gap:", round(mean(gap_panel2$gender_pay_gap), 2), "%\n\n")

# ============================================================================
# PROCESS PANEL 3: SECTOR × EDUCATION
# ============================================================================

cat("📊 STEP 5: PROCESSING PANEL 3 (Sector × Education)...\n")
cat("-------------------------------------------------------\n")

# Handle different education coding (isced97 vs isced11)
panel3_clean <- sec_edu_panel %>%
  filter(
    !is.na(values),
    sex %in% c("M", "F"),
    indic_se == "ERN",
    nchar(nace_r2) == 1,
    nace_r2 %in% names(sector_mapping),
    !geo %in% eu_agg
  ) %>%
  mutate(
    country = geo,
    sector = sector_mapping[nace_r2],
    # Harmonize education codes
    education = case_when(
      !is.na(isced11) & isced11 %in% names(education_mapping_11) ~ education_mapping_11[isced11],
      !is.na(isced97) & isced97 %in% names(education_mapping_97) ~ education_mapping_97[isced97],
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(education)) %>%
  group_by(country, year, sector, education, sex) %>%
  summarise(earnings = mean(values, na.rm = TRUE), .groups = 'drop')

# Calculate gender pay gap
gap_panel3 <- panel3_clean %>%
  pivot_wider(names_from = sex, values_from = earnings, names_prefix = "earn_") %>%
  filter(!is.na(earn_M), !is.na(earn_F)) %>%
  mutate(
    gender_pay_gap = ((earn_M - earn_F) / earn_M) * 100,
    panel_id = paste(country, sector, education, sep = "_"),
    
    # Education dummies
    medium_edu = as.numeric(education == "Medium Education"),
    high_edu = as.numeric(education == "High Education"),
    
    # Sector groups
    industry = as.numeric(sector %in% c("Mining", "Manufacturing", "Electricity", "Water")),
    construction = as.numeric(sector == "Construction"),
    services = as.numeric(sector %in% c("Trade", "Transport", "Hospitality", "IT", "Finance", "Real Estate", "Professional", "Admin Services")),
    public_sector = as.numeric(sector %in% c("Public Admin", "Education Sector", "Health")),
    
    # Year trends
    year_2014 = as.numeric(year == 2014),
    year_2018 = as.numeric(year == 2018),
    year_2022 = as.numeric(year == 2022)
  ) %>%
  filter(is.finite(gender_pay_gap))

cat("  Observations:", nrow(gap_panel3), "\n")
cat("  Panels:", length(unique(gap_panel3$panel_id)), "\n")
cat("  Countries:", length(unique(gap_panel3$country)), "\n")
cat("  Years:", paste(sort(unique(gap_panel3$year)), collapse = ", "), "\n")
cat("  Mean gap:", round(mean(gap_panel3$gender_pay_gap), 2), "%\n\n")

# ============================================================================
# DESCRIPTIVE STATISTICS
# ============================================================================

cat("📈 STEP 6: DESCRIPTIVE STATISTICS\n")
cat("====================================\n\n")

cat("PANEL 1 (Age × Occupation):\n")
cat("By Year:\n")
gap_panel1 %>%
  group_by(year) %>%
  summarise(mean_gap = round(mean(gender_pay_gap), 2), n = n()) %>%
  print()

cat("\nBy Age:\n")
gap_panel1 %>%
  group_by(age_group) %>%
  summarise(mean_gap = round(mean(gender_pay_gap), 2), n = n()) %>%
  print()

cat("\nBy Occupation (Top 5):\n")
gap_panel1 %>%
  group_by(occupation) %>%
  summarise(mean_gap = round(mean(gender_pay_gap), 2), n = n()) %>%
  arrange(desc(mean_gap)) %>%
  head(5) %>%
  print()

cat("\n\nPANEL 2 (Sector × Occupation):\n")
cat("By Year:\n")
gap_panel2 %>%
  group_by(year) %>%
  summarise(mean_gap = round(mean(gender_pay_gap), 2), n = n()) %>%
  print()

cat("\nBy Sector (Top 5):\n")
gap_panel2 %>%
  group_by(sector) %>%
  summarise(mean_gap = round(mean(gender_pay_gap), 2), n = n()) %>%
  arrange(desc(mean_gap)) %>%
  head(5) %>%
  print()

cat("\n\nPANEL 3 (Sector × Education):\n")
cat("By Year:\n")
gap_panel3 %>%
  group_by(year) %>%
  summarise(mean_gap = round(mean(gender_pay_gap), 2), n = n()) %>%
  print()

cat("\nBy Education:\n")
gap_panel3 %>%
  group_by(education) %>%
  summarise(mean_gap = round(mean(gender_pay_gap), 2), n = n()) %>%
  print()

cat("\nBy Sector (Top 5):\n")
gap_panel3 %>%
  group_by(sector) %>%
  summarise(mean_gap = round(mean(gender_pay_gap), 2), n = n()) %>%
  arrange(desc(mean_gap)) %>%
  head(5) %>%
  print()

# ============================================================================
# PANEL DATA MODELS
# ============================================================================

cat("\n\n🎯 STEP 7: ESTIMATING PANEL DATA MODELS\n")
cat("==========================================\n\n")

# ----------------------------------------------------------------------------
# MODEL 1: AGE × OCCUPATION
# ----------------------------------------------------------------------------

cat("MODEL 1: AGE × OCCUPATION PANEL (2010-2022)\n")
cat("---------------------------------------------\n")

pdata1 <- pdata.frame(gap_panel1, index = c("panel_id", "year"))

# Random Effects
model1_re <- plm(gender_pay_gap ~ young + senior + high_skill + managerial + 
                 factor(year),
                 data = pdata1, model = "random")

cat("Random Effects Model:\n")
print(summary(model1_re))

# Fixed Effects
model1_fe <- plm(gender_pay_gap ~ young + senior + high_skill + managerial + 
                 factor(year),
                 data = pdata1, model = "within")

cat("\n\nFixed Effects Model:\n")
print(summary(model1_fe))

# Hausman Test
cat("\n\nHausman Test (RE vs FE):\n")
tryCatch({
  hausman_test1 <- phtest(model1_fe, model1_re)
  print(hausman_test1)
  
  if(hausman_test1$p.value < 0.05) {
    cat("\n⚠️ Hausman test significant (p < 0.05) → Use FIXED EFFECTS\n")
    model1_final <- model1_fe
    model1_type <- "Fixed Effects"
  } else {
    cat("\n✅ Hausman test not significant (p > 0.05) → Use RANDOM EFFECTS\n")
    model1_final <- model1_re
    model1_type <- "Random Effects"
  }
}, error = function(e) {
  cat("  Hausman test could not be computed, using Random Effects\n")
  model1_final <<- model1_re
  model1_type <<- "Random Effects"
})

# ----------------------------------------------------------------------------
# MODEL 2: SECTOR × OCCUPATION
# ----------------------------------------------------------------------------

cat("\n\n\nMODEL 2: SECTOR × OCCUPATION PANEL (2010-2022)\n")
cat("------------------------------------------------\n")

pdata2 <- pdata.frame(gap_panel2, index = c("panel_id", "year"))

# Random Effects
model2_re <- plm(gender_pay_gap ~ industry + construction + public_sector +
                 high_skill + managerial + factor(year),
                 data = pdata2, model = "random")

cat("Random Effects Model:\n")
print(summary(model2_re))

# Fixed Effects
model2_fe <- plm(gender_pay_gap ~ industry + construction + public_sector +
                 high_skill + managerial + factor(year),
                 data = pdata2, model = "within")

cat("\n\nFixed Effects Model:\n")
print(summary(model2_fe))

# Hausman Test
cat("\n\nHausman Test (RE vs FE):\n")
tryCatch({
  hausman_test2 <- phtest(model2_fe, model2_re)
  print(hausman_test2)
  
  if(hausman_test2$p.value < 0.05) {
    cat("\n⚠️ Hausman test significant (p < 0.05) → Use FIXED EFFECTS\n")
    model2_final <- model2_fe
    model2_type <- "Fixed Effects"
  } else {
    cat("\n✅ Hausman test not significant (p > 0.05) → Use RANDOM EFFECTS\n")
    model2_final <- model2_re
    model2_type <- "Random Effects"
  }
}, error = function(e) {
  cat("  Hausman test could not be computed, using Random Effects\n")
  model2_final <<- model2_re
  model2_type <<- "Random Effects"
})

# ----------------------------------------------------------------------------
# MODEL 3: SECTOR × EDUCATION
# ----------------------------------------------------------------------------

cat("\n\n\nMODEL 3: SECTOR × EDUCATION PANEL (2010-2022)\n")
cat("-----------------------------------------------\n")

pdata3 <- pdata.frame(gap_panel3, index = c("panel_id", "year"))

# Random Effects
model3_re <- plm(gender_pay_gap ~ medium_edu + high_edu + 
                 industry + construction + public_sector +
                 high_edu:industry + factor(year),
                 data = pdata3, model = "random")

cat("Random Effects Model:\n")
print(summary(model3_re))

# Fixed Effects
model3_fe <- plm(gender_pay_gap ~ medium_edu + high_edu + 
                 industry + construction + public_sector +
                 high_edu:industry + factor(year),
                 data = pdata3, model = "within")

cat("\n\nFixed Effects Model:\n")
print(summary(model3_fe))

# Hausman Test
cat("\n\nHausman Test (RE vs FE):\n")
tryCatch({
  hausman_test3 <- phtest(model3_fe, model3_re)
  print(hausman_test3)
  
  if(hausman_test3$p.value < 0.05) {
    cat("\n⚠️ Hausman test significant (p < 0.05) → Use FIXED EFFECTS\n")
    model3_final <- model3_fe
    model3_type <- "Fixed Effects"
  } else {
    cat("\n✅ Hausman test not significant (p > 0.05) → Use RANDOM EFFECTS\n")
    model3_final <- model3_re
    model3_type <- "Random Effects"
  }
}, error = function(e) {
  cat("  Hausman test could not be computed, using Random Effects\n")
  model3_final <<- model3_re
  model3_type <<- "Random Effects"
})

# ============================================================================
# ROBUST STANDARD ERRORS
# ============================================================================

cat("\n\n\n📊 STEP 8: ROBUST STANDARD ERRORS\n")
cat("====================================\n\n")

cat("MODEL 1 (Age × Occupation) - Robust SE:\n")
cat("Model type:", model1_type, "\n")
coeftest(model1_final, vcov = vcovHC(model1_final, type = "HC1"))

cat("\n\nMODEL 2 (Sector × Occupation) - Robust SE:\n")
cat("Model type:", model2_type, "\n")
coeftest(model2_final, vcov = vcovHC(model2_final, type = "HC1"))

cat("\n\nMODEL 3 (Sector × Education) - Robust SE:\n")
cat("Model type:", model3_type, "\n")
coeftest(model3_final, vcov = vcovHC(model3_final, type = "HC1"))

# ============================================================================
# DIAGNOSTIC TESTS
# ============================================================================

cat("\n\n\n🔬 STEP 9: DIAGNOSTIC TESTS\n")
cat("==============================\n\n")

# Serial correlation tests
cat("Serial Correlation Tests:\n")
cat("-------------------------\n")

cat("Model 1:\n")
tryCatch({
  bg_test1 <- pbgtest(model1_final)
  print(bg_test1)
}, error = function(e) {
  cat("  Could not perform test\n")
})

cat("\nModel 2:\n")
tryCatch({
  bg_test2 <- pbgtest(model2_final)
  print(bg_test2)
}, error = function(e) {
  cat("  Could not perform test\n")
})

cat("\nModel 3:\n")
tryCatch({
  bg_test3 <- pbgtest(model3_final)
  print(bg_test3)
}, error = function(e) {
  cat("  Could not perform test\n")
})

# ============================================================================
# LATEX TABLE
# ============================================================================

cat("\n\n📝 STEP 10: CREATING LATEX TABLE\n")
cat("===================================\n")

# Determine which models to use based on Hausman tests
stargazer(model1_final, model2_final, model3_final,
          type = "latex",
          title = "Panel Data Analysis Results (2010-2022)",
          column.labels = c(
            paste0("Age×Occ (", model1_type, ")"),
            paste0("Sector×Occ (", model2_type, ")"),
            paste0("Sector×Edu (", model3_type, ")")
          ),
          dep.var.labels = "Gender Pay Gap (\\%)",
          covariate.labels = c(
            "Young (<30)",
            "Senior (50+)",
            "High-skill occupation",
            "Managerial position",
            "Industry sector",
            "Construction",
            "Public sector",
            "Medium education",
            "High education",
            "High edu × Industry",
            "Year 2014",
            "Year 2018",
            "Year 2022"
          ),
          add.lines = list(
            c("Panel units", 
              length(unique(gap_panel1$panel_id)),
              length(unique(gap_panel2$panel_id)),
              length(unique(gap_panel3$panel_id))),
            c("Countries",
              length(unique(gap_panel1$country)),
              length(unique(gap_panel2$country)),
              length(unique(gap_panel3$country))),
            c("Time periods", "4 (2010-2022)", "4 (2010-2022)", "4 (2010-2022)")
          ),
          omit.stat = c("ser"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("Robust standard errors in parentheses.",
                   "Reference categories: Mid Career (30-49), Low education, Service sectors, Year 2010"),
          notes.align = "l",
          out = "output/tables/panel_results_4years.tex")

cat("  ✅ LaTeX table saved to: output/tables/panel_results_4years.tex\n\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("💾 STEP 11: SAVING RESULTS\n")
cat("============================\n")

write.csv(gap_panel1, "output/data/panel1_age_occupation_4years.csv", row.names = FALSE)
write.csv(gap_panel2, "output/data/panel2_sector_occupation_4years.csv", row.names = FALSE)
write.csv(gap_panel3, "output/data/panel3_sector_education_4years.csv", row.names = FALSE)

cat("  ✅ output/data/panel1_age_occupation_4years.csv\n")
cat("  ✅ output/data/panel2_sector_occupation_4years.csv\n")
cat("  ✅ output/data/panel3_sector_education_4years.csv\n")

# Save model summaries
sink("output/reports/model_summaries_4years.txt")
cat(strrep("=", 80), "\n")
cat("MODEL 1: AGE × OCCUPATION (", model1_type, ")\n")
cat(strrep("=", 80), "\n\n")
print(summary(model1_final))
cat("\nRobust Standard Errors:\n")
print(coeftest(model1_final, vcov = vcovHC(model1_final, type = "HC1")))

cat("\n\n", strrep("=", 80), "\n")
cat("MODEL 2: SECTOR × OCCUPATION (", model2_type, ")\n")
cat(strrep("=", 80), "\n\n")
print(summary(model2_final))
cat("\nRobust Standard Errors:\n")
print(coeftest(model2_final, vcov = vcovHC(model2_final, type = "HC1")))

cat("\n\n", strrep("=", 80), "\n")
cat("MODEL 3: SECTOR × EDUCATION (", model3_type, ")\n")
cat(strrep("=", 80), "\n\n")
print(summary(model3_final))
cat("\nRobust Standard Errors:\n")
print(coeftest(model3_final, vcov = vcovHC(model3_final, type = "HC1")))
sink()


# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n\n✅ ANALYSIS COMPLETE!\n")
cat("======================\n\n")

cat("📊 SUMMARY:\n")
cat("  Panel 1 (Age × Occupation):     ", nrow(gap_panel1), "obs,", 
    length(unique(gap_panel1$panel_id)), "panels,", model1_type, "\n")
cat("  Panel 2 (Sector × Occupation):  ", nrow(gap_panel2), "obs,", 
    length(unique(gap_panel2$panel_id)), "panels,", model2_type, "\n")
cat("  Panel 3 (Sector × Education):   ", nrow(gap_panel3), "obs,", 
    length(unique(gap_panel3$panel_id)), "panels,", model3_type, "\n")
cat("  Time periods: 2010, 2014, 2018, 2022 (4 waves)\n")
cat("  Total countries:", length(unique(c(gap_panel1$country, gap_panel2$country, gap_panel3$country))), "\n")
cat("  Total observations:", nrow(gap_panel1) + nrow(gap_panel2) + nrow(gap_panel3), "\n\n")

cat("📁 OUTPUT FILES:\n")
cat("  - output/data/panel1_age_occupation_4years.csv\n")
cat("  - output/data/panel2_sector_occupation_4years.csv\n")
cat("  - output/data/panel3_sector_education_4years.csv\n")
cat("  - output/tables/panel_results_4years.tex (LaTeX table)\n")
cat("  - output/reports/model_summaries_4years.txt\n\n")

cat("🎓 READY FOR THESIS!\n")


