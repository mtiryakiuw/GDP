# ============================================================================
# PANEL 1 SUPPLEMENTARY ANALYSIS: AGE × OCCUPATION
# Gender Pay Gap Analysis - Age Dynamics Support
# Master's Thesis: Supporting Evidence for Age Dimension
# Years: 2010, 2014, 2018, 2022
# ============================================================================

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)

cat("============================================\n")
cat("PANEL 1: AGE × OCCUPATION ANALYSIS\n")
cat("Supplementary Analysis - Age Dynamics\n")
cat("============================================\n\n")

# ============================================================================
# STEP 1: LOAD PANEL 1 DATA
# ============================================================================

cat("📥 STEP 1: LOADING PANEL 1 DATA (Age × Occupation)...\n")
cat("--------------------------------------------------------\n")

panel1 <- read.csv("output/data/panel1_age_occupation_4years.csv")

cat("  ✅ Panel 1 loaded:", nrow(panel1), "observations\n")
cat("  Countries:", length(unique(panel1$country)), "\n")
cat("  Years:", paste(sort(unique(panel1$year)), collapse = ", "), "\n")
cat("  Age groups:", length(unique(panel1$age_group)), "\n")
cat("  Occupations:", length(unique(panel1$occupation)), "\n")
cat("  Unique panels:", length(unique(panel1$panel_id)), "\n\n")

# ============================================================================
# STEP 2: DESCRIPTIVE STATISTICS
# ============================================================================

cat("📊 STEP 2: DESCRIPTIVE STATISTICS\n")
cat("===================================\n\n")

cat("By Age Group:\n")
age_stats <- panel1 %>%
  group_by(age_group) %>%
  summarise(
    n_obs = n(),
    mean_gap = round(mean(gender_pay_gap, na.rm = TRUE), 2),
    sd_gap = round(sd(gender_pay_gap, na.rm = TRUE), 2),
    median_gap = round(median(gender_pay_gap, na.rm = TRUE), 2)
  )

print(age_stats)

cat("\n\nBy Occupation:\n")
occ_stats <- panel1 %>%
  group_by(occupation) %>%
  summarise(
    n_obs = n(),
    mean_gap = round(mean(gender_pay_gap, na.rm = TRUE), 2),
    sd_gap = round(sd(gender_pay_gap, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mean_gap))

print(occ_stats)

cat("\n\nAge × Occupation Interaction:\n")
age_occ_stats <- panel1 %>%
  group_by(age_group, occupation) %>%
  summarise(
    n_obs = n(),
    mean_gap = round(mean(gender_pay_gap, na.rm = TRUE), 2),
    .groups = 'drop'
  ) %>%
  pivot_wider(names_from = age_group, values_from = mean_gap)

print(age_occ_stats)

# ============================================================================
# STEP 3: CLEAN DATA
# ============================================================================

cat("\n\n🧹 STEP 3: CLEANING DATA\n")
cat("=========================\n\n")

original_n <- nrow(panel1)

panel1_clean <- panel1 %>%
  filter(
    gender_pay_gap >= -20,
    gender_pay_gap <= 80,
    is.finite(gender_pay_gap),
    !is.na(gender_pay_gap)
  )

cleaned_n <- nrow(panel1_clean)
cat(sprintf("Original: %d → Cleaned: %d (removed %d, %.1f%%)\n", 
            original_n, cleaned_n, original_n - cleaned_n,
            100 * (original_n - cleaned_n) / original_n))

# ============================================================================
# STEP 4: PANEL MODEL
# ============================================================================

cat("\n\n🎯 STEP 4: ESTIMATING PANEL MODEL\n")
cat("===================================\n\n")

# Create panel data
pdata1 <- pdata.frame(panel1_clean, index = c("panel_id", "year"))

cat("Panel dimensions:\n")
cat("  Observations:", nrow(pdata1), "\n")
cat("  Panels:", length(unique(panel1_clean$panel_id)), "\n")
cat("  Time periods:", length(unique(panel1_clean$year)), "\n\n")

# Random Effects Model
cat("Random Effects Model:\n")
cat("----------------------\n")

model1_re <- plm(
  gender_pay_gap ~ 
    young + senior + 
    high_skill + managerial + 
    factor(year),
  data = pdata1, 
  model = "random"
)

print(summary(model1_re))

# Robust standard errors
cat("\n\nRobust Standard Errors:\n")
cat("------------------------\n")
robust1 <- coeftest(model1_re, vcov = vcovHC(model1_re, type = "HC1"))
print(robust1)

# ============================================================================
# STEP 5: AGE-OCCUPATION INTERACTION MODEL
# ============================================================================

cat("\n\n\n🔍 STEP 5: AGE-OCCUPATION INTERACTION MODEL\n")
cat("=============================================\n\n")

model1_interaction <- plm(
  gender_pay_gap ~ 
    young + senior + 
    high_skill + managerial +
    young:high_skill + senior:high_skill +
    young:managerial + senior:managerial +
    factor(year),
  data = pdata1, 
  model = "random"
)

print(summary(model1_interaction))

cat("\n\nRobust Standard Errors (Interaction Model):\n")
cat("--------------------------------------------\n")
robust1_int <- coeftest(model1_interaction, vcov = vcovHC(model1_interaction, type = "HC1"))
print(robust1_int)

# ============================================================================
# STEP 6: KEY FINDINGS
# ============================================================================

cat("\n\n\n📋 STEP 6: KEY FINDINGS ON AGE DYNAMICS\n")
cat("=========================================\n\n")

coefs <- robust1[, "Estimate"]
pvals <- robust1[, "Pr(>|t|)"]

# Young workers
if("young" %in% names(coefs)) {
  young_coef <- coefs["young"]
  young_pval <- pvals["young"]
  
  cat("Young Workers (<30):\n")
  cat(sprintf("  Coefficient: %.3f %s\n", 
              young_coef,
              ifelse(young_pval < 0.001, "***", 
                     ifelse(young_pval < 0.01, "**", 
                            ifelse(young_pval < 0.05, "*", "")))))
  
  if(young_pval < 0.05) {
    if(young_coef < 0) {
      cat("  ✅ Young workers have LOWER gender pay gaps\n")
    } else {
      cat("  ⚠️ Young workers have HIGHER gender pay gaps\n")
    }
  } else {
    cat("  ❌ No significant difference for young workers\n")
  }
}

# Senior workers
if("senior" %in% names(coefs)) {
  senior_coef <- coefs["senior"]
  senior_pval <- pvals["senior"]
  
  cat("\nSenior Workers (50+):\n")
  cat(sprintf("  Coefficient: %.3f %s\n", 
              senior_coef,
              ifelse(senior_pval < 0.001, "***", 
                     ifelse(senior_pval < 0.01, "**", 
                            ifelse(senior_pval < 0.05, "*", "")))))
  
  if(senior_pval < 0.05) {
    if(senior_coef > 0) {
      cat("  ⚠️ Senior workers have HIGHER gender pay gaps\n")
    } else {
      cat("  ✅ Senior workers have LOWER gender pay gaps\n")
    }
  } else {
    cat("  ❌ No significant difference for senior workers\n")
  }
}

cat("\n\nInterpretation:\n")
cat("----------------\n")
cat("This analysis shows how gender pay gaps vary across age groups,\n")
cat("providing complementary evidence to the main sector-occupation analysis.\n")
cat("Age dynamics reveal cohort effects and career progression patterns.\n")

# ============================================================================
# STEP 7: SAVE RESULTS
# ============================================================================

cat("\n\n\n💾 STEP 7: SAVING RESULTS\n")
cat("==========================\n\n")

# Save cleaned data
write.csv(panel1_clean, "output/data/panel1_age_occupation_cleaned.csv", row.names = FALSE)
cat("✅ output/data/panel1_age_occupation_cleaned.csv\n")

# Save age statistics
write.csv(age_stats, "output/data/panel1_age_statistics.csv", row.names = FALSE)
cat("✅ output/data/panel1_age_statistics.csv\n")

# Save model summaries
sink("output/reports/panel1_model_summaries.txt")
cat(strrep("=", 80), "\n")
cat("PANEL 1: AGE × OCCUPATION ANALYSIS (SUPPLEMENTARY)\n")
cat(strrep("=", 80), "\n\n")

cat("MAIN MODEL: Random Effects\n")
cat(strrep("-", 80), "\n\n")
print(summary(model1_re))
cat("\n\nRobust Standard Errors:\n")
print(coeftest(model1_re, vcov = vcovHC(model1_re, type = "HC1")))

cat("\n\n", strrep("=", 80), "\n")
cat("INTERACTION MODEL: Age × Occupation\n")
cat(strrep("-", 80), "\n\n")
print(summary(model1_interaction))
cat("\n\nRobust Standard Errors:\n")
print(coeftest(model1_interaction, vcov = vcovHC(model1_interaction, type = "HC1")))

sink()
cat("✅ output/reports/panel1_model_summaries.txt\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n\n✅ PANEL 1 SUPPLEMENTARY ANALYSIS COMPLETE!\n")
cat("============================================\n\n")

cat("📊 SUMMARY:\n")
cat("  Cleaned observations:  ", nrow(panel1_clean), "\n")
cat("  Unique panels:         ", length(unique(panel1_clean$panel_id)), "\n")
cat("  Countries:             ", length(unique(panel1_clean$country)), "\n")
cat("  Years:                 ", paste(sort(unique(panel1_clean$year)), collapse = ", "), "\n")
cat("  Age groups:             3 (Young, Mid-Career, Senior)\n")
cat("  Occupations:            9 (ISCO-08)\n")
cat("  Average gap:           ", round(mean(panel1_clean$gender_pay_gap, na.rm = TRUE), 2), "%\n\n")

cat("📁 OUTPUT FILES:\n")
cat("  - output/data/panel1_age_occupation_cleaned.csv\n")
cat("  - output/data/panel1_age_statistics.csv\n")
cat("  - output/reports/panel1_model_summaries.txt\n\n")

cat("🎯 This analysis provides supporting evidence for age dynamics,\n")
cat("   complementing the main sector-occupation analysis (Panel 2).\n\n")
