# ============================================================================
# PANEL 2 MAIN ANALYSIS: SECTOR × OCCUPATION
# Gender Pay Gap Analysis with 18 Detailed Sectors
# Master's Thesis: Sectoral and Occupational Determinants
# Years: 2010, 2014, 2018, 2022
# ============================================================================

# Start capturing all output to file
sink("output/reports/panel2_analysis_full_output.txt", split = TRUE)

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(ggplot2)
library(gridExtra)
library(diptest)
library(strucchange)

cat("============================================\n")
cat("PANEL 2: SECTOR × OCCUPATION ANALYSIS\n")
cat("18 Detailed Sectors × 9 Occupations\n")
cat("Main Analysis for Thesis\n")
cat("============================================\n\n")

# ============================================================================
# STEP 1: LOAD PANEL 2 DATA
# ============================================================================

cat("📥 STEP 1: LOADING PANEL 2 DATA (Sector × Occupation)...\n")
cat("----------------------------------------------------------\n")

panel2 <- read.csv("output/data/panel2_sector_occupation_4years.csv")

cat("  ✅ Panel 2 loaded:", nrow(panel2), "observations\n")
cat("  Countries:", length(unique(panel2$country)), "\n")
cat("  Years:", paste(sort(unique(panel2$year)), collapse = ", "), "\n")
cat("  Sectors:", length(unique(panel2$sector)), "\n")
cat("  Occupations:", length(unique(panel2$occupation)), "\n")
cat("  Unique panels:", length(unique(panel2$panel_id)), "\n\n")

# ============================================================================
# STEP 2: DATA QUALITY ASSESSMENT
# ============================================================================

cat("🔍 STEP 2: DATA QUALITY ASSESSMENT\n")
cat("====================================\n\n")

# Basic statistics
cat("Gender Pay Gap Summary:\n")
print(summary(panel2$gender_pay_gap))

cat("\n📊 Observations by Year:\n")
panel2 %>%
  group_by(year) %>%
  summarise(n = n(), mean_gap = round(mean(gender_pay_gap, na.rm = TRUE), 2)) %>%
  print()

cat("\n📊 Observations by Sector (18 Detailed):\n")
sector_summary <- panel2 %>%
  group_by(sector) %>%
  summarise(
    n_obs = n(),
    mean_gap = round(mean(gender_pay_gap, na.rm = TRUE), 2),
    sd_gap = round(sd(gender_pay_gap, na.rm = TRUE), 2),
    countries = n_distinct(country)
  ) %>%
  arrange(desc(mean_gap))

print(sector_summary)

cat("\n📊 Observations by Occupation (9 ISCO levels):\n")
occupation_summary <- panel2 %>%
  group_by(occupation) %>%
  summarise(
    n_obs = n(),
    mean_gap = round(mean(gender_pay_gap, na.rm = TRUE), 2),
    sd_gap = round(sd(gender_pay_gap, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mean_gap))

print(occupation_summary)

# Check for outliers
outliers <- panel2 %>%
  filter(gender_pay_gap < -20 | gender_pay_gap > 80)

cat("\n⚠️ Potential outliers (gap < -20% or > 80%):", nrow(outliers), "\n")

if(nrow(outliers) > 0) {
  cat("Sample of outliers:\n")
  print(head(outliers %>% select(country, year, sector, occupation, gender_pay_gap)))
}

# ============================================================================
# STEP 3: CLEAN OUTLIERS
# ============================================================================

cat("\n\n🧹 STEP 3: CLEANING EXTREME OUTLIERS\n")
cat("======================================\n\n")

original_n <- nrow(panel2)

# Remove extreme outliers
panel2_clean <- panel2 %>%
  filter(
    gender_pay_gap >= -20,
    gender_pay_gap <= 80,
    is.finite(gender_pay_gap),
    !is.na(gender_pay_gap)
  )

cleaned_n <- nrow(panel2_clean)
removed_n <- original_n - cleaned_n

cat(sprintf("Original observations: %d\n", original_n))
cat(sprintf("Cleaned observations: %d\n", cleaned_n))
cat(sprintf("Removed outliers: %d (%.1f%%)\n\n", removed_n, 100 * removed_n / original_n))

cat("Cleaned data summary:\n")
print(summary(panel2_clean$gender_pay_gap))

# ============================================================================
# STEP 4: PANEL STRUCTURE VALIDATION
# ============================================================================

cat("\n\n🔍 STEP 4: PANEL STRUCTURE VALIDATION\n")
cat("=======================================\n\n")

# Check panel balance
panel_balance <- panel2_clean %>%
  group_by(panel_id) %>%
  summarise(n_years = n(), .groups = 'drop')

cat("Panel balance:\n")
print(table(panel_balance$n_years))

cat("\nPanel completeness:\n")
cat("  Balanced panels (4 years):", sum(panel_balance$n_years == 4), "\n")
cat("  3-year panels:", sum(panel_balance$n_years == 3), "\n")
cat("  2-year panels:", sum(panel_balance$n_years == 2), "\n")
cat("  1-year panels:", sum(panel_balance$n_years == 1), "\n")

# ============================================================================
# STEP 5: DESCRIPTIVE STATISTICS BY KEY DIMENSIONS
# ============================================================================

cat("\n\n📊 STEP 5: DESCRIPTIVE STATISTICS\n")
cat("===================================\n\n")

cat("By Sector Group:\n")
sector_group_stats <- panel2_clean %>%
  mutate(
    sector_group = case_when(
      industry == 1 ~ "Industry",
      construction == 1 ~ "Construction",
      services == 1 ~ "Services",
      public_sector == 1 ~ "Public Sector",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(sector_group) %>%
  summarise(
    n_obs = n(),
    mean_gap = round(mean(gender_pay_gap, na.rm = TRUE), 2),
    sd_gap = round(sd(gender_pay_gap, na.rm = TRUE), 2),
    min_gap = round(min(gender_pay_gap, na.rm = TRUE), 2),
    max_gap = round(max(gender_pay_gap, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mean_gap))

print(sector_group_stats)

cat("\n\nBy Occupation Type:\n")
occ_stats <- panel2_clean %>%
  mutate(
    occ_type = case_when(
      managerial == 1 ~ "Managers",
      high_skill == 1 ~ "High-Skill (Non-Mgr)",
      TRUE ~ "Other Occupations"
    )
  ) %>%
  group_by(occ_type) %>%
  summarise(
    n_obs = n(),
    mean_gap = round(mean(gender_pay_gap, na.rm = TRUE), 2),
    sd_gap = round(sd(gender_pay_gap, na.rm = TRUE), 2)
  ) %>%
  arrange(desc(mean_gap))

print(occ_stats)

cat("\n\nTemporal Trends:\n")
temporal_stats <- panel2_clean %>%
  group_by(year) %>%
  summarise(
    n_obs = n(),
    mean_gap = round(mean(gender_pay_gap, na.rm = TRUE), 2),
    sd_gap = round(sd(gender_pay_gap, na.rm = TRUE), 2),
    median_gap = round(median(gender_pay_gap, na.rm = TRUE), 2)
  )

print(temporal_stats)

# Calculate annual change rate
if(nrow(temporal_stats) >= 2) {
  first_year <- temporal_stats$year[1]
  last_year <- temporal_stats$year[nrow(temporal_stats)]
  first_gap <- temporal_stats$mean_gap[1]
  last_gap <- temporal_stats$mean_gap[nrow(temporal_stats)]
  
  years_diff <- last_year - first_year
  total_change <- last_gap - first_gap
  annual_change <- total_change / (years_diff / 4)  # Per 4-year period
  
  cat(sprintf("\nTrend analysis (%d-%d):\n", first_year, last_year))
  cat(sprintf("  Total change: %.2f percentage points\n", total_change))
  cat(sprintf("  Annual change rate: %.2f pp per 4-year period\n", annual_change))
}

# ============================================================================
# STEP 6: PREPARE PANEL DATA STRUCTURE
# ============================================================================

cat("\n\n🔧 STEP 6: PREPARING PANEL DATA STRUCTURE\n")
cat("===========================================\n\n")

# Create pdata.frame
pdata2 <- pdata.frame(panel2_clean, index = c("panel_id", "year"))

cat("Panel dimensions:\n")
cat("  N (observations):", nrow(pdata2), "\n")
cat("  T (time periods):", length(unique(panel2_clean$year)), "\n")
cat("  n (panels):", length(unique(panel2_clean$panel_id)), "\n\n")

# ============================================================================
# STEP 7: PANEL DATA MODELS
# ============================================================================

cat("🎯 STEP 7: ESTIMATING PANEL DATA MODELS\n")
cat("=========================================\n\n")

# Model 1: Random Effects (Primary Specification)
cat("MODEL 1: Random Effects (Primary)\n")
cat("-----------------------------------\n")

model_re <- plm(
  gender_pay_gap ~ 
    industry + construction + public_sector +
    high_skill + managerial + 
    factor(year),
  data = pdata2, 
  model = "random"
)

print(summary(model_re))

# Model 2: Fixed Effects
cat("\n\nMODEL 2: Fixed Effects\n")
cat("------------------------\n")

model_fe <- plm(
  gender_pay_gap ~ 
    industry + construction + public_sector +
    high_skill + managerial + 
    factor(year),
  data = pdata2, 
  model = "within"
)

print(summary(model_fe))

# Hausman Test
cat("\n\nHausman Test (FE vs RE):\n")
cat("-------------------------\n")

hausman_test <- tryCatch({
  phtest(model_fe, model_re)
}, error = function(e) {
  cat("Hausman test could not be computed:", e$message, "\n")
  NULL
})

if(!is.null(hausman_test)) {
  print(hausman_test)
  
  if(hausman_test$p.value < 0.05) {
    cat("\n⚠️ Hausman test significant (p < 0.05) → Fixed Effects preferred\n")
    model_primary <- model_fe
    model_type <- "Fixed Effects"
  } else {
    cat("\n✅ Hausman test not significant (p > 0.05) → Random Effects preferred\n")
    model_primary <- model_re
    model_type <- "Random Effects"
  }
} else {
  cat("\n✅ Using Random Effects as primary model\n")
  model_primary <- model_re
  model_type <- "Random Effects"
}

# Model 3: Interaction Model (Sector × Occupation)
cat("\n\n\nMODEL 3: Sector-Occupation Interactions\n")
cat("-----------------------------------------\n")

model_interaction <- plm(
  gender_pay_gap ~ 
    industry + construction + public_sector +
    high_skill + managerial +
    industry:high_skill + industry:managerial +
    public_sector:high_skill + public_sector:managerial +
    factor(year),
  data = pdata2, 
  model = "random"
)

print(summary(model_interaction))

# ============================================================================
# STEP 8: ROBUST STANDARD ERRORS
# ============================================================================

cat("\n\n\n📊 STEP 8: ROBUST STANDARD ERRORS\n")
cat("===================================\n\n")

cat("Primary Model (", model_type, ") - Robust SE:\n")
cat(strrep("-", 50), "\n")
robust_results <- coeftest(model_primary, vcov = vcovHC(model_primary, type = "HC1"))
print(robust_results)

cat("\n\nInteraction Model - Robust SE:\n")
cat(strrep("-", 50), "\n")
robust_interaction <- coeftest(model_interaction, vcov = vcovHC(model_interaction, type = "HC1"))
print(robust_interaction)

# ============================================================================
# JOINT SIGNIFICANCE TEST FOR INTERACTION TERMS
# ============================================================================

cat("\n\n\n🔬 JOINT SIGNIFICANCE TEST FOR INTERACTION TERMS\n")
cat("==================================================\n\n")

# Test if all 4 interaction terms are jointly significant
# This tests whether the interaction model is superior to additive model

cat("Testing H0: All interaction coefficients = 0\n")
cat("(i.e., additive model is sufficient)\n\n")

# Load car package for Wald test
if(!require(car, quietly = TRUE)) {
  install.packages("car")
  library(car)
}

# Define interaction terms to test
interaction_terms <- c("industry:high_skill", "industry:managerial", 
                       "public_sector:high_skill", "public_sector:managerial")

# Wald test with cluster-robust vcov
wald_result <- tryCatch({
  linearHypothesis(model_interaction, 
                   interaction_terms,
                   vcov = vcovHC(model_interaction, type="HC1"))
}, error = function(e) {
  cat("⚠️ Wald test failed:", e$message, "\n")
  NULL
})

if(!is.null(wald_result)) {
  print(wald_result)
  
  # Extract F-statistic and p-value (handle different output formats)
  if("F" %in% names(wald_result)) {
    f_stat <- wald_result$F[2]
    p_value <- wald_result$`Pr(>F)`[2]
  } else if("Chisq" %in% names(wald_result)) {
    f_stat <- wald_result$Chisq[2]
    p_value <- wald_result$`Pr(>Chisq)`[2]
  } else {
    # Manual extraction from print output
    f_stat <- NA
    p_value <- NA
  }
  
  df1 <- wald_result$Df[2]
  df2 <- wald_result$Res.Df[2]
  
  cat("\n📊 JOINT TEST SUMMARY:\n")
  cat("======================\n")
  
  if(!is.na(f_stat)) {
    cat(sprintf("F-statistic: %.3f\n", f_stat))
    cat(sprintf("Degrees of freedom: %d, %d\n", df1, df2))
    cat(sprintf("p-value: %s\n", format.pval(p_value, digits = 4)))
    
    if(p_value < 0.001) {
      cat("\n✅ RESULT: Strongly reject H0 (p < 0.001)\n")
      cat("→ Interaction terms are jointly highly significant\n")
      cat("→ Interactive specification is statistically superior to additive model\n")
    } else if(p_value < 0.05) {
      cat("\n✅ RESULT: Reject H0 (p < 0.05)\n")
      cat("→ Interaction terms are jointly significant\n")
    } else {
      cat("\n⚠️ RESULT: Cannot reject H0 (p >= 0.05)\n")
      cat("→ Interaction terms not jointly significant\n")
    }
  } else {
    cat("F-statistic: See table above\n")
    cat(sprintf("Degrees of freedom: %d, %d\n", df1, df2))
    cat("\nPlease check the Wald test output above for F-statistic and p-value.\n")
  }
  
  cat("\nINTERPRETATION:\n")
  cat("This Wald test evaluates whether sector-occupation interactions\n")
  cat("provide significant explanatory power beyond additive main effects.\n")
  cat("Rejection of H0 confirms that occupational wage gaps differ\n")
  cat("systematically across sectoral contexts, validating Research Question 2.\n\n")
}

# ============================================================================
# STEP 9: DIAGNOSTIC TESTS
# ============================================================================

cat("\n\n\n🔬 STEP 9: DIAGNOSTIC TESTS\n")
cat("============================\n\n")

# Serial Correlation Test (Breusch-Godfrey)
cat("1. Serial Correlation Test (Breusch-Godfrey/Wooldridge):\n")
cat("----------------------------------------------------------\n")
serial_test <- tryCatch({
  pbgtest(model_primary)
}, error = function(e) {
  cat("Serial correlation test failed:", e$message, "\n")
  NULL
})

if(!is.null(serial_test)) {
  print(serial_test)
  cat(sprintf("\n📊 Test statistic: chisq = %.3f, df = %d, p-value = %s\n",
              serial_test$statistic,
              serial_test$parameter,
              format.pval(serial_test$p.value, digits = 4)))
  if(serial_test$p.value < 0.05) {
    cat("⚠️ Serial correlation detected (p < 0.05)\n")
  } else {
    cat("✅ No serial correlation detected\n")
  }
}

# Heteroskedasticity Test (Breusch-Pagan)
cat("\n\n2. Heteroskedasticity Test (Breusch-Pagan):\n")
cat("--------------------------------------------\n")
bp_test <- tryCatch({
  # Convert to lm for bptest
  lm_model <- lm(
    gender_pay_gap ~ 
      industry + construction + services + public_sector +
      high_skill + managerial + 
      factor(year),
    data = panel2_clean
  )
  bptest(lm_model)
}, error = function(e) {
  cat("Breusch-Pagan test failed:", e$message, "\n")
  NULL
})

if(!is.null(bp_test)) {
  print(bp_test)
  cat(sprintf("\n📊 Test statistic: BP = %.3f, df = %d, p-value = %s\n",
              bp_test$statistic,
              bp_test$parameter,
              format.pval(bp_test$p.value, digits = 4)))
  if(bp_test$p.value < 0.05) {
    cat("⚠️ Heteroskedasticity detected (p < 0.05)\n")
    cat("   → Using cluster-robust standard errors (HC1)\n")
  } else {
    cat("✅ No significant heteroskedasticity detected\n")
  }
}

# Model fit statistics
cat("\n\n3. Model Fit Statistics:\n")
cat("-------------------------\n")
cat("Primary Model (", model_type, "):\n")
cat("  R-squared (within):", round(summary(model_primary)$r.squared["rsq"], 4), "\n")
if(model_type == "Random Effects") {
  cat("  R-squared (between):", round(summary(model_primary)$r.squared["between"], 4), "\n")
  cat("  R-squared (overall):", round(summary(model_primary)$r.squared["adjrsq"], 4), "\n")
}

# Distributional Tests
cat("\n\n4. Distributional Tests:\n")
cat("-------------------------\n")

# Hartigan's Dip Test for multimodality
cat("\nHartigan's Dip Test (Multimodality):\n")
cat("-------------------------------------\n")

sector_groups <- list(
  "Industry" = panel2_clean$gender_pay_gap[panel2_clean$industry == 1],
  "Construction" = panel2_clean$gender_pay_gap[panel2_clean$construction == 1],
  "Services" = panel2_clean$gender_pay_gap[panel2_clean$services == 1],
  "Public Sector" = panel2_clean$gender_pay_gap[panel2_clean$public_sector == 1]
)

for(sector_name in names(sector_groups)) {
  sector_data <- sector_groups[[sector_name]]
  if(length(sector_data) > 10) {
    dip_result <- tryCatch({
      dip.test(sector_data)
    }, error = function(e) {
      cat(sprintf("  %s: Test failed\n", sector_name))
      NULL
    })
    
    if(!is.null(dip_result)) {
      cat(sprintf("  %s: D = %.4f, p-value = %.3f %s\n",
                  sector_name,
                  dip_result$statistic,
                  dip_result$p.value,
                  ifelse(dip_result$p.value < 0.05, "(multimodal)", "(unimodal)")))
    }
  }
}

# Kolmogorov-Smirnov Test for distributional equality
cat("\n\nKolmogorov-Smirnov Test (Distributional Equality):\n")
cat("---------------------------------------------------\n")

ks_comparisons <- list(
  c("Industry", "Public Sector"),
  c("Industry", "Services"),
  c("Services", "Public Sector")
)

for(comp in ks_comparisons) {
  group1 <- sector_groups[[comp[1]]]
  group2 <- sector_groups[[comp[2]]]
  
  if(length(group1) > 10 & length(group2) > 10) {
    ks_result <- tryCatch({
      ks.test(group1, group2)
    }, error = function(e) {
      NULL
    })
    
    if(!is.null(ks_result)) {
      cat(sprintf("  %s vs %s: D = %.4f, p-value = %s %s\n",
                  comp[1], comp[2],
                  ks_result$statistic,
                  format.pval(ks_result$p.value, digits = 4),
                  ifelse(ks_result$p.value < 0.05, "(different distributions)", "(similar distributions)")))
    }
  }
}

# Structural Break Test (Chow Test)
cat("\n\n5. Structural Break Test (Chow Test):\n")
cat("---------------------------------------\n")

chow_result <- tryCatch({
  # Test for structural break at 2014 (midpoint)
  fs_stats <- sctest(model_primary, type = "Chow", point = 2)
  fs_stats
}, error = function(e) {
  cat("Chow test failed:", e$message, "\n")
  NULL
})

if(!is.null(chow_result)) {
  print(chow_result)
  cat(sprintf("\n📊 F-statistic = %.3f, p-value = %s\n",
              chow_result$statistic,
              format.pval(chow_result$p.value, digits = 4)))
  if(chow_result$p.value < 0.05) {
    cat("⚠️ Structural break detected (p < 0.05)\n")
  } else {
    cat("✅ No significant structural break\n")
  }
}

# ============================================================================
# STEP 10: HYPOTHESIS TESTING
# ============================================================================

cat("\n\n\n🎯 STEP 10: HYPOTHESIS TESTING\n")
cat("================================\n\n")

cat("HYPOTHESIS 1: Sectoral Effects\n")
cat("-------------------------------\n")
cat("H1: Industry & Construction sectors show larger gender pay gaps\n\n")

# Extract coefficients
coefs <- robust_results[, "Estimate"]
pvals <- robust_results[, "Pr(>|t|)"]

# Test Industry
if("industry" %in% names(coefs)) {
  industry_coef <- coefs["industry"]
  industry_pval <- pvals["industry"]
  cat("Industry sector coefficient:", round(industry_coef, 3), 
      ifelse(industry_pval < 0.001, "***", ifelse(industry_pval < 0.01, "**", ifelse(industry_pval < 0.05, "*", ""))), "\n")
  
  if(industry_pval < 0.05 & industry_coef > 0) {
    cat("✅ H1 SUPPORTED for Industry: Significantly higher gap\n")
  } else {
    cat("❌ H1 NOT SUPPORTED for Industry\n")
  }
}

# Test Construction
if("construction" %in% names(coefs)) {
  construction_coef <- coefs["construction"]
  construction_pval <- pvals["construction"]
  cat("\nConstruction sector coefficient:", round(construction_coef, 3),
      ifelse(construction_pval < 0.001, "***", ifelse(construction_pval < 0.01, "**", ifelse(construction_pval < 0.05, "*", ""))), "\n")
  
  if(construction_pval < 0.05 & construction_coef > 0) {
    cat("✅ H1 SUPPORTED for Construction: Significantly higher gap\n")
  } else {
    cat("❌ H1 NOT SUPPORTED for Construction\n")
  }
}

# Test Public Sector
if("public_sector" %in% names(coefs)) {
  public_coef <- coefs["public_sector"]
  public_pval <- pvals["public_sector"]
  cat("\nPublic sector coefficient:", round(public_coef, 3),
      ifelse(public_pval < 0.001, "***", ifelse(public_pval < 0.01, "**", ifelse(public_pval < 0.05, "*", ""))), "\n")
  
  if(public_pval < 0.05 & public_coef < 0) {
    cat("✅ H1 SUPPORTED for Public Sector: Significantly lower gap\n")
  } else {
    cat("❌ H1 NOT SUPPORTED for Public Sector\n")
  }
}

cat("\n\nHYPOTHESIS 2: Occupational Hierarchy Effects\n")
cat("----------------------------------------------\n")
cat("H2: High-skill and managerial positions show distinct gap patterns\n\n")

# Test High-Skill
if("high_skill" %in% names(coefs)) {
  highskill_coef <- coefs["high_skill"]
  highskill_pval <- pvals["high_skill"]
  cat("High-skill occupation coefficient:", round(highskill_coef, 3),
      ifelse(highskill_pval < 0.001, "***", ifelse(highskill_pval < 0.01, "**", ifelse(highskill_pval < 0.05, "*", ""))), "\n")
  
  if(highskill_pval < 0.05) {
    cat("✅ H2 SUPPORTED for High-Skill: Significant effect detected\n")
  } else {
    cat("❌ H2 NOT SUPPORTED for High-Skill\n")
  }
}

# Test Managerial
if("managerial" %in% names(coefs)) {
  manager_coef <- coefs["managerial"]
  manager_pval <- pvals["managerial"]
  cat("\nManagerial position coefficient:", round(manager_coef, 3),
      ifelse(manager_pval < 0.001, "***", ifelse(manager_pval < 0.01, "**", ifelse(manager_pval < 0.05, "*", ""))), "\n")
  
  if(manager_pval < 0.05) {
    cat("✅ H2 SUPPORTED for Managerial: Significant effect detected\n")
  } else {
    cat("❌ H2 NOT SUPPORTED for Managerial\n")
  }
}

cat("\n\nHYPOTHESIS 3: Temporal Convergence\n")
cat("-----------------------------------\n")
cat("H3: Gender pay gaps decrease over time\n\n")

# Check year effects
year_vars <- grep("factor\\(year\\)", names(coefs), value = TRUE)
if(length(year_vars) > 0) {
  cat("Year fixed effects:\n")
  for(yvar in year_vars) {
    cat(sprintf("  %s: %.3f %s\n", 
                yvar, 
                coefs[yvar],
                ifelse(pvals[yvar] < 0.001, "***", ifelse(pvals[yvar] < 0.01, "**", ifelse(pvals[yvar] < 0.05, "*", "")))))
  }
  
  # Test if trend is negative
  year_coefs <- coefs[year_vars]
  if(all(year_coefs < 0) & all(pvals[year_vars] < 0.05)) {
    cat("\n✅ H3 SUPPORTED: Consistent negative time trend\n")
  } else if(mean(year_coefs) < 0) {
    cat("\n⚠️ H3 PARTIALLY SUPPORTED: Negative trend but not all significant\n")
  } else {
    cat("\n❌ H3 NOT SUPPORTED: No consistent negative trend\n")
  }
}

# ============================================================================
# STEP 11: ROBUSTNESS CHECKS
# ============================================================================

cat("\n\n\n🔄 STEP 11: ROBUSTNESS CHECKS\n")
cat("==============================\n\n")

# Robustness 1: Balanced Panel Only
cat("ROBUSTNESS 1: Balanced Panel Analysis\n")
cat("---------------------------------------\n")

balanced_panels <- panel_balance %>%
  filter(n_years == 4) %>%
  pull(panel_id)

panel2_balanced <- panel2_clean %>%
  filter(panel_id %in% balanced_panels)

cat("Balanced panel observations:", nrow(panel2_balanced), "\n")

if(nrow(panel2_balanced) > 100) {
  pdata_balanced <- pdata.frame(panel2_balanced, index = c("panel_id", "year"))
  
  model_balanced <- tryCatch({
    plm(
      gender_pay_gap ~ 
        industry + construction + public_sector +
        high_skill + managerial + 
        factor(year),
      data = pdata_balanced, 
      model = "random"
    )
  }, error = function(e) {
    cat("Balanced panel model failed:", e$message, "\n")
    NULL
  })
  
  if(!is.null(model_balanced)) {
    cat("\nBalanced Panel Model Results:\n")
    print(coeftest(model_balanced, vcov = vcovHC(model_balanced, type = "HC1")))
  }
} else {
  cat("⚠️ Insufficient observations for balanced panel analysis\n")
}

# Robustness 2: Winsorized Dependent Variable
cat("\n\nROBUSTNESS 2: Winsorized Gender Pay Gap\n")
cat("-----------------------------------------\n")

# Winsorize at 5th and 95th percentiles
p05 <- quantile(panel2_clean$gender_pay_gap, 0.05, na.rm = TRUE)
p95 <- quantile(panel2_clean$gender_pay_gap, 0.95, na.rm = TRUE)

panel2_winsor <- panel2_clean %>%
  mutate(
    gender_pay_gap_winsor = case_when(
      gender_pay_gap < p05 ~ p05,
      gender_pay_gap > p95 ~ p95,
      TRUE ~ gender_pay_gap
    )
  )

cat(sprintf("Winsorized at 5th (%.2f) and 95th (%.2f) percentiles\n", p05, p95))

pdata_winsor <- pdata.frame(panel2_winsor, index = c("panel_id", "year"))

model_winsor <- tryCatch({
  plm(
    gender_pay_gap_winsor ~ 
      industry + construction + public_sector +
      high_skill + managerial + 
      factor(year),
    data = pdata_winsor, 
    model = "random"
  )
}, error = function(e) {
  cat("Winsorized model failed:", e$message, "\n")
  NULL
})

if(!is.null(model_winsor)) {
  cat("\nWinsorized Model Results:\n")
  print(coeftest(model_winsor, vcov = vcovHC(model_winsor, type = "HC1")))
}

# ============================================================================
# STEP 12: DETAILED SECTORAL ANALYSIS (18 Sectors)
# ============================================================================

cat("\n\n\n🏭 STEP 12: DETAILED SECTORAL ANALYSIS (18 Sectors)\n")
cat("====================================================\n\n")

# Analyze each of the 18 detailed sectors
sector_detail <- panel2_clean %>%
  group_by(sector) %>%
  summarise(
    n_obs = n(),
    n_countries = n_distinct(country),
    n_occupations = n_distinct(occupation),
    mean_gap = mean(gender_pay_gap, na.rm = TRUE),
    median_gap = median(gender_pay_gap, na.rm = TRUE),
    sd_gap = sd(gender_pay_gap, na.rm = TRUE),
    min_gap = min(gender_pay_gap, na.rm = TRUE),
    max_gap = max(gender_pay_gap, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  arrange(desc(mean_gap))

cat("Top 10 Sectors with Highest Gender Pay Gaps:\n")
print(head(sector_detail, 10))

cat("\n\nBottom 10 Sectors with Lowest Gender Pay Gaps:\n")
print(tail(sector_detail, 10))

# ============================================================================
# STEP 13: SAVE RESULTS
# ============================================================================

cat("\n\n\n💾 STEP 13: SAVING RESULTS\n")
cat("===========================\n\n")

# Save cleaned data
write.csv(panel2_clean, "output/data/panel2_sector_occupation_cleaned.csv", row.names = FALSE)
cat("✅ output/data/panel2_sector_occupation_cleaned.csv\n")

# Save sector analysis
write.csv(sector_detail, "output/data/panel2_detailed_sector_analysis.csv", row.names = FALSE)
cat("✅ output/data/panel2_detailed_sector_analysis.csv\n")

# Save model summaries
sink("output/reports/panel2_model_summaries.txt")
cat(strrep("=", 80), "\n")
cat("PANEL 2: SECTOR × OCCUPATION ANALYSIS\n")
cat("18 Detailed Sectors × 9 Occupations\n")
cat(strrep("=", 80), "\n\n")

cat("PRIMARY MODEL (", model_type, ")\n")
cat(strrep("-", 80), "\n\n")
print(summary(model_primary))
cat("\n\nRobust Standard Errors:\n")
print(coeftest(model_primary, vcov = vcovHC(model_primary, type = "HC1")))

cat("\n\n", strrep("=", 80), "\n")
cat("INTERACTION MODEL\n")
cat(strrep("-", 80), "\n\n")
print(summary(model_interaction))
cat("\n\nRobust Standard Errors:\n")
print(coeftest(model_interaction, vcov = vcovHC(model_interaction, type = "HC1")))

sink()
cat("✅ output/reports/panel2_model_summaries.txt\n")

# ============================================================================
# STEP 14: CREATE LATEX TABLE
# ============================================================================

cat("\n📝 STEP 14: CREATING LATEX TABLE\n")
cat("==================================\n")

stargazer(model_primary, model_interaction,
          type = "latex",
          title = "Panel 2: Sector × Occupation Analysis - Gender Pay Gap Determinants (2010-2022)",
          column.labels = c("Main Effects", "With Interactions"),
          dep.var.labels = "Gender Pay Gap (\\%)",
          covariate.labels = c(
            "Industry sector",
            "Construction",
            "Services",
            "Public sector",
            "High-skill occupation",
            "Managerial position",
            "Industry × High-skill",
            "Industry × Managerial",
            "Public × High-skill",
            "Public × Managerial",
            "Year 2014",
            "Year 2018",
            "Year 2022"
          ),
          add.lines = list(
            c("Observations", nrow(panel2_clean), nrow(panel2_clean)),
            c("Unique panels", length(unique(panel2_clean$panel_id)), 
              length(unique(panel2_clean$panel_id))),
            c("Countries", length(unique(panel2_clean$country)), 
              length(unique(panel2_clean$country))),
            c("Detailed sectors", "18", "18"),
            c("Occupations (ISCO)", "9", "9"),
            c("Model type", model_type, "Random Effects")
          ),
          omit.stat = c("ser"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("Robust standard errors in parentheses.",
                   "Reference: Services (other), non-high-skill, non-managerial, Year 2010.",
                   "18 detailed NACE sectors: Mining, Manufacturing, Utilities, Construction, Trade,",
                   "Transport, Hospitality, IT, Finance, Real Estate, Professional Services,",
                   "Admin Services, Public Admin, Education, Health, Arts, Other Services."),
          notes.align = "l",
          out = "output/tables/panel2_results.tex")

cat("✅ LaTeX table saved to: output/tables/panel2_results.tex\n\n")

# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n\n✅ PANEL 2 MAIN ANALYSIS COMPLETE!\n")
cat("====================================\n\n")

cat("📊 SUMMARY:\n")
cat("  Cleaned observations:  ", nrow(panel2_clean), "\n")
cat("  Unique panels:         ", length(unique(panel2_clean$panel_id)), "\n")
cat("  Countries:             ", length(unique(panel2_clean$country)), "\n")
cat("  Years:                 ", paste(sort(unique(panel2_clean$year)), collapse = ", "), "\n")
cat("  Detailed sectors:       18 (NACE Rev. 2)\n")
cat("  Occupations:            9 (ISCO-08)\n")
cat("  Average gap:           ", round(mean(panel2_clean$gender_pay_gap, na.rm = TRUE), 2), "%\n\n")

cat("📁 OUTPUT FILES:\n")
cat("  - output/data/panel2_sector_occupation_cleaned.csv\n")
cat("  - output/data/panel2_detailed_sector_analysis.csv\n")
cat("  - output/reports/panel2_model_summaries.txt\n")
cat("  - output/tables/panel2_results.tex\n\n")

cat("🎯 KEY FINDINGS:\n")
cat("  Primary model:         ", model_type, "\n")
if(!is.null(hausman_test)) {
  cat("  Hausman test p-value:  ", round(hausman_test$p.value, 4), "\n")
}
cat("  Model includes:        Sector effects, Occupation effects, Time trends\n")
cat("  Robustness checks:     Balanced panel, Winsorized DV\n\n")

cat("🚀 READY FOR THESIS INTEGRATION!\n")


# Stop capturing output
sink()
