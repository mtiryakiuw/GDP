# Diagnostic Tests for Panel 2 Gender Pay Gap Analysis
# Date: 2025-10-30
# Purpose: Breusch-Godfrey and Breusch-Pagan tests for panel regression diagnostics

library(plm)
library(lmtest)

# Start capturing output to report file
sink("output/reports/diagnostic_tests_results.txt")

# Load Panel 2 data
gap_panel2 <- read.csv('output/data/panel2_sector_occupation_4years.csv')

# Create panel data frame
pdata <- pdata.frame(gap_panel2, index = c('panel_id', 'year'))

cat("=== PANEL 2 DIAGNOSTIC TESTS ===\n\n")
cat("Dataset: panel2_sector_occupation_4years.csv\n")
cat("Observations:", nrow(gap_panel2), "\n")
cat("Panels:", length(unique(gap_panel2$panel_id)), "\n\n")

# ========================================
# 1. BREUSCH-GODFREY TEST (Serial Correlation)
# ========================================
cat("--- BREUSCH-GODFREY TEST ---\n")
cat("Tests for: Serial correlation in panel residuals\n")
cat("H0: No serial correlation\n\n")

model_fe <- plm(
  gender_pay_gap ~ industry + construction + public_sector + 
    high_skill + managerial + factor(year),
  data = pdata, 
  model = 'within'
)

tryCatch({
  bg_test <- pbgtest(model_fe)
  print(bg_test)
  cat("\nInterpretation:\n")
  if (bg_test$p.value < 0.001) {
    cat("*** Highly significant (p < 0.001): Strong evidence of serial correlation\n")
    cat("→ Cluster-robust standard errors are necessary\n")
  } else if (bg_test$p.value < 0.05) {
    cat("** Significant (p < 0.05): Evidence of serial correlation\n")
  } else {
    cat("No significant serial correlation detected\n")
  }
}, error = function(e) {
  cat("ERROR: Could not perform Breusch-Godfrey test\n")
  cat("Error message:", e$message, "\n")
})

cat("\n")

# ========================================
# 2. BREUSCH-PAGAN TEST (Heteroskedasticity)
# ========================================
cat("--- BREUSCH-PAGAN TEST ---\n")
cat("Tests for: Heteroskedasticity in panel residuals\n")
cat("H0: Homoskedastic errors\n\n")

model_re <- plm(
  gender_pay_gap ~ industry + construction + public_sector + 
    high_skill + managerial + factor(year),
  data = pdata, 
  model = 'random'
)

tryCatch({
  bp_test <- bptest(model_re)
  print(bp_test)
  cat("\nInterpretation:\n")
  if (bp_test$p.value < 0.001) {
    cat("*** Highly significant (p < 0.001): Strong evidence of heteroskedasticity\n")
    cat("→ Robust standard errors are necessary\n")
  } else if (bp_test$p.value < 0.05) {
    cat("** Significant (p < 0.05): Evidence of heteroskedasticity\n")
  } else {
    cat("No significant heteroskedasticity detected\n")
  }
}, error = function(e) {
  cat("ERROR: Could not perform Breusch-Pagan test\n")
  cat("Error message:", e$message, "\n")
})

cat("\n=== TESTS COMPLETED ===\n")

# ========================================
# 3. MAIN REGRESSION MODELS (Table 2)
# ========================================
cat("\n\n=== TABLE 2 MAIN RESULTS ===\n\n")

# Random Effects Model
cat("--- RANDOM EFFECTS MODEL ---\n")
model_re_main <- plm(
  gender_pay_gap ~ industry + construction + public_sector + 
    high_skill + managerial + factor(year),
  data = pdata, 
  model = 'random'
)

# Cluster-robust standard errors
library(lmtest)
library(sandwich)
coef_re <- coeftest(model_re_main, vcov = vcovHC(model_re_main, type = "HC1", cluster = "group"))
print(coef_re)

cat("\nModel Summary:\n")
summary(model_re_main)

# Fixed Effects Model
cat("\n\n--- FIXED EFFECTS MODEL ---\n")
model_fe_main <- plm(
  gender_pay_gap ~ industry + construction + public_sector + 
    high_skill + managerial + factor(year),
  data = pdata, 
  model = 'within'
)

# Cluster-robust standard errors
coef_fe <- coeftest(model_fe_main, vcov = vcovHC(model_fe_main, type = "HC1", cluster = "group"))
print(coef_fe)

cat("\nModel Summary:\n")
summary(model_fe_main)

# Hausman Test
cat("\n\n--- HAUSMAN TEST ---\n")
cat("Tests for: Fixed vs Random Effects specification\n")
cat("H0: Random Effects is consistent and efficient\n\n")

tryCatch({
  hausman_test <- phtest(model_fe_main, model_re_main)
  print(hausman_test)
  cat("\nInterpretation:\n")
  if (hausman_test$p.value < 0.001) {
    cat("*** Highly significant (p < 0.001): Reject H0\n")
    cat("→ Fixed Effects is preferred\n")
  } else if (hausman_test$p.value < 0.05) {
    cat("** Significant (p < 0.05): Reject H0\n")
    cat("→ Fixed Effects is preferred\n")
  } else {
    cat("Cannot reject H0: Random Effects is preferred\n")
  }
}, error = function(e) {
  cat("ERROR: Could not perform Hausman test\n")
  cat("Error message:", e$message, "\n")
})

# Extract key coefficients for Table 2
cat("\n\n=== KEY COEFFICIENTS FOR TABLE 2 ===\n\n")

cat("Random Effects (with HC1 cluster-robust SE):\n")
print(coef_re)

cat("\n\nFixed Effects (with HC1 cluster-robust SE):\n")
print(coef_fe)

cat("\n=== ANALYSIS COMPLETED ===\n")

# Close report file
sink()

cat("\n📝 Report saved to: output/reports/diagnostic_tests_results.txt\n")
