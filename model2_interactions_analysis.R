# ============================================================================
# MODEL 2: SECTOR-OCCUPATION INTERACTIONS
# Research Question 2: Do occupational penalties vary by sector?
# 4 aggregated sectors × 2 occupational indicators with interactions
# ============================================================================

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)
library(car)

cat("============================================\n")
cat("MODEL 2: SECTOR-OCCUPATION INTERACTIONS\n")
cat("Research Question 2: Occupational Moderation\n")
cat("============================================\n\n")

# Load data
panel2 <- read.csv("output/data/panel2_sector_occupation_4years.csv")

cat("Data loaded:", nrow(panel2), "observations\n")
cat("Countries:", length(unique(panel2$country)), "\n\n")

# Create panel structure
pdata_model2 <- pdata.frame(panel2, index = c("panel_id", "year"))

cat("Panel dimensions:\n")
cat("  N (observations):", nrow(pdata_model2), "\n")
cat("  n (panels):", length(unique(panel2$panel_id)), "\n")
cat("  T (time periods):", length(unique(panel2$year)), "\n\n")

# ============================================================================
# MODEL 2A: BASELINE (NO INTERACTIONS)
# ============================================================================

cat("📊 MODEL 2A: BASELINE (Additive Effects Only)\n")
cat("===============================================\n\n")

model2a_re <- plm(
  gender_pay_gap ~ 
    industry + construction + public_sector +
    high_skill + managerial +
    factor(year),
  data = pdata_model2,
  model = "random"
)

print(summary(model2a_re))

cat("\n\nRobust Standard Errors (HC1):\n")
cat("------------------------------\n")
robust2a <- coeftest(model2a_re, vcov = vcovHC(model2a_re, type = "HC1"))
print(robust2a)

# ============================================================================
# MODEL 2B: WITH INTERACTIONS
# ============================================================================

cat("\n\n📊 MODEL 2B: SECTOR × OCCUPATION INTERACTIONS\n")
cat("===============================================\n\n")

model2b_re <- plm(
  gender_pay_gap ~ 
    industry + construction + public_sector +
    high_skill + managerial +
    industry:high_skill + industry:managerial +
    public_sector:high_skill + public_sector:managerial +
    factor(year),
  data = pdata_model2,
  model = "random"
)

print(summary(model2b_re))

cat("\n\nRobust Standard Errors (HC1):\n")
cat("------------------------------\n")
robust2b <- coeftest(model2b_re, vcov = vcovHC(model2b_re, type = "HC1"))
print(robust2b)

# ============================================================================
# JOINT SIGNIFICANCE TEST FOR INTERACTIONS
# ============================================================================

cat("\n\n🔬 JOINT SIGNIFICANCE TEST FOR INTERACTIONS\n")
cat("=============================================\n\n")
cat("H0: All 4 interaction coefficients = 0\n")
cat("(Testing whether interaction model improves over additive model)\n\n")

interaction_terms <- c("industry:high_skill", "industry:managerial",
                       "public_sector:high_skill", "public_sector:managerial")

wald_test <- tryCatch({
  linearHypothesis(model2b_re, 
                   interaction_terms,
                   vcov = vcovHC(model2b_re, type = "HC1"))
}, error = function(e) {
  cat("Wald test failed:", e$message, "\n")
  NULL
})

if(!is.null(wald_test)) {
  print(wald_test)
  
  # Extract test statistics
  if("F" %in% names(wald_test)) {
    f_stat <- wald_test$F[2]
    p_value <- wald_test$`Pr(>F)`[2]
  } else if("Chisq" %in% names(wald_test)) {
    f_stat <- wald_test$Chisq[2]
    p_value <- wald_test$`Pr(>Chisq)`[2]
  }
  
  cat("\n📊 INTERPRETATION:\n")
  if(p_value < 0.05) {
    cat("✅ Interactions are jointly significant (p <", round(p_value, 4), ")\n")
    cat("   Occupational effects vary significantly across sectors.\n")
    cat("   Model 2B (with interactions) is preferred over Model 2A.\n")
  } else {
    cat("❌ Interactions not jointly significant (p =", round(p_value, 4), ")\n")
    cat("   Additive model (2A) may be sufficient.\n")
  }
}

# ============================================================================
# MARGINAL EFFECTS CALCULATION
# ============================================================================

cat("\n\n📈 MARGINAL EFFECTS: PREDICTED GAPS BY SECTOR-OCCUPATION\n")
cat("==========================================================\n\n")

# Extract coefficients
coefs <- coef(model2b_re)
intercept <- coefs["(Intercept)"]
industry_coef <- coefs["industry"]
construction_coef <- coefs["construction"]
public_coef <- coefs["public_sector"]
high_skill_coef <- coefs["high_skill"]
managerial_coef <- coefs["managerial"]
ind_hs <- coefs["industry:high_skill"]
ind_mgr <- coefs["industry:managerial"]
pub_hs <- coefs["public_sector:high_skill"]
pub_mgr <- coefs["public_sector:managerial"]

# Create predictions for 12 combinations (4 sectors × 3 occupations)
predictions <- data.frame(
  Sector = rep(c("Services", "Industry", "Construction", "Public Sector"), each = 3),
  Occupation = rep(c("Baseline", "High-Skill", "Managerial"), 4),
  Predicted_Gap = NA
)

# Services (reference) - baseline
predictions$Predicted_Gap[1] <- intercept
predictions$Predicted_Gap[2] <- intercept + high_skill_coef
predictions$Predicted_Gap[3] <- intercept + managerial_coef

# Industry
predictions$Predicted_Gap[4] <- intercept + industry_coef
predictions$Predicted_Gap[5] <- intercept + industry_coef + high_skill_coef + ind_hs
predictions$Predicted_Gap[6] <- intercept + industry_coef + managerial_coef + ind_mgr

# Construction
predictions$Predicted_Gap[7] <- intercept + construction_coef
predictions$Predicted_Gap[8] <- intercept + construction_coef + high_skill_coef
predictions$Predicted_Gap[9] <- intercept + construction_coef + managerial_coef

# Public Sector
predictions$Predicted_Gap[10] <- intercept + public_coef
predictions$Predicted_Gap[11] <- intercept + public_coef + high_skill_coef + pub_hs
predictions$Predicted_Gap[12] <- intercept + public_coef + managerial_coef + pub_mgr

predictions$Predicted_Gap <- round(predictions$Predicted_Gap, 2)

cat("Predicted Gender Pay Gaps (percentage points):\n")
cat("===============================================\n\n")
print(predictions, row.names = FALSE)

# Calculate interaction effects
cat("\n\nINTERACTION EFFECTS (deviation from additive prediction):\n")
cat("===========================================================\n\n")
cat(sprintf("Industry × High-Skill:           %+.3f pp\n", ind_hs))
cat(sprintf("Industry × Managerial:           %+.3f pp\n", ind_mgr))
cat(sprintf("Public Sector × High-Skill:      %+.3f pp\n", pub_hs))
cat(sprintf("Public Sector × Managerial:      %+.3f pp\n", pub_mgr))

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("\n\n💾 SAVING RESULTS\n")
cat("==================\n\n")

# Save predictions
write.csv(predictions, "output/data/model2_marginal_effects_predictions.csv", row.names = FALSE)
cat("✅ output/data/model2_marginal_effects_predictions.csv\n")

# Save full output
sink("output/reports/model2_interactions_full_output.txt")
cat(strrep("=", 80), "\n")
cat("MODEL 2: SECTOR-OCCUPATION INTERACTIONS\n")
cat(strrep("=", 80), "\n\n")

cat("MODEL 2A: BASELINE (NO INTERACTIONS)\n")
cat(strrep("-", 80), "\n\n")
print(summary(model2a_re))
cat("\n\nRobust SE:\n")
print(robust2a)

cat("\n\n", strrep("=", 80), "\n")
cat("MODEL 2B: WITH INTERACTIONS\n")
cat(strrep("-", 80), "\n\n")
print(summary(model2b_re))
cat("\n\nRobust SE:\n")
print(robust2b)

cat("\n\nJOINT TEST:\n")
if(!is.null(wald_test)) print(wald_test)

cat("\n\nMARGINAL EFFECTS:\n")
print(predictions, row.names = FALSE)

sink()
cat("✅ output/reports/model2_interactions_full_output.txt\n")

# LaTeX table
stargazer(model2a_re, model2b_re,
          type = "latex",
          title = "Model 2: Sector-Occupation Interactions",
          column.labels = c("Additive", "With Interactions"),
          dep.var.labels = "Gender Pay Gap (\\%)",
          covariate.labels = c(
            "Industry",
            "Construction",
            "Public Sector",
            "High-Skill Occupation",
            "Managerial Position",
            "Industry × High-Skill",
            "Industry × Managerial",
            "Public Sector × High-Skill",
            "Public Sector × Managerial",
            "Year 2014",
            "Year 2018",
            "Year 2022"
          ),
          add.lines = list(
            c("Panel units", length(unique(panel2$panel_id)), length(unique(panel2$panel_id))),
            c("Countries", length(unique(panel2$country)), length(unique(panel2$country))),
            c("Reference: Services", "Yes", "Yes")
          ),
          omit.stat = c("ser"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("Random Effects models with HC1 cluster-robust standard errors.",
                   "Reference categories: Services (sector), Baseline occupation (ISCO 4-9), Year 2010."),
          notes.align = "l",
          out = "output/tables/model2_interactions.tex")

cat("✅ output/tables/model2_interactions.tex\n")

cat("\n\n", strrep("=", 80), "\n")
cat("✅ MODEL 2 ANALYSIS COMPLETE!\n")
cat(strrep("=", 80), "\n\n")

cat("📊 MODEL 2 DIRECTLY ADDRESSES RESEARCH QUESTION 2:\n")
cat("   'Do occupational penalties vary systematically across sectors?'\n\n")
cat("   Answer: ")
if(!is.null(wald_test) && p_value < 0.05) {
  cat("YES - Significant interactions detected.\n")
} else {
  cat("Check joint test results above.\n")
}
