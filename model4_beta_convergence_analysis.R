# ============================================================================
# MODEL 4: BETA CONVERGENCE ANALYSIS
# Research Question 3 (Part 2): Do countries with higher initial gaps converge faster?
# Cross-sectional regression: Change (2010-2022) ~ Initial Gap (2010)
# ============================================================================

library(tidyverse)

cat("============================================\n")
cat("MODEL 4: BETA CONVERGENCE ANALYSIS\n")
cat("RQ3 Part 2: Convergence Dynamics\n")
cat("============================================\n\n")

# Load data
panel2 <- read.csv("output/data/panel2_sector_occupation_4years.csv")

cat("Data loaded:", nrow(panel2), "observations\n\n")

# ============================================================================
# PREPARE CONVERGENCE DATA
# ============================================================================

cat("📊 PREPARING CONVERGENCE DATA\n")
cat("==============================\n\n")

# Calculate country-level mean gaps for 2010 and 2022
convergence_data <- panel2 %>%
  filter(year %in% c(2010, 2022)) %>%
  group_by(country, year) %>%
  summarise(
    mean_gap = mean(gender_pay_gap, na.rm = TRUE),
    n_obs = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(mean_gap, n_obs),
    names_prefix = "year_"
  ) %>%
  filter(!is.na(mean_gap_year_2010), !is.na(mean_gap_year_2022)) %>%
  mutate(
    gap_2010 = mean_gap_year_2010,
    gap_2022 = mean_gap_year_2022,
    change = gap_2022 - gap_2010,
    pct_change = 100 * change / gap_2010,
    converged = change < 0
  ) %>%
  select(country, gap_2010, gap_2022, change, pct_change, converged,
         n_obs_2010 = n_obs_year_2010, n_obs_2022 = n_obs_year_2022)

cat("Countries with complete data (2010 & 2022):", nrow(convergence_data), "\n\n")

cat("Sample overview:\n")
cat("  Mean gap 2010:", round(mean(convergence_data$gap_2010), 2), "%\n")
cat("  Mean gap 2022:", round(mean(convergence_data$gap_2022), 2), "%\n")
cat("  Mean change:  ", round(mean(convergence_data$change), 2), "pp\n")
cat("  Countries that converged (gap reduced):", sum(convergence_data$converged), "\n")
cat("  Countries that diverged (gap increased):", sum(!convergence_data$converged), "\n\n")

# ============================================================================
# BETA CONVERGENCE REGRESSION
# ============================================================================

cat("📈 BETA CONVERGENCE REGRESSION\n")
cat("===============================\n\n")
cat("Model: Change(2022-2010) = α + β × Gap_2010 + ε\n")
cat("H0: β = 0 (no convergence)\n")
cat("Ha: β < 0 (beta convergence - higher initial gaps → larger reductions)\n\n")

# Estimate model
model4 <- lm(change ~ gap_2010, data = convergence_data)

# Print results
print(summary(model4))

# Extract key statistics
beta_coef <- coef(model4)["gap_2010"]
beta_se <- summary(model4)$coefficients["gap_2010", "Std. Error"]
beta_t <- summary(model4)$coefficients["gap_2010", "t value"]
beta_p <- summary(model4)$coefficients["gap_2010", "Pr(>|t|)"]
r_squared <- summary(model4)$r.squared
adj_r_squared <- summary(model4)$adj.r.squared

cat("\n\n📊 CONVERGENCE TEST RESULTS\n")
cat("============================\n\n")
cat(sprintf("β coefficient:      %.4f\n", beta_coef))
cat(sprintf("Standard error:     %.4f\n", beta_se))
cat(sprintf("t-statistic:        %.4f\n", beta_t))
cat(sprintf("p-value:            %.6f\n", beta_p))
cat(sprintf("R²:                 %.4f\n", r_squared))
cat(sprintf("Adjusted R²:        %.4f\n", adj_r_squared))
cat(sprintf("Sample size:        %d countries\n\n", nrow(convergence_data)))

cat("INTERPRETATION:\n")
if(beta_p < 0.05 && beta_coef < 0) {
  cat("✅ BETA CONVERGENCE DETECTED (β < 0, p < 0.05)\n")
  cat(sprintf("   For every 1 pp higher initial gap, countries reduced gaps\n"))
  cat(sprintf("   by an additional %.3f pp over 2010-2022.\n", abs(beta_coef)))
  cat("   Countries with larger gender pay gaps in 2010 experienced\n")
  cat("   faster convergence toward gender wage equality.\n")
} else if(beta_coef < 0) {
  cat("⚠️ WEAK CONVERGENCE (β < 0, but p ≥ 0.05)\n")
  cat("   Coefficient is negative but not statistically significant.\n")
} else {
  cat("❌ NO CONVERGENCE (β ≥ 0)\n")
  cat("   Countries with higher initial gaps did not converge faster.\n")
}

# ============================================================================
# CONVERGENCE HALF-LIFE CALCULATION
# ============================================================================

if(beta_coef < 0 && beta_coef > -1) {
  cat("\n\n⏱️ CONVERGENCE SPEED\n")
  cat("====================\n\n")
  
  # Convergence rate per year
  years <- 12  # 2010 to 2022
  lambda <- -log(1 + beta_coef) / years
  
  if(lambda > 0) {
    half_life <- log(2) / lambda
    cat(sprintf("Convergence rate (λ): %.4f per year\n", lambda))
    cat(sprintf("Half-life:            %.1f years\n", half_life))
    cat(sprintf("                      (time for gap differences to reduce by 50%%)\n\n"))
    cat("Interpretation: At this rate, the dispersion in gender pay gaps\n")
    cat(sprintf("across European countries would halve every %.1f years.\n", half_life))
  }
}

# ============================================================================
# DESCRIPTIVE ANALYSIS
# ============================================================================

cat("\n\n📋 TOP CONVERGERS (Largest Gap Reductions)\n")
cat("===========================================\n\n")

top_convergers <- convergence_data %>%
  filter(converged) %>%
  arrange(change) %>%
  head(10) %>%
  select(country, gap_2010, gap_2022, change, pct_change)

print(top_convergers, row.names = FALSE)

cat("\n\n📋 TOP DIVERGERS (Gap Increased)\n")
cat("=================================\n\n")

top_divergers <- convergence_data %>%
  filter(!converged) %>%
  arrange(desc(change)) %>%
  head(10) %>%
  select(country, gap_2010, gap_2022, change, pct_change)

if(nrow(top_divergers) > 0) {
  print(top_divergers, row.names = FALSE)
} else {
  cat("No countries showed gap increases (all converged).\n")
}

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("\n\n💾 SAVING RESULTS\n")
cat("==================\n\n")

# Save convergence data
write.csv(convergence_data, "output/data/model4_beta_convergence_data.csv", row.names = FALSE)
cat("✅ output/data/model4_beta_convergence_data.csv\n")

# Save full output
sink("output/reports/model4_beta_convergence_full_output.txt")
cat(strrep("=", 80), "\n")
cat("MODEL 4: BETA CONVERGENCE ANALYSIS\n")
cat(strrep("=", 80), "\n\n")

cat("REGRESSION RESULTS:\n")
cat(strrep("-", 80), "\n\n")
print(summary(model4))

cat("\n\nCONVERGENCE DATA:\n")
cat(strrep("-", 80), "\n\n")
print(convergence_data %>% arrange(gap_2010), row.names = FALSE)

cat("\n\nTOP CONVERGERS:\n")
print(top_convergers, row.names = FALSE)

cat("\n\nTOP DIVERGERS:\n")
if(nrow(top_divergers) > 0) {
  print(top_divergers, row.names = FALSE)
} else {
  cat("None - all countries converged.\n")
}

sink()
cat("✅ output/reports/model4_beta_convergence_full_output.txt\n")

# Create LaTeX table
if(requireNamespace("stargazer", quietly = TRUE)) {
  library(stargazer)
  stargazer(model4,
            type = "latex",
            title = "Model 4: Beta Convergence in Gender Pay Gaps (2010-2022)",
            dep.var.labels = "Change in Gender Pay Gap (2022 - 2010)",
            covariate.labels = c("Initial Gap (2010)", "Constant"),
            add.lines = list(
              c("Countries", nrow(convergence_data)),
              c("Time period", "2010-2022 (12 years)"),
              c("Convergence detected?", ifelse(beta_p < 0.05 && beta_coef < 0, "Yes", "No"))
            ),
            omit.stat = c("ser", "f"),
            star.cutoffs = c(0.05, 0.01, 0.001),
            notes = c("OLS regression. Each observation is one country.",
                     "Negative coefficient indicates beta convergence."),
            notes.align = "l",
            out = "output/tables/model4_beta_convergence.tex")
  cat("✅ output/tables/model4_beta_convergence.tex\n")
}

cat("\n\n", strrep("=", 80), "\n")
cat("✅ MODEL 4 ANALYSIS COMPLETE!\n")
cat(strrep("=", 80), "\n\n")

cat("📊 MODEL 4 ADDRESSES RQ3 (PART 2):\n")
cat("   'Is there beta convergence across European countries?'\n\n")
if(beta_p < 0.05 && beta_coef < 0) {
  cat("   Answer: YES - Significant beta convergence detected.\n")
  cat("   Countries with higher initial gaps converged faster.\n")
} else {
  cat("   Answer: Check results above for conclusive evidence.\n")
}
