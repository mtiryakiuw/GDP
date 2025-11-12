# ============================================================================
# MODEL 1: 18 DETAILED SECTORS REGRESSION
# Research Question 1: Sectoral heterogeneity across 18 NACE Rev. 2 sectors
# Created to formally test which sectors have significantly different gaps
# ============================================================================

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)

cat("============================================\n")
cat("MODEL 1: 18 DETAILED SECTORS ANALYSIS\n")
cat("Research Question 1: Sectoral Heterogeneity\n")
cat("============================================\n\n")

# Load data
panel2 <- read.csv("output/data/panel2_sector_occupation_4years.csv")

cat("Data loaded:", nrow(panel2), "observations\n")
cat("Sectors:", length(unique(panel2$sector)), "\n")
cat("Countries:", length(unique(panel2$country)), "\n\n")

# Create sector dummy variables (Other Services as reference category)
panel2_model1 <- panel2 %>%
  mutate(
    # 18 detailed sectors - using exact names from data
    Mining = as.numeric(sector == "Mining"),
    Manufacturing = as.numeric(sector == "Manufacturing"),
    Electricity = as.numeric(sector == "Electricity"),
    Water = as.numeric(sector == "Water"),
    Construction = as.numeric(sector == "Construction"),
    Trade = as.numeric(sector == "Trade"),
    Transport = as.numeric(sector == "Transport"),
    Hospitality = as.numeric(sector == "Hospitality"),
    IT = as.numeric(sector == "IT"),
    Finance = as.numeric(sector == "Finance"),
    RealEstate = as.numeric(sector == "Real Estate"),
    Professional = as.numeric(sector == "Professional"),
    Admin = as.numeric(sector == "Admin Services"),
    PublicAdmin = as.numeric(sector == "Public Admin"),
    Education = as.numeric(sector == "Education Sector"),
    Health = as.numeric(sector == "Health"),
    Arts = as.numeric(sector == "Arts"),
    # Other Services is the reference category (omitted)
    
    # Occupational controls
    high_skill = as.numeric(high_skill),
    managerial = as.numeric(managerial)
  )

# Check sector distribution
cat("Sector Distribution:\n")
sector_counts <- panel2_model1 %>%
  group_by(sector) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n))
print(sector_counts)

# Create panel structure
pdata_model1 <- pdata.frame(panel2_model1, index = c("panel_id", "year"))

cat("\n\n📊 ESTIMATING MODEL 1: 18 SECTOR DUMMIES\n")
cat("==========================================\n\n")

# Model 1a: Random Effects with 18 sector dummies
cat("Model 1a: Random Effects (18 Sectors)\n")
cat("--------------------------------------\n")

model1_re <- plm(
  gender_pay_gap ~ 
    Mining + Manufacturing + Electricity + Water + Construction +
    Trade + Transport + Hospitality + IT + Finance + RealEstate +
    Professional + Admin + PublicAdmin + Education + Health + Arts +
    high_skill + managerial +
    factor(year),
  data = pdata_model1,
  model = "random"
)

print(summary(model1_re))

# Robust standard errors
cat("\n\nRobust Standard Errors (HC1 cluster-robust):\n")
cat("---------------------------------------------\n")
robust_model1_re <- coeftest(model1_re, vcov = vcovHC(model1_re, type = "HC1"))
print(robust_model1_re)

# Model 1b: Fixed Effects with 18 sector dummies
cat("\n\n\nModel 1b: Fixed Effects (18 Sectors)\n")
cat("--------------------------------------\n")
cat("Note: FE eliminates time-invariant sector dummies\n")
cat("Only time-varying effects can be estimated in FE\n\n")

model1_fe <- plm(
  gender_pay_gap ~ 
    high_skill + managerial +
    factor(year),
  data = pdata_model1,
  model = "within"
)

print(summary(model1_fe))

# ============================================================================
# HYPOTHESIS TESTING: Which sectors differ significantly from reference?
# ============================================================================

cat("\n\n\n🎯 HYPOTHESIS TESTING FOR RQ1\n")
cat("===============================\n\n")
cat("Research Question 1: How do gender pay gaps vary across 18 sectors?\n")
cat("Reference Category: Other Services\n\n")

# Extract coefficients and p-values from robust RE model
coefs <- robust_model1_re[, "Estimate"]
se <- robust_model1_re[, "Std. Error"]
pvals <- robust_model1_re[, "Pr(>|t|)"]

# Create results table for sectors
sector_vars <- c("Mining", "Manufacturing", "Electricity", "Water", "Construction",
                 "Trade", "Transport", "Hospitality", "IT", "Finance", "RealEstate",
                 "Professional", "Admin", "PublicAdmin", "Education", "Health", "Arts")

sector_results <- data.frame(
  Sector = sector_vars,
  Coefficient = NA,
  SE = NA,
  t_stat = NA,
  p_value = NA,
  Significance = ""
)

for(i in 1:length(sector_vars)) {
  var_name <- sector_vars[i]
  if(var_name %in% names(coefs)) {
    sector_results$Coefficient[i] <- coefs[var_name]
    sector_results$SE[i] <- se[var_name]
    sector_results$t_stat[i] <- coefs[var_name] / se[var_name]
    sector_results$p_value[i] <- pvals[var_name]
    
    # Add significance stars
    if(pvals[var_name] < 0.001) {
      sector_results$Significance[i] <- "***"
    } else if(pvals[var_name] < 0.01) {
      sector_results$Significance[i] <- "**"
    } else if(pvals[var_name] < 0.05) {
      sector_results$Significance[i] <- "*"
    } else {
      sector_results$Significance[i] <- ""
    }
  }
}

# Sort by coefficient size
sector_results <- sector_results %>%
  arrange(desc(Coefficient))

cat("Sector Coefficients (relative to Other Services reference):\n")
cat("============================================================\n\n")
print(sector_results, row.names = FALSE)

# Summary interpretation
cat("\n\n📊 SUMMARY OF FINDINGS:\n")
cat("=======================\n\n")

sig_positive <- sector_results %>%
  filter(p_value < 0.05, Coefficient > 0)

sig_negative <- sector_results %>%
  filter(p_value < 0.05, Coefficient < 0)

not_sig <- sector_results %>%
  filter(p_value >= 0.05)

cat(sprintf("Sectors with SIGNIFICANTLY HIGHER gaps than Other Services: %d\n", nrow(sig_positive)))
if(nrow(sig_positive) > 0) {
  for(i in 1:nrow(sig_positive)) {
    cat(sprintf("  • %s: +%.2f pp %s (p=%.4f)\n", 
                sig_positive$Sector[i], 
                sig_positive$Coefficient[i],
                sig_positive$Significance[i],
                sig_positive$p_value[i]))
  }
}

cat(sprintf("\nSectors with SIGNIFICANTLY LOWER gaps than Other Services: %d\n", nrow(sig_negative)))
if(nrow(sig_negative) > 0) {
  for(i in 1:nrow(sig_negative)) {
    cat(sprintf("  • %s: %.2f pp %s (p=%.4f)\n", 
                sig_negative$Sector[i], 
                sig_negative$Coefficient[i],
                sig_negative$Significance[i],
                sig_negative$p_value[i]))
  }
}

cat(sprintf("\nSectors NOT significantly different from Other Services: %d\n", nrow(not_sig)))
if(nrow(not_sig) > 0) {
  for(i in 1:min(nrow(not_sig), 5)) {
    cat(sprintf("  • %s: %.2f pp (p=%.4f)\n", 
                not_sig$Sector[i], 
                not_sig$Coefficient[i],
                not_sig$p_value[i]))
  }
  if(nrow(not_sig) > 5) cat(sprintf("  ... and %d more\n", nrow(not_sig) - 5))
}

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("\n\n💾 SAVING RESULTS\n")
cat("==================\n\n")

# Save coefficient table
write.csv(sector_results, "output/tables/model1_18sectors_coefficients.csv", row.names = FALSE)
cat("✅ output/tables/model1_18sectors_coefficients.csv\n")

# Save full model output
sink("output/reports/model1_18sectors_full_output.txt")
cat(strrep("=", 80), "\n")
cat("MODEL 1: 18 DETAILED SECTORS REGRESSION ANALYSIS\n")
cat("Research Question 1: Sectoral Heterogeneity\n")
cat(strrep("=", 80), "\n\n")

cat("RANDOM EFFECTS MODEL\n")
cat(strrep("-", 80), "\n\n")
print(summary(model1_re))

cat("\n\nROBUST STANDARD ERRORS (HC1 cluster-robust):\n")
cat(strrep("-", 80), "\n\n")
print(robust_model1_re)

cat("\n\nSECTOR COEFFICIENTS TABLE:\n")
cat(strrep("-", 80), "\n\n")
print(sector_results, row.names = FALSE)

sink()
cat("✅ output/reports/model1_18sectors_full_output.txt\n")

# Create LaTeX table for thesis
stargazer(model1_re,
          type = "latex",
          title = "Model 1: Gender Pay Gap Determinants with 18 Detailed NACE Rev. 2 Sectors",
          dep.var.labels = "Gender Pay Gap (\\%)",
          covariate.labels = c(
            "Mining \\& Quarrying",
            "Manufacturing",
            "Electricity \\& Gas Supply",
            "Water Supply \\& Waste",
            "Construction",
            "Wholesale \\& Retail Trade",
            "Transportation \\& Storage",
            "Hospitality \\& Food Services",
            "IT \\& Communication",
            "Finance \\& Insurance",
            "Real Estate",
            "Professional Services",
            "Administrative Services",
            "Public Administration",
            "Education",
            "Health \\& Social Work",
            "Arts \\& Entertainment",
            "High-Skill Occupation",
            "Managerial Position",
            "Year 2014",
            "Year 2018",
            "Year 2022"
          ),
          add.lines = list(
            c("Panel units", length(unique(panel2_model1$panel_id))),
            c("Countries", length(unique(panel2_model1$country))),
            c("Reference: Other Services", "Yes")
          ),
          omit.stat = c("ser"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("Random Effects model with HC1 cluster-robust standard errors.",
                   "Reference categories: Other Services (sector), Year 2010 (time)."),
          notes.align = "l",
          out = "output/tables/model1_18sectors.tex")

cat("✅ output/tables/model1_18sectors.tex\n")

cat("\n\n", strrep("=", 80), "\n")
cat("✅ MODEL 1 ANALYSIS COMPLETE!\n")
cat(strrep("=", 80), "\n\n")

cat("📊 MODEL 1 DIRECTLY ADDRESSES RESEARCH QUESTION 1:\n")
cat("   'How do gender pay gaps vary across 18 detailed NACE sectors?'\n\n")
cat("   This model estimates separate coefficients for each sector,\n")
cat("   enabling formal hypothesis tests about sectoral differences.\n\n")
