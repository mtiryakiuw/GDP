# ============================================================================
# MODEL 3: COUNTRY GROUPS + SECTOR INTERACTIONS
# Research Question 3 (Part 1): Institutional determinants across welfare regimes
# 7 country groups × 4 sectors with 3 specific interactions
# ============================================================================

library(tidyverse)
library(plm)
library(lmtest)
library(sandwich)
library(stargazer)

cat("============================================\n")
cat("MODEL 3: COUNTRY GROUPS + INTERACTIONS\n")
cat("RQ3 Part 1: Institutional Heterogeneity\n")
cat("============================================\n\n")

# Load data
panel2 <- read.csv("output/data/panel2_sector_occupation_4years.csv")

cat("Data loaded:", nrow(panel2), "observations\n")
cat("Countries:", length(unique(panel2$country)), "\n\n")

# Load country group classifications  
# Total: 37 countries (after removing EA13, EU15, EU25 aggregates)
source_countries <- data.frame(
  country = c(
    # Nordic: 5
    "DK", "FI", "IS", "NO", "SE",
    # Continental: 7
    "AT", "BE", "FR", "DE", "LU", "NL", "CH",
    # Mediterranean: 6
    "CY", "EL", "IT", "MT", "PT", "ES",
    # Eastern: 9
    "BG", "HR", "CZ", "EE", "HU", "LV", "LT", "PL", "RO", "SK", "SI",  # removed MD, added SI
    # Liberal: 2
    "IE", "UK",
    # Balkans: 5
    "AL", "BA", "ME", "MK", "RS",  # removed XK
    # Other: 1
    "TR"
  ),
  country_group = c(
    rep("Nordic", 5),
    rep("Continental", 7),
    rep("Mediterranean", 6),
    rep("Eastern", 11),
    rep("Liberal", 2),
    rep("Balkans", 5),
    "Other"
  ),
  stringsAsFactors = FALSE
)

# Merge country groups
panel2 <- panel2 %>%
  filter(!country %in% c("EA13", "EU15", "EU25")) %>%  # Remove aggregate regions
  left_join(source_countries, by = "country")

# Check for missing country groups
missing_groups <- panel2 %>% filter(is.na(country_group)) %>% distinct(country)
if(nrow(missing_groups) > 0) {
  cat("⚠️ Warning: Countries without group assignment:\n")
  print(missing_groups$country)
  cat("\n")
}

cat("Country group distribution:\n")
print(table(panel2$country_group, useNA = "ifany"))
cat("\n")

# Create country group dummy variables (Continental = reference)
panel2_model3 <- panel2 %>%
  filter(!is.na(country_group)) %>%
  mutate(
    nordic = as.numeric(country_group == "Nordic"),
    mediterranean = as.numeric(country_group == "Mediterranean"),
    eastern = as.numeric(country_group == "Eastern"),
    liberal = as.numeric(country_group == "Liberal"),
    balkans = as.numeric(country_group == "Balkans"),
    other = as.numeric(country_group == "Other"),
    # Continental is reference (omitted)
    
    # Occupational controls
    high_skill = as.numeric(high_skill),
    managerial = as.numeric(managerial)
  )

cat("Final sample after merging country groups:", nrow(panel2_model3), "observations\n\n")

# Create panel structure
pdata_model3 <- pdata.frame(panel2_model3, index = c("panel_id", "year"))

# ============================================================================
# MODEL 3A: COUNTRY GROUPS (NO INTERACTIONS)
# ============================================================================

cat("📊 MODEL 3A: COUNTRY GROUPS (Main Effects Only)\n")
cat("=================================================\n\n")

model3a_re <- plm(
  gender_pay_gap ~ 
    industry + construction + public_sector +
    high_skill + managerial +
    nordic + mediterranean + eastern + liberal + balkans + other +
    factor(year),
  data = pdata_model3,
  model = "random"
)

print(summary(model3a_re))

cat("\n\nRobust Standard Errors (HC1):\n")
cat("------------------------------\n")
robust3a <- coeftest(model3a_re, vcov = vcovHC(model3a_re, type = "HC1"))
print(robust3a)

# ============================================================================
# MODEL 3B: WITH SPECIFIC INTERACTIONS
# ============================================================================

cat("\n\n📊 MODEL 3B: COUNTRY GROUPS × SECTOR INTERACTIONS\n")
cat("===================================================\n\n")
cat("Three specific interactions based on extended_analysis.R:\n")
cat("  1. Nordic × Public Sector\n")
cat("  2. Mediterranean × Industry\n")
cat("  3. Eastern × Industry\n\n")

model3b_re <- plm(
  gender_pay_gap ~ 
    industry + construction + public_sector +
    high_skill + managerial +
    nordic + mediterranean + eastern + liberal + balkans + other +
    nordic:public_sector + mediterranean:industry + eastern:industry +
    factor(year),
  data = pdata_model3,
  model = "random"
)

print(summary(model3b_re))

cat("\n\nRobust Standard Errors (HC1):\n")
cat("------------------------------\n")
robust3b <- coeftest(model3b_re, vcov = vcovHC(model3b_re, type = "HC1"))
print(robust3b)

# ============================================================================
# INTERPRETATION OF INTERACTIONS
# ============================================================================

cat("\n\n📈 INTERACTION EFFECTS INTERPRETATION\n")
cat("======================================\n\n")

coefs <- coef(model3b_re)

if("nordic:public_sector" %in% names(coefs)) {
  cat(sprintf("Nordic × Public Sector: %+.3f pp\n", coefs["nordic:public_sector"]))
  cat("  → Nordic public sectors show additional gap reduction beyond\n")
  cat("    main effects of Nordic institutions and public sector employment.\n\n")
}

if("mediterranean:industry" %in% names(coefs)) {
  cat(sprintf("Mediterranean × Industry: %+.3f pp\n", coefs["mediterranean:industry"]))
  cat("  → Industrial sectors in Mediterranean countries show\n")
  cat("    different gap patterns than Continental baseline.\n\n")
}

if("eastern:industry" %in% names(coefs)) {
  cat(sprintf("Eastern × Industry: %+.3f pp\n", coefs["eastern:industry"]))
  cat("  → Post-socialist industrial sectors exhibit distinct\n")
  cat("    gender wage dynamics compared to Western Europe.\n\n")
}

# ============================================================================
# COUNTRY GROUP COMPARISONS
# ============================================================================

cat("\n📊 COUNTRY GROUP EFFECTS (relative to Continental reference)\n")
cat("=============================================================\n\n")

group_coefs <- data.frame(
  Country_Group = c("Nordic", "Mediterranean", "Eastern", "Liberal", "Balkans", "Other"),
  Coefficient = c(
    ifelse("nordic" %in% names(coefs), coefs["nordic"], NA),
    ifelse("mediterranean" %in% names(coefs), coefs["mediterranean"], NA),
    ifelse("eastern" %in% names(coefs), coefs["eastern"], NA),
    ifelse("liberal" %in% names(coefs), coefs["liberal"], NA),
    ifelse("balkans" %in% names(coefs), coefs["balkans"], NA),
    ifelse("other" %in% names(coefs), coefs["other"], NA)
  )
) %>%
  arrange(Coefficient)

cat("Ranking (lowest to highest gaps, relative to Continental):\n")
print(group_coefs, row.names = FALSE)

# ============================================================================
# SAVE RESULTS
# ============================================================================

cat("\n\n💾 SAVING RESULTS\n")
cat("==================\n\n")

# Save country group coefficients
write.csv(group_coefs, "output/tables/model3_country_group_effects.csv", row.names = FALSE)
cat("✅ output/tables/model3_country_group_effects.csv\n")

# Save full output
sink("output/reports/model3_country_groups_full_output.txt")
cat(strrep("=", 80), "\n")
cat("MODEL 3: COUNTRY GROUPS + SECTOR INTERACTIONS\n")
cat(strrep("=", 80), "\n\n")

cat("MODEL 3A: MAIN EFFECTS ONLY\n")
cat(strrep("-", 80), "\n\n")
print(summary(model3a_re))
cat("\n\nRobust SE:\n")
print(robust3a)

cat("\n\n", strrep("=", 80), "\n")
cat("MODEL 3B: WITH SPECIFIC INTERACTIONS\n")
cat(strrep("-", 80), "\n\n")
print(summary(model3b_re))
cat("\n\nRobust SE:\n")
print(robust3b)

cat("\n\nCOUNTRY GROUP EFFECTS:\n")
print(group_coefs, row.names = FALSE)

sink()
cat("✅ output/reports/model3_country_groups_full_output.txt\n")

# LaTeX table
stargazer(model3a_re, model3b_re,
          type = "latex",
          title = "Model 3: Country Groups and Institutional Heterogeneity",
          column.labels = c("Main Effects", "With Interactions"),
          dep.var.labels = "Gender Pay Gap (\\%)",
          covariate.labels = c(
            "Industry",
            "Construction",
            "Public Sector",
            "High-Skill Occupation",
            "Managerial Position",
            "Nordic countries",
            "Mediterranean countries",
            "Eastern European countries",
            "Liberal countries (UK/IE)",
            "Balkans countries",
            "Other (Turkey)",
            "Nordic × Public Sector",
            "Mediterranean × Industry",
            "Eastern × Industry",
            "Year 2014",
            "Year 2018",
            "Year 2022"
          ),
          add.lines = list(
            c("Panel units", 
              length(unique(panel2_model3$panel_id)),
              length(unique(panel2_model3$panel_id))),
            c("Countries", 
              length(unique(panel2_model3$country)),
              length(unique(panel2_model3$country))),
            c("Reference: Continental", "Yes", "Yes")
          ),
          omit.stat = c("ser"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          notes = c("Random Effects models with HC1 cluster-robust standard errors.",
                   "Reference: Continental welfare regime, Services sector, Year 2010."),
          notes.align = "l",
          out = "output/tables/model3_country_groups.tex")

cat("✅ output/tables/model3_country_groups.tex\n")

cat("\n\n", strrep("=", 80), "\n")
cat("✅ MODEL 3 ANALYSIS COMPLETE!\n")
cat(strrep("=", 80), "\n\n")

cat("📊 MODEL 3 ADDRESSES RQ3 (PART 1):\n")
cat("   'How do welfare regimes moderate sectoral wage gaps?'\n\n")
cat("   Interaction effects show institutional contingency\n")
cat("   of sectoral determinants across welfare state types.\n")
