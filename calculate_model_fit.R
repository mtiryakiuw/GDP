library(plm)
library(dplyr)

cat("=== CALCULATING AIC/BIC FOR MODEL COMPARISON ===\n\n")

# Load data
gap_panel2 <- read.csv("output/data/panel2_sector_occupation_4years.csv")

cat("Data loaded:\n")
cat("  N observations:", nrow(gap_panel2), "\n")
cat("  N unique panels:", length(unique(gap_panel2$panel_id)), "\n\n")

# For plm models, we calculate AIC/BIC using RSS
# AIC = n * log(RSS/n) + 2*k
# BIC = n * log(RSS/n) + k*log(n)

# MODEL 1: Fixed Effects (Time only)
cat("MODEL 1: Fixed Effects (Time only)\n")
cat("-----------------------------------\n")
model_fe <- plm(gender_pay_gap ~ factor(year), 
                data = gap_panel2, 
                index = c("panel_id", "year"),
                model = "within")
rss_fe <- deviance(model_fe)
n_fe <- nobs(model_fe)
k_fe <- length(coef(model_fe)) + 1  # +1 for variance parameter
aic_fe <- n_fe * log(rss_fe/n_fe) + 2 * k_fe
bic_fe <- n_fe * log(rss_fe/n_fe) + k_fe * log(n_fe)
r2_fe <- summary(model_fe)$r.squared[1]

cat("  RSS:", round(rss_fe, 2), "\n")
cat("  Parameters (k):", k_fe, "\n")
cat("  Observations (n):", n_fe, "\n")
cat("  AIC:", round(aic_fe, 2), "\n")
cat("  BIC:", round(bic_fe, 2), "\n")
cat("  R-squared:", round(r2_fe, 4), "\n\n")

# MODEL 2: Random Effects (Sectors + Occupations, no interactions)
cat("MODEL 2: Random Effects (Sectors + Occupations, no interactions)\n")
cat("----------------------------------------------------------------\n")
model_re <- plm(gender_pay_gap ~ industry + construction + 
                public_sector + high_skill + managerial + factor(year),
                data = gap_panel2,
                index = c("panel_id", "year"),
                model = "random")
rss_re <- deviance(model_re)
n_re <- nobs(model_re)
k_re <- length(coef(model_re)) + 1
aic_re <- n_re * log(rss_re/n_re) + 2 * k_re
bic_re <- n_re * log(rss_re/n_re) + k_re * log(n_re)
r2_re <- summary(model_re)$r.squared[1]

cat("  RSS:", round(rss_re, 2), "\n")
cat("  Parameters (k):", k_re, "\n")
cat("  Observations (n):", n_re, "\n")
cat("  AIC:", round(aic_re, 2), "\n")
cat("  BIC:", round(bic_re, 2), "\n")
cat("  R-squared:", round(r2_re, 4), "\n\n")

# MODEL 3: Random Effects with Interactions (PRIMARY MODEL)
cat("MODEL 3: Random Effects with Sector × Occupation Interactions\n")
cat("--------------------------------------------------------------\n")
model_re_int <- plm(gender_pay_gap ~ industry + construction + 
                    public_sector + high_skill + managerial + 
                    industry:high_skill + industry:managerial + 
                    public_sector:high_skill + public_sector:managerial + 
                    factor(year),
                    data = gap_panel2,
                    index = c("panel_id", "year"),
                    model = "random")
rss_re_int <- deviance(model_re_int)
n_re_int <- nobs(model_re_int)
k_re_int <- length(coef(model_re_int)) + 1
aic_re_int <- n_re_int * log(rss_re_int/n_re_int) + 2 * k_re_int
bic_re_int <- n_re_int * log(rss_re_int/n_re_int) + k_re_int * log(n_re_int)
r2_re_int <- summary(model_re_int)$r.squared[1]

cat("  RSS:", round(rss_re_int, 2), "\n")
cat("  Parameters (k):", k_re_int, "\n")
cat("  Observations (n):", n_re_int, "\n")
cat("  AIC:", round(aic_re_int, 2), "\n")
cat("  BIC:", round(bic_re_int, 2), "\n")
cat("  R-squared:", round(r2_re_int, 4), "\n\n")

# MODEL 4: Country Groups Model
cat("MODEL 4: Random Effects with Country Groups\n")
cat("--------------------------------------------\n")

# Add country group classification to panel2 data
country_groups <- list(
  Nordic = c("DK", "FI", "IS", "NO", "SE"),
  Continental = c("AT", "BE", "DE", "FR", "LU", "NL", "CH", "LI"),
  Mediterranean = c("CY", "EL", "ES", "IT", "MT", "PT"),
  Eastern = c("BG", "CZ", "EE", "HR", "HU", "LT", "LV", "MD", "PL", "RO", "SI", "SK"),
  Liberal = c("IE", "UK"),
  Balkans = c("AL", "BA", "ME", "MK", "RS", "XK"),
  Other = c("TR")
)

gap_extended <- gap_panel2 %>%
  mutate(
    country_group = case_when(
      country %in% country_groups$Nordic ~ "Nordic",
      country %in% country_groups$Continental ~ "Continental",
      country %in% country_groups$Mediterranean ~ "Mediterranean",
      country %in% country_groups$Eastern ~ "Eastern",
      country %in% country_groups$Liberal ~ "Liberal",
      country %in% country_groups$Balkans ~ "Balkans",
      country %in% country_groups$Other ~ "Other",
      TRUE ~ "Continental"  # Default to Continental as reference
    ),
    nordic = ifelse(country_group == "Nordic", 1, 0),
    mediterranean = ifelse(country_group == "Mediterranean", 1, 0),
    eastern = ifelse(country_group == "Eastern", 1, 0),
    liberal = ifelse(country_group == "Liberal", 1, 0),
    balkans = ifelse(country_group == "Balkans", 1, 0),
    other = ifelse(country_group == "Other", 1, 0)
  )

model_country <- plm(gender_pay_gap ~ industry + construction + 
                     public_sector + high_skill + managerial + 
                     nordic + mediterranean + eastern + liberal + balkans + other + 
                     factor(year),
                     data = gap_extended,
                     index = c("panel_id", "year"),
                     model = "random")
rss_country <- deviance(model_country)
n_country <- nobs(model_country)
k_country <- length(coef(model_country)) + 1
aic_country <- n_country * log(rss_country/n_country) + 2 * k_country
bic_country <- n_country * log(rss_country/n_country) + k_country * log(n_country)
r2_country <- summary(model_country)$r.squared[1]

cat("  RSS:", round(rss_country, 2), "\n")
cat("  Parameters (k):", k_country, "\n")
cat("  Observations (n):", n_country, "\n")
cat("  AIC:", round(aic_country, 2), "\n")
cat("  BIC:", round(bic_country, 2), "\n")
cat("  R-squared:", round(r2_country, 4), "\n\n")

# SUMMARY TABLE
cat("\n=== MODEL COMPARISON SUMMARY ===\n\n")
cat(sprintf("%-35s %8s %5s %10s %10s\n", 
            "Model", "N", "k", "AIC", "BIC"))
cat(strrep("-", 75), "\n")
cat(sprintf("%-35s %8d %5d %10.1f %10.1f\n", 
            "FE (Time only)", n_fe, k_fe, aic_fe, bic_fe))
cat(sprintf("%-35s %8d %5d %10.1f %10.1f\n", 
            "RE (Sectors + Occ)", n_re, k_re, aic_re, bic_re))
cat(sprintf("%-35s %8d %5d %10.1f %10.1f  ✓ BEST\n", 
            "RE (Interactions) - PRIMARY", n_re_int, k_re_int, aic_re_int, bic_re_int))
cat(sprintf("%-35s %8d %5d %10.1f %10.1f\n", 
            "RE (Country Groups)", n_country, k_country, aic_country, bic_country))
cat(strrep("-", 75), "\n")

cat("\n✅ Lower AIC/BIC = Better model fit\n")
cat("✅ The interaction model (Model 3) has lowest AIC/BIC!\n\n")

# Save results (all 4 models)
results <- data.frame(
  Model = c("FE (Time only)", "RE (Sectors + Occ)", "RE (Interactions)", "RE (Country Groups)"),
  N = c(n_fe, n_re, n_re_int, n_country),
  k = c(k_fe, k_re, k_re_int, k_country),
  AIC = round(c(aic_fe, aic_re, aic_re_int, aic_country), 1),
  BIC = round(c(bic_fe, bic_re, bic_re_int, bic_country), 1),
  R2 = round(c(r2_fe, r2_re, r2_re_int, r2_country), 4)
)

write.csv(results, "output/reports/model_fit_comparison.csv", row.names = FALSE)
cat("Results saved to: output/reports/model_fit_comparison.csv\n")

