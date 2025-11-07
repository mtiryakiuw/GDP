#!/usr/bin/env Rscript
# ==============================================================================
# Marginal Effects Visualization for Thesis Figure 4
# ==============================================================================
# Purpose: Create predicted gender pay gaps across sector-occupation combinations
#          with 95% confidence intervals for interaction interpretation
# Author: Thesis Chapter 6 Analysis
# Date: 2025-11-07
# ==============================================================================

library(plm)
library(ggplot2)
library(dplyr)
library(sandwich)

cat("\n=== MARGINAL EFFECTS FIGURE CREATION ===\n")
cat("Loading Panel 2 data...\n")

# Load cleaned data
data <- read.csv("output/data/panel2_sector_occupation_cleaned.csv", 
                 stringsAsFactors = FALSE)

# Filter outliers (consistent with main analysis)
data <- data %>%
  filter(gender_pay_gap >= -20 & gender_pay_gap <= 80)

cat(sprintf("Observations after outlier filtering: %d\n", nrow(data)))

# Create panel structure
pdata <- pdata.frame(data, index = c("panel_id", "year"))

# Estimate interaction model (matching Table 8)
cat("\nEstimating Random Effects model with interactions...\n")

model <- plm(gender_pay_gap ~ industry + construction + public_sector + 
               high_skill + managerial + 
               industry:high_skill + industry:managerial +
               public_sector:high_skill + public_sector:managerial +
               factor(year),
             data = pdata,
             model = "random")

# Extract coefficients and cluster-robust SE
coefs <- coef(model)
vcov_robust <- vcovHC(model, type = "HC1", cluster = "group")
se_robust <- sqrt(diag(vcov_robust))

cat("\nCoefficients extracted successfully\n")

# ==============================================================================
# Calculate predicted gaps for each sector-occupation combination
# ==============================================================================

# Reference: Services sector, non-high-skill, non-managerial (baseline)
baseline <- coefs["(Intercept)"]

# Define sectors
sectors <- c("Services", "Industry", "Construction", "Public Sector")

# Define occupations
occupations <- c("Non-High-Skill\nNon-Managerial", 
                 "High-Skill\nNon-Managerial",
                 "Managerial")

# Create prediction grid
predictions <- data.frame()

for(sector in sectors) {
  for(occupation in occupations) {
    
    # Initialize prediction
    pred <- baseline
    se_components <- c()
    
    # Sector effects
    if(sector == "Industry") {
      pred <- pred + coefs["industry"]
      se_components <- c("(Intercept)", "industry")
    } else if(sector == "Construction") {
      pred <- pred + coefs["construction"]
      se_components <- c("(Intercept)", "construction")
    } else if(sector == "Public Sector") {
      pred <- pred + coefs["public_sector"]
      se_components <- c("(Intercept)", "public_sector")
    } else {
      se_components <- c("(Intercept)")
    }
    
    # Occupation effects
    if(occupation == "High-Skill\nNon-Managerial") {
      pred <- pred + coefs["high_skill"]
      se_components <- c(se_components, "high_skill")
      
      # Interaction effects
      if(sector == "Industry") {
        pred <- pred + coefs["industry:high_skill"]
        se_components <- c(se_components, "industry:high_skill")
      } else if(sector == "Public Sector") {
        pred <- pred + coefs["public_sector:high_skill"]
        se_components <- c(se_components, "public_sector:high_skill")
      }
      
    } else if(occupation == "Managerial") {
      pred <- pred + coefs["managerial"]
      se_components <- c(se_components, "managerial")
      
      # Interaction effects
      if(sector == "Industry") {
        pred <- pred + coefs["industry:managerial"]
        se_components <- c(se_components, "industry:managerial")
      } else if(sector == "Public Sector") {
        pred <- pred + coefs["public_sector:managerial"]
        se_components <- c(se_components, "public_sector:managerial")
      }
    }
    
    # Calculate standard error using delta method (conservative: sum of variances)
    # More accurate would use full vcov matrix, but this is conservative
    se_pred <- sqrt(sum(vcov_robust[se_components, se_components] * 
                       outer(rep(1, length(se_components)), 
                             rep(1, length(se_components)))))
    
    # 95% confidence interval
    ci_lower <- pred - 1.96 * se_pred
    ci_upper <- pred + 1.96 * se_pred
    
    predictions <- rbind(predictions, data.frame(
      sector = sector,
      occupation = occupation,
      predicted_gap = pred,
      se = se_pred,
      ci_lower = ci_lower,
      ci_upper = ci_upper
    ))
  }
}

# Reorder sectors for logical display
predictions$sector <- factor(predictions$sector, 
                             levels = c("Public Sector", "Services", 
                                       "Construction", "Industry"))

predictions$occupation <- factor(predictions$occupation,
                                 levels = c("Non-High-Skill\nNon-Managerial",
                                           "High-Skill\nNon-Managerial",
                                           "Managerial"))

cat("\nPredicted gaps calculated for 12 combinations\n")
print(predictions)

# ==============================================================================
# Create visualization
# ==============================================================================

cat("\nCreating marginal effects figure...\n")

p <- ggplot(predictions, aes(x = occupation, y = predicted_gap, 
                              color = sector, group = sector)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.9) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, linewidth = 0.8, alpha = 0.6) +
  scale_color_manual(values = c("Public Sector" = "#2E7D32",    # Green
                                "Services" = "#1976D2",          # Blue
                                "Construction" = "#F57C00",      # Orange
                                "Industry" = "#C62828"),         # Red
                     name = "Sector") +
  labs(title = "Predicted Gender Pay Gaps by Sector and Occupation",
       subtitle = "Random Effects estimates with 95% confidence intervals (cluster-robust SE)",
       x = "Occupational Category",
       y = "Predicted Gender Pay Gap (percentage points)",
       caption = "Source: Own calculations based on Structure of Earnings Survey (Eurostat, 2010-2022).\nNote: Predictions control for year fixed effects at 2022 levels. Reference: Services sector baseline.") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray30", hjust = 0),
    plot.caption = element_text(size = 8, color = "gray50", hjust = 0, 
                                margin = margin(t = 10)),
    axis.title = element_text(face = "bold", size = 11),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 11),
    legend.text = element_text(size = 10),
    legend.background = element_rect(fill = "white", color = "gray80"),
    panel.grid.major = element_line(color = "gray90", linewidth = 0.5),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray80", fill = NA, linewidth = 0.8)
  ) +
  scale_y_continuous(breaks = seq(0, 20, 2.5), limits = c(0, 20))

# Save high-resolution figure
ggsave("output/figures/figure4_marginal_effects.pdf", 
       plot = p, width = 10, height = 6, units = "in", dpi = 300)

ggsave("output/figures/figure4_marginal_effects.png", 
       plot = p, width = 10, height = 6, units = "in", dpi = 300)

cat("\n✓ Figures saved:\n")
cat("  - output/figures/figure4_marginal_effects.pdf\n")
cat("  - output/figures/figure4_marginal_effects.png\n")

# ==============================================================================
# Export predictions table
# ==============================================================================

predictions_export <- predictions %>%
  mutate(
    predicted_gap = round(predicted_gap, 2),
    se = round(se, 3),
    ci_lower = round(ci_lower, 2),
    ci_upper = round(ci_upper, 2),
    ci_display = paste0("[", ci_lower, ", ", ci_upper, "]")
  ) %>%
  select(sector, occupation, predicted_gap, se, ci_display)

write.csv(predictions_export, 
          "output/data/marginal_effects_predictions.csv",
          row.names = FALSE)

cat("\n✓ Predictions table saved: output/data/marginal_effects_predictions.csv\n")

# ==============================================================================
# Key insights for interpretation
# ==============================================================================

cat("\n=== KEY INSIGHTS FOR THESIS INTERPRETATION ===\n\n")

# Compare Industry vs Public Sector at managerial level
industry_manager <- predictions %>% 
  filter(sector == "Industry", occupation == "Managerial") %>%
  pull(predicted_gap)

public_manager <- predictions %>% 
  filter(sector == "Public Sector", occupation == "Managerial") %>%
  pull(predicted_gap)

cat(sprintf("1. Managerial Gap Comparison:\n"))
cat(sprintf("   Industry Managers:      %.2f pp\n", industry_manager))
cat(sprintf("   Public Sector Managers: %.2f pp\n", public_manager))
cat(sprintf("   Difference:             %.2f pp\n\n", industry_manager - public_manager))

# Compare high-skill gaps
industry_highskill <- predictions %>% 
  filter(sector == "Industry", occupation == "High-Skill\nNon-Managerial") %>%
  pull(predicted_gap)

public_highskill <- predictions %>% 
  filter(sector == "Public Sector", occupation == "High-Skill\nNon-Managerial") %>%
  pull(predicted_gap)

cat(sprintf("2. High-Skill Gap Comparison:\n"))
cat(sprintf("   Industry High-Skill:      %.2f pp\n", industry_highskill))
cat(sprintf("   Public Sector High-Skill: %.2f pp\n", public_highskill))
cat(sprintf("   Difference:               %.2f pp\n\n", industry_highskill - public_highskill))

# Occupational gradient within sectors
industry_base <- predictions %>% 
  filter(sector == "Industry", occupation == "Non-High-Skill\nNon-Managerial") %>%
  pull(predicted_gap)

cat(sprintf("3. Occupational Gradient in Industry:\n"))
cat(sprintf("   Non-High-Skill:  %.2f pp (baseline)\n", industry_base))
cat(sprintf("   High-Skill:      %.2f pp (%.2f pp difference)\n", 
            industry_highskill, industry_highskill - industry_base))
cat(sprintf("   Managerial:      %.2f pp (%.2f pp difference)\n\n", 
            industry_manager, industry_manager - industry_base))

public_base <- predictions %>% 
  filter(sector == "Public Sector", occupation == "Non-High-Skill\nNon-Managerial") %>%
  pull(predicted_gap)

cat(sprintf("4. Occupational Gradient in Public Sector:\n"))
cat(sprintf("   Non-High-Skill:  %.2f pp (baseline)\n", public_base))
cat(sprintf("   High-Skill:      %.2f pp (%.2f pp difference)\n", 
            public_highskill, public_highskill - public_base))
cat(sprintf("   Managerial:      %.2f pp (%.2f pp difference)\n\n", 
            public_manager, public_manager - public_base))

cat("=== MARGINAL EFFECTS FIGURE CREATION COMPLETE ===\n")
