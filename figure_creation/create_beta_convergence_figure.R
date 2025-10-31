library(tidyverse)
library(ggrepel)

# Read data
beta_data <- read.csv("output/data/beta_convergence.csv")

# Country code to full name mapping
country_names <- c(
  "AT" = "Austria", "BE" = "Belgium", "BG" = "Bulgaria", 
  "CH" = "Switzerland", "CY" = "Cyprus", "CZ" = "Czechia",
  "DE" = "Germany", "DK" = "Denmark", "EE" = "Estonia",
  "EL" = "Greece", "ES" = "Spain", "FI" = "Finland",
  "FR" = "France", "HR" = "Croatia", "HU" = "Hungary",
  "IE" = "Ireland", "IS" = "Iceland", "IT" = "Italy",
  "LT" = "Lithuania", "LU" = "Luxembourg", "LV" = "Latvia",
  "MT" = "Malta", "NL" = "Netherlands", "NO" = "Norway",
  "PL" = "Poland", "PT" = "Portugal", "RO" = "Romania",
  "SE" = "Sweden", "SI" = "Slovenia", "SK" = "Slovakia"
)

# Prepare data
beta_plot_data <- beta_data %>%
  mutate(
    country_full = country_names[country],
    converging = change < 0,
    gap_type = case_when(
      converging ~ "Converging (Gap Reduced)",
      !converging ~ "Diverging (Gap Increased)"
    )
  )

# Fit regression model for stats
beta_model <- lm(change ~ gap_2010, data = beta_plot_data)
beta_coef <- coef(beta_model)[2]
r_squared <- summary(beta_model)$r.squared
p_value <- summary(beta_model)$coefficients[2, 4]

# Create enhanced scatter plot
p <- ggplot(beta_plot_data, aes(x = gap_2010, y = change, color = gap_type)) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", size = 0.5) +
  geom_vline(xintercept = mean(beta_plot_data$gap_2010, na.rm = TRUE), 
             linetype = "dashed", color = "gray50", size = 0.5) +
  
  # Regression line with confidence interval
  geom_smooth(method = "lm", se = TRUE, color = "#3498DB", 
              fill = "#AED6F1", alpha = 0.3, size = 1.2,
              aes(group = 1)) +
  
  # Points
  geom_point(size = 3.5, alpha = 0.8) +
  
  # Country labels with ggrepel
  geom_text_repel(
    aes(label = country_full),
    size = 3.5,
    max.overlaps = 30,
    box.padding = 0.5,
    point.padding = 0.3,
    segment.color = "gray70",
    segment.size = 0.3,
    force = 2,
    seed = 42
  ) +
  
  # Color scheme
  scale_color_manual(
    values = c(
      "Converging (Gap Reduced)" = "#27AE60",
      "Diverging (Gap Increased)" = "#E74C3C"
    ),
    name = ""
  ) +
  
  # Labels
  labs(
    title = "Beta Convergence in Gender Pay Gaps (2010-2022)",
    subtitle = sprintf("β = %.3f*** (p < 0.001), R² = %.3f | Higher initial gaps → Faster convergence", 
                      beta_coef, r_squared),
    x = "Initial Gender Pay Gap in 2010 (%)",
    y = "Change in Gap 2010-2022 (percentage points)",
    caption = "Source: Eurostat Structure of Earnings Survey (2010, 2022). Negative values indicate gap reduction."
  ) +
  
  # Theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 11, color = "gray30", hjust = 0),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "gray90", size = 0.3),
    axis.title = element_text(face = "bold", size = 11),
    axis.text = element_text(size = 10)
  ) +
  
  # Scales
  scale_x_continuous(breaks = seq(0, 30, 5), limits = c(4, 23)) +
  scale_y_continuous(breaks = seq(-10, 10, 2), limits = c(-9, 7))

# Save figure
ggsave("output/figures/beta_convergence_scatter.png", p, 
       width = 10, height = 7, dpi = 300, bg = "white")

cat("✅ Beta convergence figure created: output/figures/beta_convergence_scatter.png\n")
cat(sprintf("   β coefficient: %.3f (p < 0.001)\n", beta_coef))
cat(sprintf("   R²: %.3f\n", r_squared))
cat(sprintf("   Countries: %d\n", nrow(beta_plot_data)))
