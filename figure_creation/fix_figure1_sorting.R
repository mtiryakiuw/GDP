# Fix Figure 1: Sort by Mean Gap (primary) AND Negative % (secondary)
# This ensures color gradient flows smoothly within each gap range
library(dplyr)
library(ggplot2)

# Load data
gap_panel2 <- read.csv("output/data/panel2_sector_occupation_4years.csv")

cat("=== FIXING FIGURE 1 SECTOR SORTING ===\n\n")

# Calculate sector statistics
sector_detail <- gap_panel2 %>%
  group_by(sector) %>%
  summarise(
    mean_gap = mean(gender_pay_gap, na.rm = TRUE),
    sd_gap = sd(gender_pay_gap, na.rm = TRUE),
    min_gap = min(gender_pay_gap, na.rm = TRUE),
    max_gap = max(gender_pay_gap, na.rm = TRUE),
    n = n(),
    pct_negative = sum(gender_pay_gap < 0, na.rm = TRUE) / n() * 100,
    .groups = "drop"
  ) %>%
  # PRIMARY SORT: Mean gap (descending) 
  # SECONDARY SORT: Negative % (DESCENDING) - ensures Red→Orange→Green gradient!
  arrange(desc(mean_gap), desc(pct_negative)) %>%  # ✅ İKİSİ DE DESCENDING
  mutate(
    gap_type = case_when(
      pct_negative >= 15 ~ "High Negative (≥15%)",
      pct_negative >= 10 ~ "Moderate Negative (10-15%)",
      TRUE ~ "Low Negative (<10%)"
    ),
    gap_category = case_when(
      mean_gap > 15 ~ "High-Gap (>15%)",
      mean_gap >= 12 ~ "Medium-Gap (12-15%)",
      TRUE ~ "Low-Gap (<12%)"
    )
  )

cat("Sector ordering (top to bottom in figure):\n")
print(sector_detail %>% select(sector, mean_gap, pct_negative, gap_type, gap_category))

# Prepare plot data with fixed factor levels
sector_plot_data <- sector_detail %>%
  mutate(sector = factor(sector, levels = rev(sector)))  # Rev for ggplot (bottom-up)

# Create improved plot
p1 <- ggplot(sector_plot_data, aes(x = mean_gap, y = sector, fill = gap_type)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", size = 0.5) +
  scale_fill_manual(
    values = c(
      "High Negative (≥15%)" = "#2ECC71",      # Green
      "Moderate Negative (10-15%)" = "#F39C12", # Orange
      "Low Negative (<10%)" = "#E74C3C"         # Red
    ),
    name = "Negative Gap Incidence"
  ) +
  labs(
    title = "Gender Pay Gap by 18 NACE Rev. 2 Sectors",
    subtitle = "Mean gap with negative gap incidence highlighting (2010-2022, sorted by mean gap then negative %)",
    x = "Mean Gender Pay Gap (%)",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

# Save fixed figure
ggsave("output/figures/18_sectors_detailed.png", p1, width = 10, height = 8, dpi = 300)

cat("\n✅ Fixed Figure 1 saved: output/figures/18_sectors_detailed.png\n")
cat("   Now sorted by: (1) Mean gap DESC, (2) Negative % DESC\n")
cat("   Color gradient should flow smoothly within gap categories!\n")