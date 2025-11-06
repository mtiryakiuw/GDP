library(dplyr)
library(ggplot2)
library(ggrepel)
library(scales)

# Load data
gap_panel2 <- read.csv("output/data/panel2_sector_occupation_4years.csv")

cat("=== CREATING SCATTER PLOT: Same Logic as Bar Chart ===\n\n")

# Calculate sector statistics (SAME as bar chart)
sector_detail <- gap_panel2 %>%
  group_by(sector) %>%
  summarise(
    mean_gap = mean(gender_pay_gap, na.rm = TRUE),
    sd_gap = sd(gender_pay_gap, na.rm = TRUE),
    n = n(),
    pct_negative = sum(gender_pay_gap < 0, na.rm = TRUE) / n() * 100,
    .groups = "drop"
  ) %>%
  arrange(desc(mean_gap), desc(pct_negative)) %>%  # SAME sorting
  mutate(
    gap_type = case_when(
      pct_negative >= 15 ~ "High Negative (≥15%)",
      pct_negative >= 10 ~ "Moderate Negative (10-15%)",
      TRUE ~ "Low Negative (<10%)"
    ),
    # NACE sector codes
    sector_code = case_when(
      sector == "Mining" ~ "B",
      sector == "Manufacturing" ~ "C",
      sector == "Electricity" ~ "D",
      sector == "Water" ~ "E",
      sector == "Construction" ~ "F",
      sector == "Trade" ~ "G",
      sector == "Transport" ~ "H",
      sector == "Hospitality" ~ "I",
      sector == "IT" ~ "J",
      sector == "Finance" ~ "K",
      sector == "Real Estate" ~ "L",
      sector == "Professional" ~ "M",
      sector == "Admin Services" ~ "N",
      sector == "Public Admin" ~ "O",
      sector == "Education Sector" ~ "P",
      sector == "Health" ~ "Q",
      sector == "Arts" ~ "R",
      sector == "Other Services" ~ "S"
    ),
    sector_label = paste0(sector_code, " - ", sector)
  )

cat("Sector data (same sorting as bar chart):\n")
print(sector_detail %>% select(sector_label, mean_gap, pct_negative, gap_type))

# Create scatter plot with SAME colors as bar chart
p_scatter <- ggplot(sector_detail, aes(x = pct_negative, y = mean_gap)) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "solid", color = "gray30", linewidth = 0.4) +
  geom_vline(xintercept = seq(0, 25, 5), linetype = "solid", color = "gray85", linewidth = 0.3) +
  
  # Points with SAME colors as bar chart
  geom_point(aes(color = gap_type), size = 2.5, alpha = 0.8) +
  
  # Labels
  geom_text_repel(
    aes(label = sector_label, color = gap_type),
    size = 2.8,
    fontface = "bold",
    box.padding = 0.35,
    point.padding = 0.3,
    segment.color = NA,
    max.overlaps = 30,
    force = 5,
    direction = "both",
    min.segment.length = 0,
    nudge_y = 0.5
  ) +
  
  # SAME color scale as bar chart
  scale_color_manual(
    values = c(
      "High Negative (≥15%)" = "#2ECC71",      # Green (SAME)
      "Moderate Negative (10-15%)" = "#F39C12", # Orange (SAME)
      "Low Negative (<10%)" = "#E74C3C"         # Red (SAME)
    ),
    name = "Negative Gap Incidence"
  ) +
  
  # Axes
  scale_x_continuous(
    breaks = seq(0, 25, 5),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 25),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_y_continuous(
    breaks = seq(0, 20, 2.5),
    labels = function(x) paste0(x, "%"),
    limits = c(0, 20),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  # Labels
  labs(
    title = "Gender Pay Gap by 18 NACE Rev. 2 Sectors: Two Dimensions",
    subtitle = "Each sector averaged across country-occupation-year observations",
    x = "Frequency of Contexts Where Women Out-Earn Men (%)\n← Consistent Male Advantage | More Female Advantage →",
    y = "Mean Gender Pay Gap (%)"
  ) +
  
  # Theme
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.3),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = "gray70", linewidth = 0.5),
    plot.background = element_rect(fill = "white", color = NA),
    axis.title.x = element_text(size = 9, color = "gray20", margin = margin(t = 8)),
    axis.title.y = element_text(size = 9, color = "gray20", margin = margin(r = 8)),
    axis.text = element_text(size = 8, color = "gray30"),
    plot.title = element_text(face = "bold", size = 11),
    plot.subtitle = element_text(color = "gray40", size = 9),
    plot.margin = margin(15, 15, 15, 15)
  )

# Save
ggsave("output/figures/18_sectors_scatter.png", p_scatter, 
       width = 10, height = 7, dpi = 300, bg = "white")

cat("\n✅ Scatter plot saved: output/figures/18_sectors_scatter.png\n")
cat("   - SAME data as bar chart\n")
cat("   - SAME color logic (Red/Orange/Green)\n")
cat("   - X-axis: % negative gaps\n")
cat("   - Y-axis: Mean gap\n")
cat("   - Shows two dimensions simultaneously!\n")