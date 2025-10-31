library(tidyverse)

# Read data
country_group_stats <- read.csv("output/data/country_group_statistics.csv")

# Prepare data - exclude Unclassified
country_group_plot <- country_group_stats %>%
  filter(country_group != "Unclassified") %>%
  arrange(desc(mean_gap)) %>%  # Sort from highest to lowest
  mutate(
    country_group = factor(country_group, levels = country_group),
    se = sd_gap / sqrt(n_obs),  # Standard error
    label_text = sprintf("%.1f%%", mean_gap),  # Format label
    # Determine text color based on bar size (white for long bars, dark for short)
    text_color = ifelse(mean_gap > 10, "white", "gray20")
  )

# Calculate overall mean
overall_mean <- mean(country_group_plot$mean_gap)

# Create clean bar chart with embedded values
p <- ggplot(country_group_plot, aes(x = mean_gap, y = country_group)) +
  # Vertical line for overall mean
  geom_vline(xintercept = overall_mean, 
             linetype = "dashed", color = "gray60", linewidth = 0.4) +
  
  # Bars
  geom_col(aes(fill = country_group), width = 0.65, show.legend = FALSE) +
  
  # Embedded value labels inside bars
  geom_text(aes(label = label_text, color = text_color), 
            hjust = 1.1, size = 4, fontface = "bold",
            show.legend = FALSE) +
  
  # Error bars (standard errors)
  geom_errorbarh(aes(xmin = mean_gap - se, xmax = mean_gap + se),
                 height = 0.25, color = "gray40", linewidth = 0.4) +
  
  # Manual color scheme
  scale_fill_manual(
    values = c(
      "Liberal" = "#E74C3C",
      "Mediterranean" = "#F39C12", 
      "Continental" = "#F4D03F",
      "Nordic" = "#52BE80",
      "Eastern" = "#5DADE2",
      "Other" = "#AF7AC5",
      "Balkans" = "#48C9B0"
    )
  ) +
  
  scale_color_identity() +  # Use text_color as is
  
  # Scales
  scale_x_continuous(
    breaks = seq(0, 20, 5),
    limits = c(0, 18.5),
    expand = c(0, 0)
  ) +
  
  # Labels
  labs(
    title = "Gender Pay Gap by Country Group (2010-2022)",
    subtitle = sprintf("Error bars show standard errors | Dashed line: overall mean (%.1f%%)", overall_mean),
    x = "Mean Gender Pay Gap (%)",
    y = NULL,
    caption = "Source: Eurostat Structure of Earnings Survey. Groups based on welfare regime classification."
  ) +
  
  # Clean minimal theme
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0),
    plot.subtitle = element_text(size = 10, color = "gray40", hjust = 0),
    plot.caption = element_text(size = 9, color = "gray50", hjust = 0),
    axis.title.x = element_text(face = "bold", size = 11, margin = margin(t = 10)),
    axis.title.y = element_blank(),
    axis.text.y = element_text(size = 11, face = "bold"),
    axis.text.x = element_text(size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(15, 15, 15, 15)
  )

# Save figure
ggsave("output/figures/country_groups_comparison.png", p, 
       width = 10, height = 6, dpi = 300, bg = "white")

cat("✅ Country groups figure created: output/figures/country_groups_comparison.png\n")
cat(sprintf("   Groups: %d (excluding Unclassified)\n", nrow(country_group_plot)))
cat(sprintf("   Overall mean: %.2f%%\n", overall_mean))
cat(sprintf("   Range: %.2f%% (Balkans) to %.2f%% (Liberal)\n", 
            min(country_group_plot$mean_gap), max(country_group_plot$mean_gap)))
