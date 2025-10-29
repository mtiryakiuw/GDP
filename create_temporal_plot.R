library(tidyverse)
library(ggplot2)

gap_panel2 <- read.csv("output/data/panel2_sector_occupation_4years.csv")

country_groups <- list(
  Nordic = c("DK", "FI", "IS", "NO", "SE"),
  Continental = c("AT", "BE", "DE", "FR", "LU", "NL", "CH", "LI"),
  Mediterranean = c("CY", "EL", "ES", "IT", "MT", "PT"),
  Eastern = c("BG", "CZ", "EE", "HR", "HU", "LT", "LV", "PL", "RO", "SI", "SK"),
  Liberal = c("IE", "UK"),
  Balkans = c("AL", "BA", "ME", "MK", "RS", "XK")
)

country_group_map <- data.frame(
  country = unlist(country_groups),
  country_group = rep(names(country_groups), sapply(country_groups, length)),
  stringsAsFactors = FALSE
)

temporal_trends <- gap_panel2 %>%
  left_join(country_group_map, by = "country") %>%
  filter(!is.na(country_group)) %>%
  group_by(year, country_group) %>%
  summarise(
    mean_gap = mean(gender_pay_gap, na.rm = TRUE),
    se_gap = sd(gender_pay_gap, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  )

theme_set(theme_minimal(base_size = 12))

p5 <- ggplot(temporal_trends, aes(x = year, y = mean_gap, color = country_group, group = country_group)) +
  geom_line(linewidth = 1.2, alpha = 0.8) +
  geom_point(size = 3, alpha = 0.8) +
  geom_ribbon(aes(ymin = mean_gap - se_gap, ymax = mean_gap + se_gap, fill = country_group),
              alpha = 0.2, color = NA) +
  scale_color_brewer(palette = "Set2", name = "Country Group") +
  scale_fill_brewer(palette = "Set2", name = "Country Group") +
  scale_x_continuous(breaks = c(2010, 2014, 2018, 2022)) +
  labs(
    title = "Temporal Evolution of Gender Pay Gap by Country Group",
    subtitle = "Mean gap with standard error bands (2010-2022)",
    x = "Year",
    y = "Mean Gender Pay Gap (%)",
    caption = "All groups show convergence trends over time"
  ) +
  theme(legend.position = "bottom")

ggsave("output/figures/temporal_trends_country_groups.png", p5, width = 10, height = 6, dpi = 300)
cat("✅ Saved: output/figures/temporal_trends_country_groups.png\n")
