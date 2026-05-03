library(data.table)
library(ggplot2)

# --- Load data ---
dt <- fread("~/cartel_top_pct_combined.csv")

# --- Reshape to long format ---
# Keep frac columns, melt by threshold
long <- melt(dt,
             id.vars       = c("condition", "model", "cartel_size"),
             measure.vars  = c("frac_top_1pct", "frac_top_0.1pct", "frac_top_0.01pct"),
             variable.name = "threshold",
             value.name    = "frac")

# Random baselines per threshold
baselines <- c(
  frac_top_1pct    = 0.01,
  `frac_top_0.1pct`  = 0.001,
  `frac_top_0.01pct` = 0.0001
)

long[, baseline := baselines[as.character(threshold)]]

# Fold enrichment over random baseline
# Zero fractions get fold = 0
long[, fold := fifelse(frac == 0, 0, frac / baseline)]

# Clean threshold labels for display
long[, threshold := factor(threshold,
                           levels = c("frac_top_1pct",
                                      "frac_top_0.1pct",
                                      "frac_top_0.01pct"),
                           labels = c("Top 1%", "Top 0.1%", "Top 0.01%"))]

# Factor ordering
long[, model := factor(model, levels = c("ctrl", "cartel-r", "cartel-p"))]
long[, cartel_size := factor(cartel_size,
                             levels = c(5, 25, 125, 250),
                             labels = c("Size 5", "Size 25",
                                        "Size 125", "Size 250"))]
long[, condition := factor(condition,
                           levels = c("restrictive", "relaxed"),
                           labels = c("Restrictive", "Relaxed"))]

# Colors for condition
cond_colors <- c("Restrictive" = "#E64B35", "Relaxed" = "#4DBBD5")

# Dodge width
dodge <- position_dodge(width = 0.5)

# --- Plot ---
p <- ggplot(long, aes(x = threshold, y = fold,
                      color = condition, group = condition)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "grey40", linewidth = 0.5) +
  geom_linerange(aes(ymin = 0, ymax = fold),
                 position = dodge, linewidth = 0.7) +
  geom_point(position = dodge, size = 2.5) +
  facet_grid(model ~ cartel_size) +
  scale_color_manual(values = cond_colors) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x     = "Citation threshold",
    y     = "Fold enrichment over random expectation",
    color = "Condition"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 30, hjust = 1)
  )

ggsave("cartel_top_pct_lollipop.png",
       plot = p, width = 12, height = 8, dpi = 300)
message("Saved: cartel_top_pct_lollipop.png")

# --- Vector formats (uncomment to use) ---
# ggsave("cartel_top_pct_lollipop.pdf",
#        plot = p, width = 12, height = 8, device = "pdf")
# ggsave("cartel_top_pct_lollipop.eps",
#        plot = p, width = 12, height = 8, device = "eps")
