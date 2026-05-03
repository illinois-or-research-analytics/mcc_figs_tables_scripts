library(data.table)
library(ggplot2)

# --- Caption text ---
# Figure X. Fold enrichment of cartel and control authors in the top 1% of
# agent paper citations, relative to random expectation. Rows: model condition
# (ctrl, cartel-r, cartel-p). Columns: cartel size (5, 25, 125, 250).
# Red: restrictive condition (clonal cartel members).
# Blue: relaxed condition (heterogeneous phenotype weights).
# Dashed line at fold = 1 indicates random expectation. Values above 1
# indicate overrepresentation in the top citation tier. Fold enrichment
# computed as observed fraction / 0.01. Thresholds derived from the pooled
# in-degree distribution of all agent nodes across 3 replicates.

# --- Load data ---
dt <- fread("~/cartel_top_pct_combined.csv")

# Fold enrichment for top 1% only
dt[, fold_top1 := frac_top_1pct / 0.01]

# Factor ordering
dt[, model := factor(model, levels = c("ctrl", "cartel-r", "cartel-p"))]
dt[, cartel_size := factor(cartel_size,
                           levels = c(5, 25, 125, 250),
                           labels = c("Size 5", "Size 25",
                                      "Size 125", "Size 250"))]
dt[, condition := factor(condition,
                         levels = c("restrictive", "relaxed"),
                         labels = c("Restrictive", "Relaxed"))]

cond_colors <- c("Restrictive" = "#E64B35", "Relaxed" = "#4DBBD5")

p <- ggplot(dt, aes(x = condition, y = fold_top1, fill = condition)) +
  geom_hline(yintercept = 1, linetype = "dashed",
             color = "grey40", linewidth = 0.5) +
  geom_col(width = 0.6) +
  facet_grid(model ~ cartel_size) +
  scale_fill_manual(values = cond_colors) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x    = "Condition",
    y    = "Fold enrichment over random expectation",
    fill = "Condition"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_blank(),
    axis.ticks.x     = element_blank()
  )

ggsave("cartel_top1pct_bars.png",
       plot = p, width = 10, height = 7, dpi = 300)
message("Saved: cartel_top1pct_bars.png")

# --- Vector formats (uncomment to use) ---
ggsave("cartel_top1pct_bars.pdf",
       plot = p, width = 10, height = 7, device = "pdf")
ggsave("cartel_top1pct_bars.eps",
       plot = p, width = 10, height = 7, device = "eps")
