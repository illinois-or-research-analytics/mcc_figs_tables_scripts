# GC edited Claude code. 3/23
# 8 panel plot for cartel exp. 3 reps per condition averaged over authors

library(data.table)
library(ggplot2)

# --- Caption text ---
#
# Main figure (cartel_citations_8panel.png):
# Figure X. Annual citations received per cartel and control author over
# simulation time. Rows: restrictive (clonal cartel members, top) and relaxed
# (heterogeneous phenotype weights, bottom) conditions. Columns: cartel sizes
# 5, 25, 125, and 250. Lines show the mean across 3 replicates; ribbons show
# +/- 1 SD. Year 1 = 1983, year 30 = 2012. Y-axis is log10 scale, fixed
# across all panels. Models: ctrl = control (no coordinated citing);
# cartel-r = random cartel citing; cartel-p = phenotype-matched cartel citing.
#
# Supplementary figure (bg_citations_8panel.png):
# Figure SX. Annual citations received per background author over simulation
# time (all sources to background). Layout, axes, and legend as in Figure X.
# The near-identical traces across all conditions confirm that cartel activity
# has no detectable effect on citation rates of non-cartel authors.

# --- Load and prep ---
model_colors <- c("ctrl" = "#444444", "cartel-r" = "#E69F00", "cartel-p" = "#56B4E9")

prep <- function(file, condition) {
  dt <- fread(file)
  dt[, model := factor(model, levels = c("ctrl", "cartel-r", "cartel-p"))]
  dt[, cartel_size := factor(cartel_size,
                             levels = c(5, 25, 125, 250),
                             labels = c("Size 5", "Size 25",
                                        "Size 125", "Size 250"))]
  avg <- dt[,
    .(mean_rate = mean(citations_per_author),
      sd_rate   = sd(citations_per_author)),
    by = .(model, cartel_size, sim_year, target_cartel)]
  avg[, condition := condition]
  avg
}

restr <- prep("citation_flows_restr.csv", "Restrictive")
relax <- prep("citation_flows_relax.csv", "Relaxed")
avg   <- rbindlist(list(restr, relax))

avg[, condition := factor(condition, levels = c("Restrictive", "Relaxed"))]

make_plot <- function(data, author_filter, y_label) {
  dat <- data[target_cartel == author_filter]
  ggplot(dat, aes(x = sim_year, y = mean_rate,
                  color = model, group = model)) +
    geom_ribbon(aes(ymin = mean_rate - sd_rate,
                    ymax = mean_rate + sd_rate,
                    fill = model),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.7) +
    facet_grid(condition ~ cartel_size) +
    scale_x_continuous(breaks = seq(0, 30, by = 10), limits = c(0, 30)) +
    scale_y_log10(limits = c(0.01, 1000),
                  breaks  = c(0.01, 0.1, 1, 10, 100, 1000),
                  labels  = c("0.01", "0.1", "1", "10", "100", "1000")) +
    scale_color_manual(values = model_colors) +
    scale_fill_manual(values  = model_colors) +
    guides(fill = "none") +
    labs(
      x     = "Simulation year",
      y     = y_label,
      color = "Model"
    ) +
    theme_bw(base_size = 14) +
    theme(
      legend.position  = "bottom",
      strip.background = element_rect(fill = "grey92"),
      panel.grid.minor = element_blank()
    )
}

# --- Main figure: cartel / control authors (target_cartel != -1) ---
# Note: cartel_id is 0 (ctrl) or 1 (cartel-r/p); both != -1
p_cartel <- make_plot(avg,
                      author_filter = 1,
                      y_label = "Citations received per author (log10)")

# Also include ctrl (cartel_id == 0) lines — need to combine both
# cartel_id values for the top panels
dat_cartel <- avg[target_cartel != -1]
p_cartel <- ggplot(dat_cartel, aes(x = sim_year, y = mean_rate,
                                   color = model, group = model)) +
  geom_ribbon(aes(ymin = mean_rate - sd_rate,
                  ymax = mean_rate + sd_rate,
                  fill = model),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.7) +
  facet_grid(condition ~ cartel_size) +
  scale_x_continuous(breaks = seq(0, 30, by = 10), limits = c(0, 30)) +
  scale_y_log10(limits = c(0.01, 1000),
                breaks  = c(0.01, 0.1, 1, 10, 100, 1000),
                labels  = c("0.01", "0.1", "1", "10", "100", "1000")) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values  = model_colors) +
  guides(fill = "none") +
  labs(
    x     = "Simulation year",
    y     = "Citations received per author (log10)",
    color = "Model"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("cartel_citations_8panel.png",
       plot = p_cartel, width = 14, height = 7, dpi = 300)
message("Saved: cartel_citations_8panel.png")

# --- Supplementary figure: background authors (target_cartel == -1) ---
dat_bg <- avg[target_cartel == -1]
p_bg <- ggplot(dat_bg, aes(x = sim_year, y = mean_rate,
                            color = model, group = model)) +
  geom_ribbon(aes(ymin = mean_rate - sd_rate,
                  ymax = mean_rate + sd_rate,
                  fill = model),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.7) +
  facet_grid(condition ~ cartel_size) +
  scale_x_continuous(breaks = seq(0, 30, by = 10), limits = c(0, 30)) +
  scale_y_log10(limits = c(0.01, 1000),
                breaks  = c(0.01, 0.1, 1, 10, 100, 1000),
                labels  = c("0.01", "0.1", "1", "10", "100", "1000")) +
  scale_color_manual(values = model_colors) +
  scale_fill_manual(values  = model_colors) +
  guides(fill = "none") +
  labs(
    x     = "Simulation year",
    y     = "Citations received per author (log10)",
    color = "Model"
  ) +
  theme_bw(base_size = 14) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("bg_citations_8panel.png",
       plot = p_bg, width = 14, height = 7, dpi = 300)
message("Saved: bg_citations_8panel.png")

# --- Vector formats (uncomment to use) ---
ggsave("cartel_citations_8panel.eps",
        plot = p_cartel, width = 14, height = 7,
        device = "eps")
ggsave("cartel_citations_8panel.pdf",
        plot = p_cartel, width = 14, height = 7,
        device = "pdf")
ggsave("bg_citations_8panel.eps",
        plot = p_bg, width = 14, height = 7,
        device = "eps")
ggsave("bg_citations_8panel.pdf",
        plot = p_bg, width = 14, height = 7,
        device = "pdf")
