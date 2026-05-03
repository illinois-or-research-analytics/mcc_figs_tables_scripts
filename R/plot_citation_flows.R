library(data.table)
library(ggplot2)

dt <- fread("citation_flows_v2.csv")

# Factor ordering
dt[, model := factor(model, levels = c("ctrl", "cartel-r", "cartel-p"))]
dt[, cartel_size := factor(cartel_size,
                           levels = c(5, 25, 125, 250),
                           labels = c("Size 5", "Size 25", "Size 125", "Size 250"))]

# Average across reps
avg <- dt[,
  .(mean_rate = mean(citations_per_author),
    sd_rate   = sd(citations_per_author)),
  by = .(model, cartel_size, sim_year, target_cartel)]

model_colors <- c("ctrl" = "#444444", "cartel-r" = "#E69F00", "cartel-p" = "#56B4E9")

plot_panel <- function(data, title, subtitle, filename) {
  p <- ggplot(data, aes(x = sim_year, y = mean_rate,
                        color = model, group = model)) +
    geom_ribbon(aes(ymin = mean_rate - sd_rate,
                    ymax = mean_rate + sd_rate,
                    fill = model),
                alpha = 0.15, color = NA) +
    geom_line(linewidth = 0.8) +
    facet_wrap(~ cartel_size, scales = "fixed", ncol = 2) +
    scale_x_continuous(breaks = seq(0, 30, by = 5),
                       limits = c(0, 30)) +
    scale_y_continuous(limits = c(0, NA)) +
    scale_color_manual(values = model_colors) +
    scale_fill_manual(values = model_colors) +
    labs(
      x        = "Simulation year",
      y        = "Citations received per author",
      color    = "Model",
      fill     = "Model",
      title    = title,
      subtitle = subtitle
    ) +
    theme_bw(base_size = 12) +
    theme(
      legend.position  = "bottom",
      strip.background = element_rect(fill = "grey92"),
      panel.grid.minor = element_blank()
    )

  ggsave(filename, plot = p, width = 8, height = 6, dpi = 300)
  message("Saved: ", filename)
}

# --- Plot 1: cartel/control authors (target_cartel != -1) ---
plot_panel(
  data     = avg[target_cartel != -1],
  title    = "Annual citations received by cartel / control authors",
  subtitle = "mean \u00b1 SD across 3 replicates; year 1 = 1983, year 30 = 2012",
  filename = "cartel_citations_over_time.png")

# --- Plot 2: background authors (target_cartel == -1) ---
plot_panel(
  data     = avg[target_cartel == -1],
  title    = "Annual citations received by background authors",
  subtitle = "all sources \u2192 background; mean \u00b1 SD across 3 replicates; year 1 = 1983, year 30 = 2012",
  filename = "bg_citations_over_time.png"
)
