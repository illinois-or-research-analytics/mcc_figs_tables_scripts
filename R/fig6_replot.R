library(data.table)
library(ggplot2)

summary_dt <- fread("/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/corrected_cartel_data/fig6_summary.csv")

# Convert calendar year to relative simulation year
summary_dt[, sim_year := syear - min(syear)]

model_order <- c("ctrl", "cartel-r", "cartel-p")
cond_order  <- c("restrictive", "relaxed")

summary_dt[, model     := factor(model,     levels = model_order)]
summary_dt[, condition := factor(condition, levels = cond_order)]
summary_dt[, cartel_size := factor(cartel_size,
                                   levels = c(5, 25, 125, 250, 500, 1000),
                                   labels = paste0("n=", c(5, 25, 125, 250, 500, 1000)))]

pal <- c(ctrl       = "black",
         `cartel-r` = "blue",
         `cartel-p` = "brown")

p <- ggplot(summary_dt,
            aes(x = sim_year, y = mean_cpa,
                color = model, fill = model)) +
  geom_ribbon(aes(ymin = mean_cpa - sd_cpa, ymax = mean_cpa + sd_cpa),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values  = pal) +
  scale_y_log10() +
  scale_x_continuous(limits = c(0, NA), breaks = c(0, 10, 20, 30)) +
  facet_grid(condition ~ cartel_size) +
  labs(
    x     = "Simulation year",
    y     = "Citations per cartel author (log10 scale)",
    color = "Model",
    fill  = "Model"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("fig6_citations_per_author_v4_replot.pdf", p, width = 16, height = 8)
ggsave("fig6_citations_per_author_v4_replot.png", p, width = 16, height = 8, dpi = 300)
ggsave("fig6_citations_per_author_v4_replot.eps", p, width = 16, height = 8, device = cairo_ps)
message("Saved: fig6_citations_per_author_v4_replot.pdf/.png/.eps")
