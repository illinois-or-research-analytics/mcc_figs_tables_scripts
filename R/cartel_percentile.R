library(data.table)
library(ggplot2)

root_18 <- "/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/3_18_min_cartel_experiments"
root_22 <- "/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/3_22_min_cartel_experiments"

models     <- c("control_model", "null_model", "phenotype_model")
sizes      <- c(5, 25, 125, 250, 500, 1000)
proportion <- "proportion_0.50"
reps       <- c("r0", "r1", "r2")

model_labels <- c(
  control_model   = "ctrl",
  null_model      = "cartel-r",
  phenotype_model = "cartel-p"
)

conditions <- list(
  restrictive = root_18,
  relaxed     = root_22
)

all_data <- list()

for (cond in names(conditions)) {
  root <- conditions[[cond]]
  for (model in models) {
    for (size in sizes) {
      for (rep in reps) {
        path <- file.path(root, model, paste0("size_", size),
                          proportion, rep, "output", "nodelist.csv")
        if (!file.exists(path)) {
          warning(paste("Missing:", path))
          next
        }
        dt <- fread(path, select = c("type", "in_degree", "cartel_id"))
        dt <- dt[type == "agent"]
        dt[, condition   := cond]
        dt[, model       := model_labels[[model]]]
        dt[, cartel_size := size]
        dt[, replicate   := rep]
        all_data[[length(all_data) + 1]] <- dt
        message(sprintf("Done: %s | %s | size_%s | %s", cond, model, size, rep))
      }
    }
  }
}

dt_all <- rbindlist(all_data)
dt_all[, in_degree := as.numeric(in_degree)]
dt_all[in_degree == 0, in_degree := 0.1]

# Cartel agents keep their model label; background pooled across models
dt_cartel <- dt_all[cartel_id != -1, .(in_degree, condition, cartel_size, group = model)]
dt_bg     <- dt_all[cartel_id == -1, .(in_degree, condition, cartel_size, group = "background")]

dt_combined <- rbind(dt_cartel, dt_bg)

# Compute percentiles at 1% grid, pooled across replicates
probs  <- seq(0, 1, by = 0.01)

pct_dt <- dt_combined[, .(
  percentile = probs * 100,
  value      = quantile(in_degree, probs = probs)
), by = .(condition, cartel_size, group)]

# Save summarized data for replotting
fwrite(pct_dt, "cartel_percentile_summary.csv")
message("Saved: cartel_percentile_summary.csv")

# Factor ordering
group_order <- c("ctrl", "cartel-r", "cartel-p", "background")
cond_order  <- c("restrictive", "relaxed")

pct_dt[, group     := factor(group,     levels = group_order)]
pct_dt[, condition := factor(condition, levels = cond_order)]
pct_dt[, cartel_size := factor(cartel_size,
                                levels = c(5, 25, 125, 250, 500, 1000),
                                labels = paste0("n=", c(5, 25, 125, 250, 500, 1000)))]

pal <- c(ctrl       = "black",
         `cartel-r` = "blue",
         `cartel-p` = "brown",
         background = "grey60")

p <- ggplot(pct_dt, aes(x = percentile, y = value, color = group)) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = pal) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) +
  facet_grid(condition ~ cartel_size) +
  labs(
    x     = "Percentile",
    y     = "In-degree (log10 scale)",
    color = "Group"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("cartel_percentile_plot.pdf", p, width = 16, height = 6)
ggsave("cartel_percentile_plot.png", p, width = 16, height = 6, dpi = 300)
ggsave("cartel_percentile_plot.eps", p, width = 16, height = 6, device = cairo_ps)
message("Saved: cartel_percentile_plot.pdf/.png/.eps")
