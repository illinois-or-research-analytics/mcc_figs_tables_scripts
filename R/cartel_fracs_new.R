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
        dt[, condition   := cond]
        dt[, model       := model_labels[[model]]]
        dt[, cartel_size := size]
        dt[, replicate   := rep]
        all_data[[length(all_data) + 1]] <- dt
      }
    }
  }
}

dt_all    <- rbindlist(all_data)
dt_agents <- dt_all[type == "agent"]

dt_cartel <- dt_agents[cartel_id != -1]
dt_bg     <- dt_agents[cartel_id == -1]

# 99th percentile of background pooled across conditions and replicates,
# per model x cartel_size
p99 <- dt_bg[, .(p99 = quantile(in_degree, 0.99)),
             by = .(model, cartel_size)]

# Fraction of cartel agents >= p99 per model x size x condition x replicate
dt_merged <- merge(dt_cartel, p99, by = c("model", "cartel_size"))

frac_rep <- dt_merged[, .(frac = mean(in_degree >= p99)),
                      by = .(model, cartel_size, condition, replicate)]

# Mean and SD across replicates for bar height and error bars
frac_mean <- frac_rep[, .(mean_frac = mean(frac), sd_frac = sd(frac)),
                      by = .(model, cartel_size, condition)]

model_order <- c("ctrl", "cartel-r", "cartel-p")
cond_order  <- c("restrictive", "relaxed")

frac_rep[,  model     := factor(model,     levels = model_order)]
frac_rep[,  condition := factor(condition, levels = cond_order)]
frac_mean[, model     := factor(model,     levels = model_order)]
frac_mean[, condition := factor(condition, levels = cond_order)]

pal <- c(restrictive = "#C0392B", relaxed = "#2980B9")

p <- ggplot(frac_mean, aes(x = factor(cartel_size), y = mean_frac, fill = condition)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6, alpha = 0.85) +
  geom_errorbar(aes(ymin = mean_frac - sd_frac, ymax = mean_frac + sd_frac),
                position = position_dodge(width = 0.7),
                width = 0.25, linewidth = 0.5, color = "black") +
  geom_hline(yintercept = 0.01, linetype = "dashed", color = "black", linewidth = 0.5) +
  scale_fill_manual(values = pal) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~ model, nrow = 1) +
  labs(
    x     = "Cartel size",
    y     = "fraction > q0.99",
    fill  = "Condition"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("cartel_fracs_p99.pdf", p, width = 12, height = 5)
ggsave("cartel_fracs_p99.png", p, width = 12, height = 5, dpi = 300)
ggsave("cartel_fracs_p99.eps", p, width = 12, height = 5, device = cairo_ps)
message("Saved: cartel_fracs_p99.pdf/.png/.eps")
