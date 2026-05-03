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
        all_data[[length(all_data) + 1]] <- dt
      }
    }
  }
}

dt_all    <- rbindlist(all_data)
dt_agents <- dt_all[type == "agent"]

dt_cartel <- dt_agents[cartel_id != -1]
dt_bg     <- dt_agents[cartel_id == -1]

# Replace zero in_degree with 0.1 so they appear on log scale
dt_cartel[, in_degree := as.numeric(in_degree)]
dt_cartel[in_degree == 0, in_degree := 0.1]

# Single reference line per cell: pool background across both conditions
p99 <- dt_bg[, .(p99 = quantile(in_degree, 0.99)),
             by = .(model, cartel_size)]

model_order <- c("ctrl", "cartel-r", "cartel-p")
cond_order  <- c("restrictive", "relaxed")

dt_cartel[, model     := factor(model,     levels = model_order)]
dt_cartel[, condition := factor(condition, levels = cond_order)]
p99[,        model     := factor(model,     levels = model_order)]

pal <- c(restrictive = "#C0392B", relaxed = "#2980B9")

p <- ggplot(dt_cartel, aes(x = condition, y = in_degree, fill = condition)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.3, linewidth = 0.4) +
  geom_hline(data = p99,
             aes(yintercept = p99),
             color = "black", linetype = "dashed", linewidth = 0.6) +
  scale_fill_manual(values = pal) +
  scale_y_log10(labels = scales::comma) +
  facet_grid(model ~ cartel_size,
             labeller = labeller(cartel_size = function(x) paste0("n=", x))) +
  labs(
    x     = NULL,
    y     = "In-degree (log10 scale)",
    fill  = "Condition",
    title = "In-degree of cartel agents by model and cartel size"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 30, hjust = 1),
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

out <- "cartel_indegree_boxplots.pdf"
ggsave(out, p, width = 16, height = 7)
message("Saved: ", out)
