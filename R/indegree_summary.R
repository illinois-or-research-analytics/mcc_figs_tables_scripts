library(data.table)
library(ggplot2)
library(xtable)

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

# Assign group labels
dt_all[, group := fcase(
  cartel_id == -1,                         "background",
  cartel_id != -1 & model == "ctrl",       "ctrl",
  cartel_id != -1 & model == "cartel-r",   "cartel-r",
  cartel_id != -1 & model == "cartel-p",   "cartel-p",
  default = NA_character_
)]

dt_all <- dt_all[!is.na(group)]

# Save intermediate data
fwrite(dt_all, "indegree_all_agents.csv")
message("Saved: indegree_all_agents.csv")

# -------------------------------------------------------------------
# 1. Summary table by condition x cartel_size x group
# -------------------------------------------------------------------
summary_tbl <- dt_all[, .(
  N      = .N,
  min    = min(in_degree),
  Q1     = quantile(in_degree, 0.25),
  median = quantile(in_degree, 0.50),
  Q3     = quantile(in_degree, 0.75),
  Q90    = quantile(in_degree, 0.90),
  Q99    = quantile(in_degree, 0.99),
  max    = max(in_degree)
), by = .(condition, cartel_size, group)]

group_order <- c("background", "ctrl", "cartel-r", "cartel-p")
cond_order  <- c("restrictive", "relaxed")

summary_tbl[, group      := factor(group,      levels = group_order)]
summary_tbl[, condition  := factor(condition,  levels = cond_order)]
summary_tbl[, cartel_size := factor(cartel_size,
                                     levels = c(5, 25, 125, 250, 500, 1000),
                                     labels = paste0("n=", c(5, 25, 125, 250, 500, 1000)))]
setorder(summary_tbl, condition, cartel_size, group)

fwrite(summary_tbl, "indegree_summary_table.csv")
message("Saved: indegree_summary_table.csv")

xt <- xtable(as.data.frame(summary_tbl),
             caption = "In-degree summary statistics by condition, cartel size, and group. Replicates are pooled within each cell.",
             label   = "tab:indegree_summary",
             digits  = 0)
print(xt, include.rownames = FALSE, booktabs = TRUE,
      file = "indegree_summary_table.tex")
message("Saved: indegree_summary_table.tex")

# -------------------------------------------------------------------
# 2. Boxplot: agents with in_degree > 1000
#    Three boxes per cell: background, cartel-r, cartel-p
# -------------------------------------------------------------------
dt_box <- dt_all[in_degree > 1000 &
                 group %in% c("background", "cartel-r", "cartel-p")]

dt_box[, group      := factor(group,      levels = c("background", "cartel-r", "cartel-p"))]
dt_box[, condition  := factor(condition,  levels = cond_order)]
dt_box[, cartel_size := factor(cartel_size,
                                levels = c(5, 25, 125, 250, 500, 1000),
                                labels = paste0("n=", c(5, 25, 125, 250, 500, 1000)))]

pal <- c(background = "grey60",
         `cartel-r` = "blue",
         `cartel-p` = "brown")

p <- ggplot(dt_box, aes(x = group, y = in_degree, fill = group)) +
  geom_boxplot(outlier.size = 0.3, outlier.alpha = 0.2, linewidth = 0.4) +
  scale_fill_manual(values = pal) +
  scale_y_log10(labels = scales::comma) +
  facet_grid(condition ~ cartel_size) +
  labs(
    x    = NULL,
    y    = "In-degree (log10 scale)",
    fill = "Group"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x      = element_text(angle = 30, hjust = 1),
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

ggsave("indegree_gt1000_boxplot.pdf", p, width = 16, height = 7)
ggsave("indegree_gt1000_boxplot.png", p, width = 16, height = 7, dpi = 300)
ggsave("indegree_gt1000_boxplot.eps", p, width = 16, height = 7, device = cairo_ps)
message("Saved: indegree_gt1000_boxplot.pdf/.png/.eps")
