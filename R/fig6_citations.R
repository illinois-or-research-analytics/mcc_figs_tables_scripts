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
        base <- file.path(root, model, paste0("size_", size),
                          proportion, rep, "output")
        el_path <- file.path(base, "edgelist.csv")
        nl_path <- file.path(base, "nodelist.csv")

        if (!file.exists(el_path) || !file.exists(nl_path)) {
          warning(paste("Missing files:", cond, model, size, rep))
          next
        }

        nl <- fread(nl_path, select = c("node_id", "type", "year",
                                        "author_id", "cartel_id"))
        el <- fread(el_path)

        # Agents only; identify cartel authors
        agents     <- nl[type == "agent"]
        cartel_authors <- agents[cartel_id != -1, unique(author_id)]

        if (length(cartel_authors) == 0) next

        # For each citation edge, get the target's year and author
        target_info <- nl[, .(node_id, tyear = year, tauthor_id = author_id)]
        el_ann <- merge(el, target_info, by.x = "target", by.y = "node_id")

        # Citations received by cartel authors, by target publication year
        cartel_cites <- el_ann[tauthor_id %in% cartel_authors,
                               .N, by = .(tauthor_id, tyear)]

        cartel_cites[, condition   := cond]
        cartel_cites[, model       := model_labels[[model]]]
        cartel_cites[, cartel_size := size]
        cartel_cites[, replicate   := rep]
        cartel_cites[, n_cartel_authors := length(cartel_authors)]

        all_data[[length(all_data) + 1]] <- cartel_cites
      }
    }
  }
}

dt <- rbindlist(all_data)

# Citations per cartel author per year, averaged across cartel authors
# then summarised across replicates
dt[, cites_per_author := N / n_cartel_authors]

# Mean per year per replicate (across cartel authors already done above)
rep_yr <- dt[, .(cites_per_author = sum(N) / unique(n_cartel_authors)),
             by = .(condition, model, cartel_size, replicate, tyear)]

# Mean and SD across replicates
summary_dt <- rep_yr[, .(mean_cpa = mean(cites_per_author),
                          sd_cpa   = sd(cites_per_author)),
                     by = .(condition, model, cartel_size, tyear)]

model_order <- c("ctrl", "cartel-r", "cartel-p")
cond_order  <- c("restrictive", "relaxed")

summary_dt[, model     := factor(model,     levels = model_order)]
summary_dt[, condition := factor(condition, levels = cond_order)]

pal <- c(restrictive = "#C0392B", relaxed = "#2980B9")

p <- ggplot(summary_dt,
            aes(x = tyear, y = mean_cpa, color = condition, fill = condition)) +
  geom_ribbon(aes(ymin = mean_cpa - sd_cpa, ymax = mean_cpa + sd_cpa),
              alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = pal) +
  scale_fill_manual(values  = pal) +
  scale_y_log10() +
  scale_x_continuous(breaks = c(0, 10, 20, 30)) +
  facet_grid(model ~ cartel_size,
             labeller = labeller(cartel_size = function(x) paste0("n=", x))) +
  labs(
    x     = "Simulation year",
    y     = "Citations per cartel author (log10)",
    color = "Condition",
    fill  = "Condition"
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position  = "bottom",
    strip.background = element_rect(fill = "grey92"),
    panel.grid.minor = element_blank()
  )

out <- "fig6_citations_per_author.pdf"
ggsave(out, p, width = 16, height = 7)
message("Saved: ", out)
