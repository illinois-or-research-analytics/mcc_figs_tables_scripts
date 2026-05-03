library(data.table)
library(ggplot2)

BASE_DIR <- "/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/3_18_min_cartel_experiments"

MODELS <- c("control_model", "null_model", "phenotype_model")
SIZES  <- c("size_5", "size_25", "size_125", "size_250", "size_500", "size_1000")
PROP   <- "proportion_0.50"
REPS   <- c("r0", "r1", "r2")

# --- Read all author_nodelists ---
runs <- expand.grid(model = MODELS, size = SIZES, rep = REPS, stringsAsFactors = FALSE)
runs$path <- file.path(BASE_DIR, runs$model, runs$size, PROP, runs$rep, "output", "author_nodelist.csv")

dt_list <- vector("list", nrow(runs))

for (i in seq_len(nrow(runs))) {
  p <- runs$path[i]
  if (!file.exists(p)) {
    cat("Missing:", p, "\n")
    next
  }
  tmp <- fread(p)
  tmp[, `:=`(model = runs$model[i], size = runs$size[i], rep = runs$rep[i])]
  dt_list[[i]] <- tmp
  rm(tmp); gc()
}

dt <- rbindlist(dt_list, fill = TRUE)
rm(dt_list); gc()

# --- Cartel authors only ---
dt <- dt[is_cartel == TRUE]

# --- Extract numeric size ---
dt[, size_n := as.integer(sub("size_", "", size))]

# --- Per-run mean citations per cartel author ---
per_run <- dt[, .(
  self    = mean(citations_from_self,       na.rm = TRUE),
  cartel  = mean(citations_from_cartel,     na.rm = TRUE),
  background = mean(citations_from_background, na.rm = TRUE)
), by = .(model, size_n, rep)]

# --- Mean and SD across reps ---
summary_dt <- per_run[, .(
  self_mean    = mean(self),
  self_sd      = sd(self),
  cartel_mean  = mean(cartel),
  cartel_sd    = sd(cartel),
  bg_mean      = mean(background),
  bg_sd        = sd(background)
), by = .(model, size_n)]

# --- Melt to long for plotting ---
mean_long <- melt(summary_dt,
  id.vars = c("model", "size_n"),
  measure.vars = c("self_mean", "cartel_mean", "bg_mean"),
  variable.name = "source", value.name = "mean_citations"
)
sd_long <- melt(summary_dt,
  id.vars = c("model", "size_n"),
  measure.vars = c("self_sd", "cartel_sd", "bg_sd"),
  variable.name = "source_sd", value.name = "sd_citations"
)

# Align source labels
mean_long[, source := fcase(
  source == "self_mean",   "Self",
  source == "cartel_mean", "Other cartel",
  source == "bg_mean",     "Background"
)]
sd_long[, source := fcase(
  source_sd == "self_sd",   "Self",
  source_sd == "cartel_sd", "Other cartel",
  source_sd == "bg_sd",     "Background"
)]

plot_dt <- merge(mean_long, sd_long[, .(model, size_n, source, sd_citations)],
                 by = c("model", "size_n", "source"))

# --- Clean model labels ---
plot_dt[, model_label := fcase(
  model == "control_model",   "Control",
  model == "null_model",      "Cartel-R",
  model == "phenotype_model", "Cartel-P"
)]
plot_dt[, model_label := factor(model_label, levels = c("Control", "Cartel-R", "Cartel-P"))]
plot_dt[, source := factor(source, levels = c("Self", "Other cartel", "Background"))]

# --- Plot: absolute counts ---
p_abs <- ggplot(plot_dt, aes(x = size_n, y = mean_citations, color = source, group = source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = mean_citations - sd_citations,
                  ymax = mean_citations + sd_citations,
                  fill = source), alpha = 0.15, color = NA) +
  facet_wrap(~ model_label, nrow = 1) +
  scale_x_log10(breaks = c(5, 25, 125, 250, 500, 1000)) +
  scale_color_manual(values = c("Self" = "#E69F00", "Other cartel" = "#0072B2", "Background" = "#999999")) +
  scale_fill_manual(values  = c("Self" = "#E69F00", "Other cartel" = "#0072B2", "Background" = "#999999")) +
  labs(
    title = "Mean citations per cartel author by source",
    x = "Cartel size (log scale)",
    y = "Mean citations received",
    color = "Citation source",
    fill  = "Citation source"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# --- Plot: proportions ---
plot_dt[, total_mean := sum(mean_citations), by = .(model, size_n)]
plot_dt[, prop := mean_citations / total_mean]

p_prop <- ggplot(plot_dt, aes(x = size_n, y = prop, color = source, group = source)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  facet_wrap(~ model_label, nrow = 1) +
  scale_x_log10(breaks = c(5, 25, 125, 250, 500, 1000)) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_color_manual(values = c("Self" = "#E69F00", "Other cartel" = "#0072B2", "Background" = "#999999")) +
  labs(
    title = "Proportion of citations per cartel author by source",
    x = "Cartel size (log scale)",
    y = "Proportion of citations",
    color = "Citation source"
  ) +
  theme_bw(base_size = 12) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

# --- Save ---
ggsave("cartel_citations_absolute.pdf", p_abs, width = 10, height = 4, device = "pdf")
ggsave("cartel_citations_proportion.pdf", p_prop, width = 10, height = 4, device = "pdf")

cat("Plots saved.\n")
