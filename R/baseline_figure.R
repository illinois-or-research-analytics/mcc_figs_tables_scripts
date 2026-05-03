# Mar 6 and May 3 GC edited Claude Code with hjust and label changes
# Updated to read directly from cluster nodelist paths (no compressed files)

# ============================================================
# SASCA-ResA Baseline Figure
# Two-panel: (i) CCDF of in_degree, log-log
#            (ii) Spearman correlations with in_degree
#
# Requirements: data.table, ggplot2, RColorBrewer, patchwork, scales
# ============================================================

rm(list=ls())
if (!requireNamespace("patchwork", quietly = TRUE)) install.packages("patchwork", repos = "https://cran.r-project.org")
library(data.table)
library(ggplot2)
library(RColorBrewer)
library(patchwork)
library(scales)

# ── Parameters ────────────────────────────────────────────────────────────────
FONT_SIZE    <- 16
THRESHOLD_1  <- 1707  # top 0.1%
THRESHOLD_2  <- 9018  # top 0.01%

COL_LINE <- brewer.pal(9, "Blues")[7]
COL_POS  <- brewer.pal(11, "RdBu")[9]
COL_NEG  <- brewer.pal(11, "RdBu")[3]
COL_REF  <- "grey60"

# ── Baseline file paths ───────────────────────────────────────────────────────
base_dir <- "/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/chacko/claude_baseline_data"
out_dir  <- base_dir

baseline_files <- file.path(base_dir, paste0("bsl", 1:10), "output", "nodelist.csv")

vars_of_interest <- c("fit_peak_value", "out_degree", "num_authors",
                      "initial_author_reputation", "alpha", "year")

# ── Compute per-replicate percentiles and correlations ────────────────────────
percentile_points <- 1:100
all_pcts   <- matrix(NA_real_, nrow = length(baseline_files), ncol = 100)
corr_list  <- vector("list", length(baseline_files))

for (i in seq_along(baseline_files)) {
  cat("Loading", baseline_files[i], "\n")
  dt     <- fread(baseline_files[i], select = c("type", "in_degree", vars_of_interest))
  agents <- dt[type == "agent"]

  all_pcts[i, ] <- quantile(agents$in_degree, probs = percentile_points / 100)

  corr_list[[i]] <- sapply(vars_of_interest, function(v) {
    cor(agents[[v]], agents$in_degree, method = "spearman", use = "complete.obs")
  })
}

# ── Average across replicates ─────────────────────────────────────────────────
mean_pcts  <- colMeans(all_pcts)
corr_mat   <- do.call(rbind, corr_list)
mean_corr  <- colMeans(corr_mat)
sd_corr    <- apply(corr_mat, 2, sd)

# ── Panel (i): CCDF ───────────────────────────────────────────────────────────
# Exclude zero in_degree (log undefined) and p100 where ccdf = 0 (log undefined)
# Note: ~7.03% of agents have zero citations and are excluded from this plot
ccdf_dt <- data.table(
  in_degree = mean_pcts,
  ccdf      = 1 - percentile_points / 100
)[in_degree > 0 & ccdf > 0]

p1 <- ggplot(ccdf_dt, aes(x = in_degree, y = ccdf)) +
  geom_line(colour = COL_LINE, linewidth = 1) +
  geom_vline(xintercept = THRESHOLD_1, linetype = "dashed", colour = COL_REF, linewidth = 0.7) +
  geom_vline(xintercept = THRESHOLD_2, linetype = "dashed", colour = COL_REF, linewidth = 0.7) +
  annotate("text", x = THRESHOLD_1 * 1.4, y = 0.3, label = "Top 0.1%",
           colour = COL_REF, angle = 90, size = (FONT_SIZE - 4) / .pt) +
  annotate("text", x = THRESHOLD_2 * 1.4, y = 0.3, label = "Top 0.01%",
           colour = COL_REF, angle = 90, size = (FONT_SIZE - 4) / .pt) +
  scale_x_log10(labels = label_comma()) +
  scale_y_log10() +
  labs(x = "In-degree (citations)",
       y = "Proportion of papers with in-degree > x",
       tag = "(i)") +
  theme_bw(base_size = FONT_SIZE) +
  theme(
    plot.tag          = element_text(face = "bold", size = FONT_SIZE),
    plot.tag.position = c(0.04, 0.05),
    panel.grid.minor  = element_blank(),
    axis.title        = element_text(size = FONT_SIZE),
    axis.text         = element_text(size = FONT_SIZE - 2)
  )

# ── Panel (ii): Spearman bar chart ────────────────────────────────────────────
var_labels <- c(
  fit_peak_value             = "Fitness",
  out_degree                 = "Out-degree",
  num_authors                = "Number of authors",
  initial_author_reputation  = "Initial author reputation",
  alpha                      = "Alpha",
  year                       = "Publication year"
)

corr_dt <- data.table(
  variable = names(mean_corr),
  r        = as.numeric(mean_corr),
  sd       = as.numeric(sd_corr)
)[, `:=`(
  label   = var_labels[variable],
  bar_col = fifelse(r >= 0, "positive", "negative")
)][, label := factor(label, levels = label[order(abs(r))])]

p2 <- ggplot(corr_dt, aes(x = r, y = label, fill = bar_col)) +
  geom_col(width = 0.6, colour = "white") +
  geom_errorbar(aes(xmin = r - sd, xmax = r + sd),
                width = 0.25, colour = "grey50", linewidth = 0.8, orientation = "y") +
  geom_vline(xintercept = 0, colour = "black", linewidth = 0.6) +
  geom_text(aes(
    label = fifelse(r >= 0, sprintf("  %.3f", r), sprintf("%.3f  ", r)),
    hjust = fifelse(r >= 0, -0.2, 1.2)
  ), size = (FONT_SIZE - 5) / .pt, colour = "black") +
  scale_fill_manual(values = c(positive = COL_POS, negative = COL_NEG), guide = "none") +
  scale_x_continuous(limits = c(-1, 1)) +
  labs(x = "Spearman's r  with in-degree", y = NULL, tag = "(ii)") +
  theme_bw(base_size = FONT_SIZE) +
  theme(
    plot.tag             = element_text(face = "bold", size = FONT_SIZE),
    plot.tag.position    = c(0.04, 0.05),
    panel.grid.minor     = element_blank(),
    panel.grid.major.y   = element_blank(),
    axis.title           = element_text(size = FONT_SIZE),
    axis.text            = element_text(size = FONT_SIZE - 2)
  )

# ── Combine and save ──────────────────────────────────────────────────────────
combined <- p1 + p2 + plot_layout(widths = c(1, 1))

ggsave(file.path(out_dir, "baseline_figure.pdf"), plot = combined, width = 12, height = 5.5, units = "in", dpi = 300)
ggsave(file.path(out_dir, "baseline_figure.png"), plot = combined, width = 12, height = 5.5, units = "in", dpi = 300)
ggsave(file.path(out_dir, "baseline_figure.eps"), plot = combined, width = 12, height = 5.5, units = "in", dpi = 300, device = "eps")

cat("Saved baseline_figure.pdf/.png/.eps to", out_dir, "\n")
