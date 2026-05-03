# ============================================================
# SASCA-ReS-A Validation Script (R / data.table)
# Baseline characterization + planted node analysis
# Agents only — seed nodes excluded from all analyses
# ============================================================
# Dependencies: data.table, ggplot2, scales
# Install if needed:
#   install.packages(c("data.table", "ggplot2", "scales"))
# ============================================================

library(data.table)
library(ggplot2)
library(scales)

# ============================================================
# CONFIGURATION — edit paths before running
# ============================================================

baseline_files <- list(
  bsl1 = "bsl1_mar12026.csv",
  bsl2 = "bsl2_mar12026.csv",
  bsl3 = "bsl3_mar12026.csv"
)

# Planted experiment files — fill in when available
# p5_files  <- list(p5_r1  = "p5_r1_nodelist.csv", ...)
# ps5_files <- list(ps5_r1 = "ps5_r1_nodelist.csv", ...)

# Planted node IDs — fill in from your config
# p5_planted_ids  <- c(id1, id2, id3, id4, id5)
# ps5_planted_ids <- c(id1, id2, id3, id4, id5)

COLS_KEEP <- c(
  "node_id", "type", "year", "in_degree",
  "fit_peak_value", "num_authors", "out_degree", "alpha",
  "pa_weight", "fit_weight", "num_authors_weight",
  "author_reputation_weight", "sampled_neighborhood_size",
  "planted_nodes_line_number"
)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

#' Read nodelist CSV, filter to agents only, return key columns
read_nodelist <- function(path, run_label = NULL) {
  message("Reading: ", path)
  dt <- fread(path, select = COLS_KEEP)
  dt <- dt[type == "agent"]
  if (!is.null(run_label)) dt[, run := run_label]
  dt
}

#' Add rank column: rank 1 = highest in_degree, ties by min method
add_ranks <- function(dt) {
  dt[, rank := rank(-in_degree, ties.method = "min")]
  dt
}

#' Summary metrics matching the ablation table format
rank_summary <- function(ranks, label = "") {
  data.table(
    condition   = label,
    n           = length(ranks),
    median_rank = median(ranks),
    mean_rank   = mean(ranks),
    sd_rank     = sd(ranks),
    top10       = sum(ranks <= 10),
    top100      = sum(ranks <= 100),
    worst_rank  = max(ranks)
  )
}

#' Extract planted node rows with their ranks given node IDs
rank_planted_nodes <- function(dt_ranked, planted_ids) {
  dt_ranked[node_id %in% planted_ids, .(node_id, in_degree, rank, run)]
}

# ============================================================
# SECTION 1: LOAD BASELINE DATA (agents only)
# ============================================================

message("\n=== Loading baseline runs (agents only) ===")

baseline_list <- lapply(names(baseline_files), function(nm) {
  dt <- read_nodelist(baseline_files[[nm]], run_label = nm)
  add_ranks(dt)
})
names(baseline_list) <- names(baseline_files)

baseline_all <- rbindlist(baseline_list)

message("Agent counts per run:")
print(baseline_all[, .N, by = run])

# ============================================================
# SECTION 2: BASELINE IN-DEGREE DISTRIBUTION
# ============================================================

message("\n=== In-degree distribution (agents only) ===")

baseline_stats <- baseline_all[, .(
  n_agents     = .N,
  mean_indeg   = mean(in_degree),
  median_indeg = median(in_degree),
  sd_indeg     = sd(in_degree),
  p90          = quantile(in_degree, 0.90),
  p99          = quantile(in_degree, 0.99),
  max_indeg    = max(in_degree),
  n_zero       = sum(in_degree == 0)
), by = run]
print(baseline_stats)

# Log-log in-degree distribution plot
indeg_counts <- baseline_all[in_degree > 0, .N, by = .(run, in_degree)]

p_indeg <- ggplot(indeg_counts, aes(x = in_degree, y = N, color = run)) +
  geom_point(alpha = 0.4, size = 0.8) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Baseline In-degree Distribution — agents only (3 runs)",
    x     = "In-degree (log scale)",
    y     = "Count (log scale)",
    color = "Run"
  ) +
  theme_minimal(base_size = 13)

ggsave("baseline_indegree_loglog.pdf", p_indeg, width = 8, height = 5)
message("Saved: baseline_indegree_loglog.pdf")

# ============================================================
# SECTION 3: RANK DISTRIBUTION SUMMARY
# ============================================================

message("\n=== Rank distribution summary (agents only) ===")

rank_summaries <- rbindlist(lapply(names(baseline_list), function(nm) {
  rank_summary(baseline_list[[nm]]$rank, label = nm)
}))
print(rank_summaries)

# ============================================================
# SECTION 4: CROSS-RUN CONSISTENCY (SPEARMAN ON QUANTILES)
# ============================================================

message("\n=== Cross-run Spearman correlations (in-degree quantiles) ===")

q_seq <- seq(0.01, 0.99, by = 0.01)

quantile_dt <- data.table(
  quantile = q_seq,
  bsl1 = quantile(baseline_list$bsl1$in_degree, q_seq),
  bsl2 = quantile(baseline_list$bsl2$in_degree, q_seq),
  bsl3 = quantile(baseline_list$bsl3$in_degree, q_seq)
)

cor_mat <- cor(quantile_dt[, .(bsl1, bsl2, bsl3)], method = "spearman")
message("Spearman correlation matrix of in-degree quantiles across runs:")
print(round(cor_mat, 4))

# ============================================================
# SECTION 5: TOP-RANKED AGENT CHARACTERIZATION
# ============================================================

message("\n=== Top 100 agents: parameter profiles ===")

param_cols <- c(
  "fit_peak_value", "num_authors", "out_degree", "alpha",
  "pa_weight", "fit_weight", "num_authors_weight",
  "author_reputation_weight", "sampled_neighborhood_size"
)

top100 <- baseline_all[rank <= 100]
top100_summary <- top100[, lapply(.SD, mean, na.rm = TRUE),
                          .SDcols = param_cols, by = run]
message("Mean parameters of top-100 agents per run:")
print(top100_summary)

# Top-100 vs all agents pooled
compare_dt <- rbindlist(list(
  baseline_all[, c(.SD, .(group = "all_agents")), .SDcols = param_cols],
  top100[,       c(.SD, .(group = "top100")),      .SDcols = param_cols]
))

compare_summary <- compare_dt[, .(
  fit_peak_value_mean   = mean(fit_peak_value,   na.rm = TRUE),
  fit_peak_value_median = median(fit_peak_value, na.rm = TRUE),
  num_authors_mean      = mean(num_authors,       na.rm = TRUE),
  num_authors_median    = median(num_authors,     na.rm = TRUE),
  out_degree_mean       = mean(out_degree,        na.rm = TRUE),
  out_degree_median     = median(out_degree,      na.rm = TRUE),
  nbhd_mean             = mean(sampled_neighborhood_size,   na.rm = TRUE),
  nbhd_median           = median(sampled_neighborhood_size, na.rm = TRUE)
), by = group]
message("\nTop-100 vs all agents (pooled):")
print(compare_summary)

# ============================================================
# SECTION 6: NEIGHBORHOOD SIZE vs RANK (SPEARMAN)
# ============================================================

message("\n=== Neighborhood size vs rank: Spearman r ===")

nbhd_cor <- baseline_all[
  !is.na(sampled_neighborhood_size) & sampled_neighborhood_size > 0,
  .(spearman_r = cor(sampled_neighborhood_size, rank, method = "spearman"),
    n = .N),
  by = run
]
print(nbhd_cor)

# Log-log scatter: neighborhood size vs in_degree (10k sample per run)
set.seed(42)
nbhd_sample <- baseline_all[
  sampled_neighborhood_size > 0 & in_degree > 0
][, .SD[sample(.N, min(.N, 10000))], by = run]

p_nbhd <- ggplot(nbhd_sample,
                 aes(x = sampled_neighborhood_size, y = in_degree, color = run)) +
  geom_point(alpha = 0.2, size = 0.6) +
  scale_x_log10(labels = comma) +
  scale_y_log10(labels = comma) +
  labs(
    title = "Neighborhood Size vs In-degree — agents only (baseline, sampled)",
    x     = "Sampled neighborhood size (log)",
    y     = "In-degree (log)",
    color = "Run"
  ) +
  theme_minimal(base_size = 13)

ggsave("baseline_nbhd_vs_indegree.pdf", p_nbhd, width = 8, height = 5)
message("Saved: baseline_nbhd_vs_indegree.pdf")

# ============================================================
# SECTION 7: PLANTED NODE ANALYSIS (uncomment when files ready)
# ============================================================

# --- p5 ---
# p5_list <- lapply(names(p5_files), function(nm) {
#   dt <- read_nodelist(p5_files[[nm]], run_label = nm)
#   add_ranks(dt)
# })
# names(p5_list) <- names(p5_files)
#
# p5_planted <- rbindlist(lapply(p5_list, rank_planted_nodes,
#                                planted_ids = p5_planted_ids))
# p5_summary <- rank_summary(p5_planted$rank, label = "p5")
# print(p5_summary)

# --- ps5 ---
# ps5_list <- lapply(names(ps5_files), function(nm) {
#   dt <- read_nodelist(ps5_files[[nm]], run_label = nm)
#   add_ranks(dt)
# })
# names(ps5_list) <- names(ps5_files)
#
# ps5_planted <- rbindlist(lapply(ps5_list, rank_planted_nodes,
#                                 planted_ids = ps5_planted_ids))
# ps5_summary <- rank_summary(ps5_planted$rank, label = "ps5")
# print(ps5_summary)

# --- Comparison table ---
# comparison <- rbindlist(list(p5_summary, ps5_summary))
# print(comparison)

# --- Plot: planted ranks vs baseline ---
# planted_combined <- rbindlist(list(
#   p5_planted[,  condition := "p5"],
#   ps5_planted[, condition := "ps5"]
# ))
#
# p_planted <- ggplot(planted_combined,
#                     aes(x = condition, y = rank, color = condition)) +
#   geom_jitter(width = 0.15, size = 3) +
#   geom_boxplot(alpha = 0.2, outlier.shape = NA) +
#   scale_y_log10(labels = comma) +
#   labs(title = "Planted Node Ranks: p5 vs ps5",
#        x = "Condition", y = "Rank (log scale)") +
#   theme_minimal(base_size = 13)
# ggsave("planted_ranks_p5_vs_ps5.pdf", p_planted, width = 6, height = 5)

message("\n=== Done ===")
