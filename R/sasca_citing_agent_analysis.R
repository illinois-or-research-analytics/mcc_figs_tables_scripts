# ============================================================
# SASCA-ReS-A: Citing Agent Composition Analysis (R / data.table)
#
# Hypothesis: planted nodes are cited early by high fit_weight
# agents, with pa_weight agents predominating later as
# preferential attachment feedback kicks in.
# Comparison: same measurement on high-ranked baseline agents.
#
# Inputs required (same directory or edit paths below):
#   - nodelist CSV (one per run: p5, ps5, baseline)
#   - edgelist CSV (one per run, matching nodelists)
#
# Edgelist format assumed: source_node_id, target_node_id
#   (i.e. source CITES target — confirm and adjust TARGET_COL
#    and SOURCE_COL below if your convention differs)
# ============================================================
# Dependencies: data.table, ggplot2, scales
# install.packages(c("data.table", "ggplot2", "scales"))
# ============================================================

library(data.table)
library(ggplot2)
library(scales)

# ============================================================
# CONFIGURATION — edit before running
# ============================================================

# --- File paths ---
# One nodelist + edgelist pair per run; extend lists for more reps

# p5 runs
p5_nodelists <- list(
  p5_1 = "p5_1_nodelist.csv",
  p5_2 = "p5_2_nodelist.csv",
  p5_3 = "p5_3_nodelist.csv"
)
p5_edgelists <- list(
  p5_1 = "p5_1_edgelist.csv",
  p5_2 = "p5_2_edgelist.csv",
  p5_3 = "p5_3_edgelist.csv"
)

# ps5 runs
ps5_nodelists <- list(
  ps5_1 = "ps5_1_nodelist.csv",
  ps5_2 = "ps5_2_nodelist.csv",
  ps5_3 = "ps5_3_nodelist.csv"
)
ps5_edgelists <- list(
  ps5_1 = "ps5_1_edgelist.csv",
  ps5_2 = "ps5_2_edgelist.csv",
  ps5_3 = "ps5_3_edgelist.csv"
)

# baseline runs (for comparison)
bsl_nodelists <- list(
  bsl1 = "bsl1_mar12026.csv",
  bsl2 = "bsl2_mar12026.csv",
  bsl3 = "bsl3_mar12026.csv"
)
bsl_edgelists <- list(
  bsl1 = "bsl1_edgelist.csv",
  bsl2 = "bsl2_edgelist.csv",
  bsl3 = "bsl3_edgelist.csv"
)

# --- Edgelist column names ---
# Adjust if your edgelist uses different column names.
# Convention: SOURCE_COL agent CITES TARGET_COL agent.
SOURCE_COL <- "source_node_id"   # the citing agent
TARGET_COL <- "target_node_id"   # the node being cited

# --- Year binning ---
# Simulation runs 30 years; bin citing agents' birth years
# into periods to track composition trends.
# Adjust SIM_START to match your simulation start year.
SIM_START  <- 1983
BIN_WIDTH  <- 5    # years per bin; adjust as needed

# --- Baseline comparison: how many top-ranked baseline agents to sample ---
N_BASELINE_SAMPLE <- 20   # per run

# --- Nodelist columns to keep ---
NODE_COLS <- c(
  "node_id", "type", "year", "in_degree",
  "pa_weight", "fit_weight", "num_authors_weight",
  "author_reputation_weight", "fit_peak_value",
  "num_authors", "out_degree", "sampled_neighborhood_size",
  "planted_nodes_line_number"
)

# ============================================================
# HELPER FUNCTIONS
# ============================================================

#' Read nodelist, filter to agents, add ranks
read_nodelist <- function(path, run_label = NULL) {
  message("  Reading nodelist: ", path)
  dt <- fread(path, select = NODE_COLS)
  dt <- dt[type == "agent"]
  dt[, rank := rank(-in_degree, ties.method = "min")]
  if (!is.null(run_label)) dt[, run := run_label]
  dt
}

#' Read edgelist (source cites target)
read_edgelist <- function(path) {
  message("  Reading edgelist: ", path)
  fread(path, select = c(SOURCE_COL, TARGET_COL),
        col.names = c("source_id", "target_id"))
}

#' Assign year bins to a vector of years
year_bin <- function(years, start = SIM_START, width = BIN_WIDTH) {
  bin_start <- start + floor((years - start) / width) * width
  bin_end   <- bin_start + width - 1
  paste0(bin_start, "-", bin_end)
}

#' For a set of target node IDs, find all citing agents,
#' join their parameters from nodelist, bin by birth year.
#' Returns a data.table of citing agent parameters by year bin.
citing_agent_profiles <- function(target_ids, edgelist, nodelist) {
  # citations received by our targets
  citations <- edgelist[target_id %in% target_ids]
  # join citing agent parameters
  citers <- nodelist[citations, on = .(node_id = source_id), nomatch = 0]
  citers[, year_bin := year_bin(year)]
  citers
}

#' Summarize weight trends by year bin
summarize_by_bin <- function(citers, group_label) {
  citers[, .(
    condition       = group_label,
    n_citations     = .N,
    mean_pa_weight  = mean(pa_weight,   na.rm = TRUE),
    mean_fit_weight = mean(fit_weight,  na.rm = TRUE),
    mean_na_weight  = mean(num_authors_weight,       na.rm = TRUE),
    mean_ar_weight  = mean(author_reputation_weight, na.rm = TRUE),
    mean_fit_value  = mean(fit_peak_value, na.rm = TRUE),
    mean_out_degree = mean(out_degree,     na.rm = TRUE)
  ), by = year_bin][order(year_bin)]
}

#' Process one run: returns citing agent summary for planted nodes
#' and for top-N baseline agents from the same run
process_run <- function(run_label, nodelist_path, edgelist_path,
                        is_planted_run = TRUE,
                        n_baseline = N_BASELINE_SAMPLE) {

  message("\nProcessing run: ", run_label)
  nl <- read_nodelist(nodelist_path, run_label)
  el <- read_edgelist(edgelist_path)

  results <- list()

  # --- Planted node analysis ---
  if (is_planted_run) {
    planted_ids <- nl[planted_nodes_line_number != -1, node_id]
    message("  Planted nodes found: ", length(planted_ids))

    if (length(planted_ids) > 0) {
      citers_planted <- citing_agent_profiles(planted_ids, el, nl)
      results$planted <- summarize_by_bin(citers_planted, group_label = "planted")

      # Also per-planted-node breakdown (useful for rank-stratified analysis)
      results$planted_per_node <- citers_planted[, .(
        condition       = paste0("planted_", target_id),
        n_citations     = .N,
        mean_pa_weight  = mean(pa_weight,   na.rm = TRUE),
        mean_fit_weight = mean(fit_weight,  na.rm = TRUE),
        target_rank     = nl[node_id == target_id[1], rank]
      ), by = .(year_bin, target_id)][order(target_id, year_bin)]
    }
  }

  # --- Baseline comparison: top-N agents by in_degree ---
  top_ids <- nl[order(rank)][seq_len(min(n_baseline, .N)), node_id]
  citers_top <- citing_agent_profiles(top_ids, el, nl)
  results$baseline_top <- summarize_by_bin(citers_top,
                                           group_label = "baseline_top")

  # --- Baseline comparison: random sample of agents ---
  set.seed(42)
  random_ids <- nl[sample(.N, min(n_baseline * 10, .N)), node_id]
  citers_random <- citing_agent_profiles(random_ids, el, nl)
  results$baseline_random <- summarize_by_bin(citers_random,
                                              group_label = "baseline_random")

  results$run <- run_label
  results
}

# ============================================================
# SECTION 1: PROCESS ALL RUNS
# ============================================================

message("\n=== Processing p5 runs ===")
p5_results <- mapply(
  process_run,
  run_label      = names(p5_nodelists),
  nodelist_path  = p5_nodelists,
  edgelist_path  = p5_edgelists,
  is_planted_run = TRUE,
  SIMPLIFY       = FALSE
)

message("\n=== Processing ps5 runs ===")
ps5_results <- mapply(
  process_run,
  run_label      = names(ps5_nodelists),
  nodelist_path  = ps5_nodelists,
  edgelist_path  = ps5_edgelists,
  is_planted_run = TRUE,
  SIMPLIFY       = FALSE
)

message("\n=== Processing baseline runs ===")
bsl_results <- mapply(
  process_run,
  run_label      = names(bsl_nodelists),
  nodelist_path  = bsl_nodelists,
  edgelist_path  = bsl_edgelists,
  is_planted_run = FALSE,
  SIMPLIFY       = FALSE
)

# ============================================================
# SECTION 2: POOL RESULTS ACROSS REPS
# ============================================================

pool_component <- function(result_list, component) {
  rbindlist(lapply(result_list, function(r) {
    if (!is.null(r[[component]])) r[[component]] else NULL
  }), fill = TRUE)
}

p5_planted_trend  <- pool_component(p5_results,  "planted")
ps5_planted_trend <- pool_component(ps5_results, "planted")
bsl_top_trend     <- pool_component(c(p5_results, ps5_results, bsl_results),
                                    "baseline_top")
bsl_random_trend  <- pool_component(bsl_results, "baseline_random")

# Average across reps by year bin
avg_trend <- function(dt, group_label) {
  dt[, .(
    condition       = group_label,
    n_citations     = sum(n_citations),
    mean_pa_weight  = mean(mean_pa_weight,  na.rm = TRUE),
    mean_fit_weight = mean(mean_fit_weight, na.rm = TRUE),
    mean_na_weight  = mean(mean_na_weight,  na.rm = TRUE),
    mean_ar_weight  = mean(mean_ar_weight,  na.rm = TRUE)
  ), by = year_bin][order(year_bin)]
}

trends <- rbindlist(list(
  avg_trend(p5_planted_trend,  "p5_planted"),
  avg_trend(ps5_planted_trend, "ps5_planted"),
  avg_trend(bsl_top_trend,     "baseline_top"),
  avg_trend(bsl_random_trend,  "baseline_random")
))

message("\n=== Weight trends by year bin (averaged across reps) ===")
print(trends)

# ============================================================
# SECTION 3: RANK-STRATIFIED ANALYSIS
# ============================================================

# Pool per-node breakdowns to compare high vs low ranked planted nodes
p5_per_node  <- pool_component(p5_results,  "planted_per_node")
ps5_per_node <- pool_component(ps5_results, "planted_per_node")

# We need rank info — join from nodelists
# Load all planted node ranks from previously computed data
# (re-derive here for self-containment)
get_planted_ranks <- function(file_list) {
  rbindlist(lapply(names(file_list), function(nm) {
    dt <- fread(file_list[[nm]], select = NODE_COLS)
    dt <- dt[type == "agent"]
    dt[, rank := rank(-in_degree, ties.method = "min")]
    dt[planted_nodes_line_number != -1,
       .(node_id, rank, in_degree, year, run = nm)]
  }))
}

p5_ranks  <- get_planted_ranks(p5_nodelists)
ps5_ranks <- get_planted_ranks(ps5_nodelists)

# Classify planted nodes as high (rank <= 20) vs low (rank > 20)
# Adjust threshold as appropriate given your rank distributions
RANK_THRESHOLD <- 20

classify_rank <- function(ranks_dt) {
  ranks_dt[, rank_group := fifelse(rank <= RANK_THRESHOLD, "high_rank", "low_rank")]
}
p5_ranks  <- classify_rank(p5_ranks)
ps5_ranks <- classify_rank(ps5_ranks)

message("\n=== p5 planted node rank classification ===")
print(p5_ranks[, .(node_id, rank, rank_group, in_degree)])

message("\n=== ps5 planted node rank classification ===")
print(ps5_ranks[, .(node_id, rank, rank_group, in_degree)])

# ============================================================
# SECTION 4: PLOTS
# ============================================================

# --- Plot 1: pa_weight vs fit_weight over time (planted vs baseline) ---

# Reshape to long for easier plotting
trends_long <- melt(
  trends,
  id.vars       = c("condition", "year_bin"),
  measure.vars  = c("mean_pa_weight", "mean_fit_weight",
                    "mean_na_weight",  "mean_ar_weight"),
  variable.name = "weight_type",
  value.name    = "mean_weight"
)

# Focus on pa and fit weights for the main hypothesis
pa_fit_long <- trends_long[weight_type %in% c("mean_pa_weight", "mean_fit_weight")]
pa_fit_long[, weight_type := fifelse(weight_type == "mean_pa_weight",
                                     "PA weight", "Fitness weight")]

p_weights <- ggplot(pa_fit_long,
                    aes(x = year_bin, y = mean_weight,
                        color = condition, group = condition,
                        linetype = weight_type)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  scale_color_manual(values = c(
    p5_planted    = "#1f77b4",
    ps5_planted   = "#ff7f0e",
    baseline_top  = "#2ca02c",
    baseline_random = "#9467bd"
  )) +
  labs(
    title    = "Citing Agent Weight Composition Over Time",
    subtitle = "Mean pa_weight and fit_weight of agents citing target nodes, by birth year bin",
    x        = "Citing agent birth year bin",
    y        = "Mean weight",
    color    = "Target condition",
    linetype = "Weight type"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("citing_agent_weights_over_time.pdf", p_weights, width = 10, height = 6)
message("Saved: citing_agent_weights_over_time.pdf")

# --- Plot 2: pa_weight / fit_weight RATIO over time ---
# Ratio > 1 means PA-dominant citing; < 1 means fitness-dominant

trends[, pa_fit_ratio := mean_pa_weight / mean_fit_weight]

p_ratio <- ggplot(trends,
                  aes(x = year_bin, y = pa_fit_ratio,
                      color = condition, group = condition)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey50") +
  annotate("text", x = 1, y = 1.02,
           label = "PA = Fitness", hjust = 0, size = 3.5, color = "grey40") +
  scale_color_manual(values = c(
    p5_planted    = "#1f77b4",
    ps5_planted   = "#ff7f0e",
    baseline_top  = "#2ca02c",
    baseline_random = "#9467bd"
  )) +
  labs(
    title    = "PA/Fitness Weight Ratio of Citing Agents Over Time",
    subtitle = "Ratio > 1: PA-dominant citing; < 1: fitness-dominant citing",
    x        = "Citing agent birth year bin",
    y        = "Mean pa_weight / mean fit_weight",
    color    = "Target condition"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("citing_agent_pa_fit_ratio.pdf", p_ratio, width = 10, height = 6)
message("Saved: citing_agent_pa_fit_ratio.pdf")

# --- Plot 3: Citation volume over time (are planted nodes attracting more?) ---

vol_dt <- rbindlist(list(
  p5_planted_trend[,  .(condition = "p5_planted",     year_bin, n_citations)],
  ps5_planted_trend[, .(condition = "ps5_planted",    year_bin, n_citations)],
  bsl_top_trend[,     .(condition = "baseline_top",   year_bin, n_citations)],
  bsl_random_trend[,  .(condition = "baseline_random",year_bin, n_citations)]
))[, .(n_citations = sum(n_citations)), by = .(condition, year_bin)]

p_vol <- ggplot(vol_dt,
                aes(x = year_bin, y = n_citations,
                    fill = condition, group = condition)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c(
    p5_planted    = "#1f77b4",
    ps5_planted   = "#ff7f0e",
    baseline_top  = "#2ca02c",
    baseline_random = "#9467bd"
  )) +
  labs(
    title = "Citation Volume to Target Nodes by Citing Agent Birth Year",
    x     = "Citing agent birth year bin",
    y     = "Total citations received",
    fill  = "Target condition"
  ) +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("citing_agent_volume_over_time.pdf", p_vol, width = 10, height = 6)
message("Saved: citing_agent_volume_over_time.pdf")

# ============================================================
# SECTION 5: NEIGHBORHOOD SIZE vs RANK (PLANTED NODES)
# ============================================================

message("\n=== Neighborhood size vs rank: planted nodes ===")

all_planted_ranks <- rbindlist(list(
  p5_ranks[,  condition := "p5"],
  ps5_ranks[, condition := "ps5"]
))

print(all_planted_ranks[order(condition, rank),
                        .(condition, run, node_id, year, rank,
                          in_degree, rank_group)])

# Spearman r: neighborhood size vs rank (from nodelist)
# (neighborhood size was already extracted above via get_planted_ranks —
#  re-read with sampled_neighborhood_size included)
get_planted_nbhd <- function(file_list) {
  rbindlist(lapply(names(file_list), function(nm) {
    dt <- fread(file_list[[nm]], select = NODE_COLS)
    dt <- dt[type == "agent"]
    dt[, rank := rank(-in_degree, ties.method = "min")]
    dt[planted_nodes_line_number != -1,
       .(node_id, rank, in_degree, year,
         sampled_neighborhood_size, run = nm)]
  }))
}

p5_nbhd  <- get_planted_nbhd(p5_nodelists)
ps5_nbhd <- get_planted_nbhd(ps5_nodelists)

for (label in c("p5", "ps5")) {
  nbhd_dt <- if (label == "p5") p5_nbhd else ps5_nbhd
  r <- cor(nbhd_dt$sampled_neighborhood_size, nbhd_dt$rank,
           method = "spearman")
  message(sprintf("%s: Spearman r (neighborhood_size vs rank) = %.4f  (n=%d)",
                  label, r, nrow(nbhd_dt)))
  print(nbhd_dt[order(rank),
                .(run, year, rank, in_degree, sampled_neighborhood_size)])
  cat("\n")
}

message("\n=== Done ===")
