# ============================================================
# SASCA-ReS-A: Generator Neighborhood Richness (R / data.table)
#
# For each planted node, extracts its generator node ID,
# finds all 1-hop neighbors of that generator in the SEED
# network (year 0 state), and computes the sum of in-degrees
# of those neighbors as a proxy for "congenital advantage."
#
# Outputs a small summary CSV suitable for passing back for
# further analysis.
#
# Inputs:
#   - Seed nodelist (sj_nodelist.csv — the original input network)
#   - Seed edgelist (sj_edgelist.csv — the original input network)
#   - One nodelist per experimental run (to extract planted nodes
#     and their generator_node_string)
#
# ============================================================
# Dependencies: data.table
# install.packages("data.table")
# ============================================================

library(data.table)

# ============================================================
# CONFIGURATION — edit paths before running
# ============================================================

# Original seed network files (used to compute year-0 neighborhood richness)
SEED_NODELIST <- "/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v5/abm_inputs/sj_nodelist.csv"
SEED_EDGELIST <- "/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v5/abm_inputs/sj_edgelist.csv"

# Experimental run nodelists — add ps5 and more reps as available
run_nodelists <- list(
  p5_1  = "p5_1.csv",
  p5_2  = "p5_2.csv",
  p5_3  = "p5_3.csv",
  ps5_1 = "ps5_1.csv",
  ps5_2 = "ps5_2.csv",
  ps5_3 = "ps5_3.csv"
)

# Output CSV path
OUTPUT_CSV <- "planted_node_generator_richness.csv"

# Nodelist columns needed from run files
RUN_NODE_COLS <- c(
  "node_id", "type", "year", "in_degree",
  "planted_nodes_line_number", "generator_node_string",
  "sampled_neighborhood_size"
)

# ============================================================
# SECTION 1: LOAD SEED NETWORK
# ============================================================

message("Loading seed nodelist...")
# Assume seed nodelist has at minimum: node_id, in_degree
# Adjust select= if column names differ
seed_nodes <- fread(SEED_NODELIST)
message(sprintf("  Seed nodes: %d", nrow(seed_nodes)))

# Standardize: we need node_id and in_degree from seed
# Check what columns are present and map accordingly
message("Seed nodelist columns: ", paste(names(seed_nodes), collapse = ", "))

# If the seed nodelist doesn't have in_degree pre-computed,
# we'll derive it from the seed edgelist below.
# Set this to TRUE if in_degree is already in the seed nodelist.
SEED_HAS_INDEGREE <- "in_degree" %in% names(seed_nodes)

message("Loading seed edgelist...")
seed_edges <- fread(SEED_EDGELIST)
message(sprintf("  Seed edges: %d", nrow(seed_edges)))
message("Seed edgelist columns: ", paste(names(seed_edges), collapse = ", "))

# ============================================================
# SECTION 2: DERIVE SEED IN-DEGREES IF NEEDED
# ============================================================

# Identify the source and target columns in the seed edgelist.
# Common conventions — script will auto-detect or fall back to positional.
edge_cols <- names(seed_edges)

# Try to identify source (citing) and target (cited) columns
if (all(c("source_node_id", "target_node_id") %in% edge_cols)) {
  src_col <- "source_node_id"
  tgt_col <- "target_node_id"
} else if (all(c("source", "target") %in% edge_cols)) {
  src_col <- "source"
  tgt_col <- "target"
} else {
  # Fall back to first two columns
  src_col <- edge_cols[1]
  tgt_col <- edge_cols[2]
  message(sprintf("  Using positional columns: source=%s, target=%s",
                  src_col, tgt_col))
}

setnames(seed_edges, c(src_col, tgt_col), c("source_id", "target_id"))

if (!SEED_HAS_INDEGREE) {
  message("Computing in-degrees from seed edgelist...")
  seed_indegree <- seed_edges[, .(seed_in_degree = .N), by = .(node_id = target_id)]
} else {
  seed_indegree <- seed_nodes[, .(node_id, seed_in_degree = in_degree)]
}

# Ensure all seed nodes present (some may have 0 in-degree)
all_seed_ids <- unique(c(seed_edges$source_id, seed_edges$target_id,
                          seed_nodes$node_id))
seed_indegree <- merge(
  data.table(node_id = all_seed_ids),
  seed_indegree,
  by = "node_id", all.x = TRUE
)
seed_indegree[is.na(seed_in_degree), seed_in_degree := 0]

message(sprintf("  Seed in-degree computed for %d nodes", nrow(seed_indegree)))
message(sprintf("  Seed in-degree: median=%.0f, max=%.0f",
                median(seed_indegree$seed_in_degree),
                max(seed_indegree$seed_in_degree)))

# ============================================================
# SECTION 3: BUILD 1-HOP NEIGHBOR LOOKUP FOR SEED NETWORK
# ============================================================

message("Building 1-hop neighbor index from seed edgelist...")

# For a generator node G, its 1-hop neighborhood (distance-1) consists of:
# all nodes that G cites (out-edges) and all nodes that cite G (in-edges).
# The agent inherits this as its initial candidate citation pool.
# We want the sum of seed_in_degree over all 1-hop neighbors of G.

# Out-neighbors of G (nodes G cites)
out_neighbors <- seed_edges[, .(generator_id = source_id, neighbor_id = target_id)]

# In-neighbors of G (nodes that cite G)
in_neighbors  <- seed_edges[, .(generator_id = target_id, neighbor_id = source_id)]

# Combined 1-hop neighborhood (undirected)
hop1 <- unique(rbindlist(list(out_neighbors, in_neighbors)))

# Join neighbor in-degrees
hop1 <- merge(hop1, seed_indegree, by.x = "neighbor_id", by.y = "node_id",
              all.x = TRUE)
hop1[is.na(seed_in_degree), seed_in_degree := 0]

# Compute per-generator: count of 1-hop neighbors and sum of their in-degrees
generator_richness <- hop1[, .(
  n_hop1_neighbors     = .N,
  sum_indegree_hop1    = sum(seed_in_degree),
  mean_indegree_hop1   = mean(seed_in_degree),
  median_indegree_hop1 = median(seed_in_degree),
  max_indegree_hop1    = max(seed_in_degree)
), by = generator_id]

message(sprintf("  Generator richness computed for %d seed nodes", nrow(generator_richness)))

# ============================================================
# SECTION 4: EXTRACT PLANTED NODES AND THEIR GENERATORS
# ============================================================

message("\nExtracting planted nodes from experimental runs...")

extract_planted <- function(path, run_label) {
  dt <- fread(path, select = RUN_NODE_COLS)
  dt <- dt[type == "agent"]
  dt[, rank := rank(-in_degree, ties.method = "min")]
  planted <- dt[planted_nodes_line_number != -1]
  planted[, run := run_label]
  planted
}

planted_all <- rbindlist(mapply(
  extract_planted,
  path      = run_nodelists,
  run_label = names(run_nodelists),
  SIMPLIFY  = FALSE
))

message(sprintf("  Total planted nodes extracted: %d", nrow(planted_all)))
print(planted_all[, .(run, node_id, year, rank, in_degree,
                       planted_nodes_line_number, generator_node_string)])

# ============================================================
# SECTION 5: PARSE GENERATOR NODE ID
# ============================================================

# generator_node_string format needs inspection — print examples
message("\nSample generator_node_string values:")
print(planted_all[, .(generator_node_string)] |> head(10))

# Common formats observed in ABM outputs:
#   "12345678"          -> plain node ID
#   "node_12345678"     -> prefixed
#   "12345678,..."      -> node ID is first token
# Script attempts to parse as integer after stripping non-numeric prefix.
# Adjust parse_generator() if your format differs.

parse_generator <- function(x) {
  # Extract first contiguous run of digits
  m <- regmatches(x, regexpr("[0-9]+", x))
  as.integer(ifelse(length(m) == 0, NA, m))
}

planted_all[, generator_id := parse_generator(generator_node_string),
             by = seq_len(nrow(planted_all))]

message("\nParsed generator IDs:")
print(planted_all[, .(run, node_id, generator_node_string, generator_id)])

# Check how many matched to seed network
n_matched <- sum(planted_all$generator_id %in% generator_richness$generator_id,
                 na.rm = TRUE)
message(sprintf("  %d / %d planted nodes matched to seed generator richness",
                n_matched, nrow(planted_all)))

# ============================================================
# SECTION 6: JOIN GENERATOR RICHNESS TO PLANTED NODES
# ============================================================

result <- merge(
  planted_all[, .(run, node_id, year, rank, in_degree,
                   sampled_neighborhood_size, generator_id,
                   generator_node_string, planted_nodes_line_number)],
  generator_richness,
  by = "generator_id",
  all.x = TRUE
)

# Also join generator's own seed in-degree
result <- merge(
  result,
  seed_indegree[, .(generator_id = node_id, generator_seed_indegree = seed_in_degree)],
  by = "generator_id",
  all.x = TRUE
)

# Derive condition label from run name
result[, condition := fifelse(grepl("^p5", run), "p5", "ps5")]

# Sort for readability
setorder(result, condition, run, rank)

message("\n=== Generator richness vs planted node rank ===")
print(result[, .(condition, run, year, rank, in_degree,
                  sampled_neighborhood_size,
                  generator_seed_indegree,
                  n_hop1_neighbors,
                  sum_indegree_hop1,
                  mean_indegree_hop1)])

# ============================================================
# SECTION 7: SPEARMAN CORRELATIONS
# ============================================================

message("\n=== Spearman correlations with planted node rank ===")

metrics <- c("sampled_neighborhood_size", "generator_seed_indegree",
             "n_hop1_neighbors", "sum_indegree_hop1", "mean_indegree_hop1")

cor_results <- rbindlist(lapply(metrics, function(m) {
  sub <- result[!is.na(get(m))]
  if (nrow(sub) < 3) return(NULL)
  r <- cor(sub[[m]], sub$rank, method = "spearman")
  data.table(metric = m, spearman_r = round(r, 4), n = nrow(sub))
}))
print(cor_results)

# By condition separately
for (cond in c("p5", "ps5")) {
  message(sprintf("\n  Condition: %s", cond))
  sub_cond <- result[condition == cond]
  cor_cond <- rbindlist(lapply(metrics, function(m) {
    sub <- sub_cond[!is.na(get(m))]
    if (nrow(sub) < 3) return(NULL)
    r <- cor(sub[[m]], sub$rank, method = "spearman")
    data.table(metric = m, spearman_r = round(r, 4), n = nrow(sub))
  }))
  print(cor_cond)
}

# ============================================================
# SECTION 8: WRITE OUTPUT CSV
# ============================================================

fwrite(result, OUTPUT_CSV)
message(sprintf("\nOutput written to: %s", OUTPUT_CSV))
message("Columns: ", paste(names(result), collapse = ", "))
message("\n=== Done ===")
