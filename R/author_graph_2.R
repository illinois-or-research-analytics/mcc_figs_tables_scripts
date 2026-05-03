library(data.table)

BASE_DIR <- "/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/3_18_min_cartel_experiments"

MODELS <- c("control_model", "null_model", "phenotype_model")
SIZES  <- c("size_5", "size_25", "size_125", "size_250", "size_500", "size_1000")
PROP   <- "proportion_0.50"
REPS   <- c("r0", "r1", "r2")

run_dirs <- expand.grid(model = MODELS, size = SIZES, rep = REPS, stringsAsFactors = FALSE)
run_dirs$path <- file.path(BASE_DIR, run_dirs$model, run_dirs$size, PROP, run_dirs$rep, "output")
run_dirs <- run_dirs$path[dir.exists(run_dirs$path)]

cat("Found", length(run_dirs), "runs\n")

for (run_dir in run_dirs) {
  cat("Processing:", run_dir, "\n")

  el_path <- file.path(run_dir, "edgelist.csv")
  nl_path <- file.path(run_dir, "nodelist.csv")

  if (!file.exists(el_path) || !file.exists(nl_path)) {
    cat("  Skipping — missing files\n")
    next
  }

  # --- Load ---
  el <- fread(el_path, select = c("source", "target"))
  nl <- fread(nl_path, select = c("node_id", "author_id", "cartel_id", "year"))

  # --- Agent papers only (exclude seeds) ---
  agent_nodes <- nl[year >= 1983, .(node_id, author_id, cartel_id)]

  # --- Filter edgelist to agent papers on both ends ---
  agent_ids <- agent_nodes$node_id
  el <- el[source %in% agent_ids & target %in% agent_ids]

  # --- Join source author info ---
  src <- agent_nodes[, .(node_id, author_id, cartel_id)]
  setnames(src, c("node_id", "author_id", "cartel_id"), c("source", "src_author", "src_cartel"))
  tgt <- agent_nodes[, .(node_id, author_id, cartel_id)]
  setnames(tgt, c("node_id", "author_id", "cartel_id"), c("target", "tgt_author", "tgt_cartel"))

  el <- src[el, on = "source", nomatch = 0]
  el <- tgt[el, on = "target", nomatch = 0]

  # --- Author node list: all unique authors ---
  author_nl <- unique(agent_nodes[, .(author_id, cartel_id)], by = "author_id")
  author_nl[, is_cartel := cartel_id != -1]

  # --- Citation breakdowns for cartel authors only ---
  cartel_authors <- author_nl[is_cartel == TRUE, author_id]

  # Subset directed el where target is a cartel author
  el_cartel_tgt <- el[tgt_author %in% cartel_authors]

  # Self-citations: same author on both ends
  self_cites <- el_cartel_tgt[src_author == tgt_author,
                               .(citations_from_self = .N), by = .(author_id = tgt_author)]

  # Citations from other cartel members (excluding self)
  cartel_cites <- el_cartel_tgt[src_author != tgt_author & src_cartel != -1,
                                 .(citations_from_cartel = .N), by = .(author_id = tgt_author)]

  # Citations from background authors
  bg_cites <- el_cartel_tgt[src_cartel == -1,
                             .(citations_from_background = .N), by = .(author_id = tgt_author)]

  # Merge breakdowns onto cartel authors
  author_nl <- self_cites[author_nl, on = "author_id"]
  author_nl <- cartel_cites[author_nl, on = "author_id"]
  author_nl <- bg_cites[author_nl, on = "author_id"]

  # Zero-fill only for cartel authors; leave NA for background authors
  author_nl[is_cartel == TRUE & is.na(citations_from_self),       citations_from_self := 0L]
  author_nl[is_cartel == TRUE & is.na(citations_from_cartel),     citations_from_cartel := 0L]
  author_nl[is_cartel == TRUE & is.na(citations_from_background), citations_from_background := 0L]

  # --- Undirected author edge list (exclude self-citations) ---
  el_no_self <- el[src_author != tgt_author]
  el_no_self[, `:=`(
    a1 = pmin(src_author, tgt_author),
    a2 = pmax(src_author, tgt_author)
  )]
  author_el <- el_no_self[, .(weight = .N), by = .(author_id_1 = a1, author_id_2 = a2)]

  # --- Write outputs ---
  fwrite(author_el, file.path(run_dir, "author_edgelist.csv"))
  fwrite(author_nl, file.path(run_dir, "author_nodelist.csv"))

  rm(el, nl, agent_nodes, agent_ids, src, tgt, el_cartel_tgt,
     self_cites, cartel_cites, bg_cites, el_no_self, author_el, author_nl, cartel_authors)
  gc()

  cat("  Done\n")
}

cat("All runs complete\n")
