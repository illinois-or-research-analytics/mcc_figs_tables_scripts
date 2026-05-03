library(data.table)

# --- Directory structure ---
root_dir   <- "/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/3_18_min_cartel_experiments"
output_csv <- "cartel_ecdf_summary_pooled.csv"

models     <- c("control_model", "null_model", "phenotype_model")
sizes      <- c(5, 25, 125, 250)
proportion <- "proportion_0.50"
reps       <- c("r0", "r1", "r2")

model_labels <- c(
  control_model   = "ctrl",
  null_model      = "cartel-r",
  phenotype_model = "cartel-p"
)

results <- list()

for (model in models) {
  for (size in sizes) {

    tag <- sprintf("%s | size_%s", model, size)

    # Accumulate background and cartel in_degrees across all reps
    bg_pool     <- c()
    cartel_pool <- c()

    for (rep in reps) {

      path      <- file.path(root_dir, model,
                             paste0("size_", size),
                             proportion, rep, "output")
      node_file <- file.path(path, "nodelist.csv")

      if (!file.exists(node_file)) {
        warning(paste("Missing nodelist:", tag, rep))
        next
      }

      nodes  <- fread(node_file,
                      select = c("node_id", "type", "in_degree",
                                 "author_id", "cartel_id"))
      agents <- nodes[type == "agent"]

      bg_pool     <- c(bg_pool,     agents[cartel_id == -1, in_degree])
      cartel_pool <- c(cartel_pool, agents[cartel_id != -1, in_degree])
    }

    if (length(cartel_pool) == 0) {
      warning(paste("No cartel agent nodes found:", tag))
      next
    }

    # Build ECDF from pooled background and evaluate pooled cartel nodes
    bg_ecdf        <- ecdf(bg_pool)
    cartel_pctiles <- bg_ecdf(cartel_pool)

    row <- data.table(
      model                  = model_labels[[model]],
      cartel_size            = size,
      n_bg                   = length(bg_pool),
      n_cartel               = length(cartel_pool),
      cartel_indegree_min    = min(cartel_pool),
      cartel_indegree_q1     = quantile(cartel_pool, 0.25),
      cartel_indegree_median = median(cartel_pool),
      cartel_indegree_mean   = mean(cartel_pool),
      cartel_indegree_q3     = quantile(cartel_pool, 0.75),
      cartel_indegree_max    = max(cartel_pool),
      pctile_min             = min(cartel_pctiles),
      pctile_q1              = quantile(cartel_pctiles, 0.25),
      pctile_median          = median(cartel_pctiles),
      pctile_mean            = mean(cartel_pctiles),
      pctile_q3              = quantile(cartel_pctiles, 0.75),
      pctile_max             = max(cartel_pctiles)
    )

    results[[length(results) + 1]] <- row

    message(sprintf("Done: %s | n_bg=%d | n_cartel=%d | median_pctile=%.3f",
                    tag, length(bg_pool), length(cartel_pool),
                    median(cartel_pctiles)))
  }
}

# --- Combine and save ---
dt_out <- rbindlist(results)
setorder(dt_out, model, cartel_size)
fwrite(dt_out, output_csv)
message("\nSaved: ", output_csv)

# --- Print results ---
message("\n--- Pooled percentile stats (model x size) ---")
print(dt_out[, .(model, cartel_size, n_bg, n_cartel,
                 pctile_min, pctile_q1, pctile_median,
                 pctile_mean, pctile_q3, pctile_max)])
                
 
 ### Run on 3_18 file hierarchy (restrictive) and 3_22 (relaxed) on cluster               
 ### Process locally into a table for supplementary methods
 
 library(data.table); library(xtable)
 cartel_percs_restricted <- fread('~/Desktop/abm/3_18_cartel_ecdf_summary_pooled.csv')
 cartel_percs_relaxed <- fread('~/Desktop/abm/3_22_cartel_ecdf_summary_pooled.csv')
 cartel_percs_relaxed[,tag:='relx']
 cartel_percs_restricted[,tag:='restr']
 df <- rbind(cartel_percs_restricted,cartel_percs_relaxed)
 xtable(df) # copy output into Overleaf document and add caption and label.
 
                
                
                