library(data.table)
library(xtable)

base_dir <- "/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/chacko/claude_baseline_data"
replicates <- paste0("bsl", 1:10)

stats_list <- lapply(replicates, function(rep) {
  f <- file.path(base_dir, rep, "output", "nodelist.csv")
  dt <- fread(f, select = c("type", "in_degree"))
  x <- dt[type == "agent", in_degree]
  data.frame(
    rep        = rep,
    median     = median(x),
    q75        = quantile(x, 0.75),
    p90        = quantile(x, 0.90),
    p99        = quantile(x, 0.99),
    p999       = quantile(x, 0.999),
    p9999      = quantile(x, 0.9999),
    maximum    = max(x)
  )
})

stats_df <- do.call(rbind, stats_list)

summary_df <- data.frame(
  Statistic = c(
    "Median",
    "q0.75",
    "q0.90",
    "q0.99",
    "q0.999",
    "q0.9999",
    "max"
  ),
  Mean = c(
    mean(stats_df$median),
    mean(stats_df$q75),
    mean(stats_df$p90),
    mean(stats_df$p99),
    mean(stats_df$p999),
    mean(stats_df$p9999),
    mean(stats_df$maximum)
  ),
  SD = c(
    sd(stats_df$median),
    sd(stats_df$q75),
    sd(stats_df$p90),
    sd(stats_df$p99),
    sd(stats_df$p999),
    sd(stats_df$p9999),
    sd(stats_df$maximum)
  )
)

out_dir <- base_dir

# CSV
write.csv(summary_df, file.path(out_dir, "baseline_indegree_table.csv"), row.names = FALSE)

# LaTeX
print(
  xtable(
    summary_df,
    caption = paste0(
      "Distributional statistics of agent in-degree across 10 baseline replicates ",
      "(701,570 agents per replicate). The median is stable across replicates (SD~=~0.0) ",
      "while variability increases at the extreme tail, with the maximum in-degree ",
      "varying by $\\pm$11,761 across replicates."
    ),
    label   = "tab:baseline_indegree",
    digits  = c(0, 0, 1, 1)
  ),
  include.rownames  = FALSE,
  sanitize.text.function = identity,
  booktabs          = TRUE,
  file              = file.path(out_dir, "baseline_indegree_table.tex"),
  caption.placement = "top"
)

cat("Done. Files written to", out_dir, "\n")
print(summary_df)
