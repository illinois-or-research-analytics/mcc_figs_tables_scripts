library(data.table)

pct_dt <- fread("cartel_percentile_summary.csv")

# Extract median (50th percentile) for each group x condition x cartel_size
med <- pct_dt[percentile == 50, .(condition, cartel_size, group, median_indegree = value)]

# Pivot: get background median alongside cartel group medians
bg  <- med[group == "background", .(condition, cartel_size, bg_median = median_indegree)]
grp <- med[group != "background"]

merged <- grp[bg, on = c("condition", "cartel_size")]
merged[, ratio := median_indegree / bg_median]

# Order nicely
group_order <- c("ctrl", "cartel-r", "cartel-p")
cond_order  <- c("restrictive", "relaxed")
merged[, group     := factor(group,     levels = group_order)]
merged[, condition := factor(condition, levels = cond_order)]
merged[, cartel_size := factor(cartel_size,
                                levels = c(5, 25, 125, 250, 500, 1000),
                                labels = paste0("n=", c(5, 25, 125, 250, 500, 1000)))]

setorder(merged, condition, cartel_size, group)

cat("\nMedian in-degree ratio: cartel group / background\n")
cat("==================================================\n")
print(merged[, .(condition, cartel_size, group,
                 bg_median      = round(bg_median, 2),
                 cartel_median  = round(median_indegree, 2),
                 ratio          = round(ratio, 2))],
      row.names = FALSE)

fwrite(merged[, .(condition, cartel_size, group,
                  bg_median, cartel_median = median_indegree, ratio)],
       "cartel_median_ratios.csv")
message("\nSaved: cartel_median_ratios.csv")
