library(data.table)
library(xtable)

pct_dt <- fread("cartel_percentile_summary.csv")

# Extract the three percentiles of interest
pct_sel <- pct_dt[percentile %in% c(50, 90, 99)]

# Separate ctrl from cartel groups
ctrl <- pct_sel[group == "ctrl",
                .(condition, cartel_size, percentile, ctrl_value = value)]
grp  <- pct_sel[group %in% c("cartel-r", "cartel-p")]

merged <- grp[ctrl, on = c("condition", "cartel_size", "percentile")]
merged[, ratio := round(value / ctrl_value, 2)]

# Reshape: one row per condition x cartel_size x group, columns per percentile
wide <- dcast(merged,
              condition + cartel_size + group ~ percentile,
              value.var = "ratio")
setnames(wide, c("50", "90", "99"), c("P50", "P90", "P99"))

# Factor ordering
group_order <- c("cartel-r", "cartel-p")
cond_order  <- c("restrictive", "relaxed")
wide[, group      := factor(group,      levels = group_order)]
wide[, condition  := factor(condition,  levels = cond_order)]
wide[, cartel_size := factor(cartel_size,
                              levels = c(5, 25, 125, 250, 500, 1000),
                              labels = paste0("n=", c(5, 25, 125, 250, 500, 1000)))]
setorder(wide, condition, cartel_size, group)

out <- wide[, .(Condition = condition,
                `Cartel size` = cartel_size,
                Group = group,
                `P50 ratio` = P50,
                `P90 ratio` = P90,
                `P99 ratio` = P99)]

xt <- xtable(as.data.frame(out),
             caption = "Ratio of cartel-r and cartel-p in-degree to ctrl at the 50th, 90th, and 99th percentiles. Values $>1$ indicate a citation trading advantage over the same publication rate without trading.",
             label   = "tab:cartel_vs_ctrl",
             digits  = 2)

print(xt,
      include.rownames = FALSE,
      booktabs         = TRUE,
      file             = "cartel_vs_ctrl_ratio_table.tex")

fwrite(wide, "cartel_vs_ctrl_ratio_table.csv")
message("Saved: cartel_vs_ctrl_ratio_table.tex and cartel_vs_ctrl_ratio_table.csv")
