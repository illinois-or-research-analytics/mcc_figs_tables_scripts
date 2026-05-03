library(data.table)
library(xtable)

pct_dt <- fread("cartel_percentile_summary.csv")

# Extract the three percentiles of interest
pct_sel <- pct_dt[percentile %in% c(50, 90, 99)]

# Separate background from cartel groups
bg  <- pct_sel[group == "background",
               .(condition, cartel_size, percentile, bg_value = value)]
grp <- pct_sel[group != "background"]

merged <- grp[bg, on = c("condition", "cartel_size", "percentile")]
merged[, ratio := round(value / bg_value, 2)]

# Reshape: one row per condition x cartel_size x group, columns per percentile
wide <- dcast(merged,
              condition + cartel_size + group ~ percentile,
              value.var = "ratio")
setnames(wide, c("50", "90", "99"), c("P50", "P90", "P99"))

# Factor ordering
group_order <- c("ctrl", "cartel-r", "cartel-p")
cond_order  <- c("restrictive", "relaxed")
wide[, group      := factor(group,      levels = group_order)]
wide[, condition  := factor(condition,  levels = cond_order)]
wide[, cartel_size := factor(cartel_size,
                              levels = c(5, 25, 125, 250, 500, 1000),
                              labels = paste0("n=", c(5, 25, 125, 250, 500, 1000)))]
setorder(wide, condition, cartel_size, group)

# Rename columns for LaTeX
out <- wide[, .(Condition = condition,
                `Cartel size` = cartel_size,
                Group = group,
                `P50 ratio` = P50,
                `P90 ratio` = P90,
                `P99 ratio` = P99)]

xt <- xtable(as.data.frame(out),
             caption = "Ratio of cartel group in-degree to background at the 50th, 90th, and 99th percentiles.",
             label   = "tab:cartel_ratios",
             digits  = 2)

print(xt,
      include.rownames = FALSE,
      booktabs         = TRUE,
      file             = "cartel_ratio_table.tex")

fwrite(wide, "cartel_ratio_table.csv")
message("Saved: cartel_ratio_table.tex and cartel_ratio_table.csv")
