library(data.table)
library(ggplot2)
library(RColorBrewer)

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  stop("Usage: Rscript planted_comparison_boxplot_v2.R <series_base_dir> <outdir>")
}
base_dir <- args[1]
outdir   <- args[2]
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

load_reps <- function(base_dir, label) {
  dirs <- c(base_dir, paste0(base_dir, "_r1"), paste0(base_dir, "_r2"))
  rbindlist(lapply(seq_along(dirs), function(i) {
    cat(sprintf("  Loading %s rep%d...\n", label, i))
    nodes <- fread(file.path(dirs[[i]], "output/nodelist.csv"),
                   select = c("node_id", "type", "in_degree", "planted_nodes_line_number"))
    nodes[type == "agent"][, `:=`(condition = label, rep = i)]
  }))
}

cat("Loading conditions...\n")
exps <- list(
  list("p5_bsl_medians",  "control"),
  list("p5_od_hi",        "od_hi"),
  list("p5_fit_hi",       "fit_hi"),
  list("p5_na_hi",        "na_hi"),
  list("p5_od_fit",       "od+fit"),
  list("p5_od_na",        "od+na"),
  list("p5_fit_na",       "fit+na"),
  list("p5_od_fit_na_hi", "od+fit+na")
)

all_agents <- rbindlist(lapply(exps, function(e)
  load_reps(file.path(base_dir, e[[1]]), e[[2]])))

# Tier thresholds from fit_hi rep1 non-planted
ref   <- sort(all_agents[condition == "fit_hi" & rep == 1 & planted_nodes_line_number == -1, in_degree])
n_ref <- length(ref)
t001  <- ref[floor(0.9999 * n_ref)]
t01   <- ref[floor(0.999  * n_ref)]
t1    <- ref[floor(0.99   * n_ref)]
cat(sprintf("Thresholds: top 1%%>=%d  top 0.1%%>=%d  top 0.01%%>=%d\n", t1, t01, t001))

# Planted nodes only
planted <- all_agents[planted_nodes_line_number != -1]

# Background elite sample from fit_hi rep1 non-planted
set.seed(42)
bg <- all_agents[condition == "fit_hi" & rep == 1 & planted_nodes_line_number == -1]
bg_top1   <- bg[in_degree >= t1  & in_degree < t01][sample(.N, min(15, .N))][, condition := "bg top 1%"]
bg_top01  <- bg[in_degree >= t01 & in_degree < t001][sample(.N, min(15, .N))][, condition := "bg top 0.1%"]
bg_top001 <- bg[in_degree >= t001][sample(.N, min(15, .N))][, condition := "bg top 0.01%"]
background <- rbindlist(list(bg_top1, bg_top01, bg_top001))
background[, planted_nodes_line_number := -1L]

plot_data <- rbindlist(list(
  planted[, .(condition, in_degree)],
  background[, .(condition, in_degree)]
), fill = TRUE)

plot_data[, condition := factor(condition, levels = c(
  "control",
  "od_hi", "fit_hi", "na_hi",
  "od+fit", "od+na", "fit+na",
  "od+fit+na",
  "bg top 1%", "bg top 0.1%", "bg top 0.01%"
))]

tier_lines <- data.table(
  threshold = c(t1, t01, t001),
  label     = c("top 1%", "top 0.1%", "top 0.01%")
)

d2 <- brewer.pal(8, "Dark2")
cond_colours <- c(
  "control"    = "#878787",
  "od_hi"      = d2[1],
  "fit_hi"     = d2[1],
  "na_hi"      = d2[1],
  "od+fit"     = d2[2],
  "od+na"      = d2[2],
  "fit+na"     = d2[2],
  "od+fit+na"  = d2[3],
  "bg top 1%"   = "#543005",
  "bg top 0.1%" = "#543005",
  "bg top 0.01%"= "#543005"
)

p <- ggplot(plot_data, aes(x = condition, y = in_degree, colour = condition, fill = condition)) +
  geom_boxplot(alpha = 0.15, outlier.shape = NA, linewidth = 0.5, width = 0.6) +
  geom_jitter(width = 0.15, size = 2.5, alpha = 0.85) +
  geom_hline(data = tier_lines,
             aes(yintercept = threshold, linetype = label),
             colour = "grey30", linewidth = 0.6) +
  scale_linetype_manual(
    values = c("top 1%" = "dashed", "top 0.1%" = "dotdash", "top 0.01%" = "dotted"),
    name = "Elite tier"
  ) +
  scale_colour_manual(values = cond_colours, guide = "none") +
  scale_fill_manual(values = cond_colours, guide = "none") +
  scale_y_log10(labels = scales::comma) +
  labs(
    x = NULL,
    y = "In-degree (log10 scale)",
    subtitle = "Planted node in-degree by condition (3 replicates x 5 nodes)"
  ) +
  theme_bw(base_size = 16) +
  theme(panel.grid.minor = element_blank(),
        axis.text.x      = element_text(angle = 35, hjust = 1),
        legend.position  = "right")

ggsave(file.path(outdir, "planted_comparison_boxplot_v2.pdf"), p,
       width = 14, height = 8, device = cairo_pdf)
ggsave(file.path(outdir, "planted_comparison_boxplot_v2.eps"), p,
       width = 14, height = 8, device = cairo_ps)
ggsave(file.path(outdir, "planted_comparison_boxplot_v2.png"), p,
       width = 14, height = 8, dpi = 300)
cat("Saved planted_comparison_boxplot_v2 pdf/eps/png\n")

# Summary table
planted[, tier := fcase(
  in_degree >= t001, "top 0.01%",
  in_degree >= t01,  "top 0.1%",
  in_degree >= t1,   "top 1%",
  default            = "below top 1%"
)]
tab <- planted[, .(
  top_001      = sum(tier == "top 0.01%"),
  top_01       = sum(tier == "top 0.1%"),
  top_1        = sum(tier == "top 1%"),
  below        = sum(tier == "below top 1%"),
  max_indegree = max(in_degree)
), by = condition][order(condition)]
print(tab)
fwrite(tab, file.path(outdir, "planted_comparison_summary_v2.csv"))
cat("Done.\n")
