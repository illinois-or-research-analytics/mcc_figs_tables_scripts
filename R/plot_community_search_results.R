# plot_results.R
#
# Produces a side-by-side barplot comparing community search results from two
# SASCA simulation batches: "restrictive" (3/18) and "relaxed" (3/22).
#
# Input:
#   - 3_18_results.csv  (restrictive simulation results)
#   - 3_22_results.csv  (relaxed simulation results)
#   Each CSV has four columns: model, size, k, community_size
#     - model: ABM model type (control, null, phenotype)
#     - size: cartel size (5, 25, 125, 250)
#     - k: the k-core number from the community search result
#     - community_size: number of nodes in the returned community
#
# Output:
#   - combined_barplot.png
#   Bars are grouped by model (faceted), with cartel size on the x-axis.
#   For each configuration, restrictive and relaxed bars appear side by side.
#   The k-core number is annotated above each bar.
#
# Usage:
#   module load R/4.5.1-tidyverse
#   Rscript plot_results.R

library(ggplot2)
library(dplyr)

basedir <- "/scratch/yi54/experiments/3_22_sasca_cartel_community_search"

# Load both result sets and tag each with its simulation type
df_318 <- read.csv(file.path(basedir, "3_18_results.csv"),
                   colClasses = c(model = "character")) %>%
  mutate(experiment = "restrictive")

df_322 <- read.csv(file.path(basedir, "3_22_results.csv"),
                   colClasses = c(model = "character")) %>%
  mutate(experiment = "relaxed")

# Combine and set factor levels to control display order
df <- bind_rows(df_318, df_322) %>%
  mutate(
    model = factor(model, levels = c("control", "null", "phenotype")),
    size = factor(size, levels = c(5, 25, 125, 250)),
    experiment = factor(experiment, levels = c("restrictive", "relaxed"))
  )

# Build the plot
p <- ggplot(df, aes(x = size, y = community_size, fill = experiment)) +
  # Side-by-side bars for restrictive vs relaxed
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  # Annotate the k-core number above each bar
  geom_text(aes(label = paste0("k=", k)),
            position = position_dodge(width = 0.8),
            vjust = -0.3, size = 3.5, fontface = "bold") +
  # One facet per model, arranged in a single row
  facet_wrap(~ model, nrow = 1, scales = "free_x") +
  # Expand y-axis to make room for k annotations
  scale_y_continuous(expand = expansion(mult = c(0, 0.12))) +
  scale_fill_brewer(palette = "Dark2") +
  labs(x = "Cartel Size", y = "Community Size", fill = "Simulation") +
  theme_bw(base_size = 16) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "plain", size = 16),
    axis.title = element_text(size = 16),
    axis.text = element_text(size = 16),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 16)
  )

ggsave(file.path(basedir, "combined_barplot.png"), p, width = 14, height = 6, dpi = 150)
cat("Saved: combined_barplot.png\n")
