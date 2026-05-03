library(data.table)
library(ggplot2)

ct <- fread('citations_over_time_all_sizes.csv')

# annual citations per cartel author received from background authors
ct[, rate := N / n_cartel]

active <- ct[
  tyear >= 1981 &
  src_type == 'background' &
  tgt_type == 'cartel'
]

# mean across reps
avg <- active[, .(rate = mean(rate)), by = .(model, cartel_size, tyear)]

avg[, model := factor(model, levels = c('ctrl', 'cartel-r', 'cartel-p'))]
avg[, size_label := paste0('cartel size = ', cartel_size)]
avg[, size_label := factor(size_label,
    levels = paste0('cartel size = ', c(5, 25, 125, 250)))]

pal <- c('ctrl' = '#3266ad', 'cartel-r' = '#D85A30', 'cartel-p' = '#1D9E75')

p <- ggplot(avg, aes(x = tyear, y = rate, color = model)) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~ size_label, nrow = 2) +
  scale_color_manual(values = pal) +
  scale_x_continuous(breaks = seq(1981, 2012, by = 10)) +
  scale_y_continuous(limits = c(0, 170), breaks = seq(0, 150, by = 50)) +
  labs(
    x     = 'Year',
    y     = 'Annual citations per cartel author\n(from background authors)',
    color = NULL
  ) +
  theme_bw(base_size = 11) +
  theme(
    legend.position   = 'top',
    strip.background  = element_rect(fill = 'grey92'),
    strip.text        = element_text(face = 'bold'),
    panel.grid.minor  = element_blank()
  )

ggsave(p, file = 'bg_cartel_annual_citations.png', width = 10, height = 7, dpi = 300)
