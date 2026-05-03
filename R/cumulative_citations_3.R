# modified to include size_125 and run on the campus cluster

library(data.table)

path <- '/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/3_18_min_cartel_experiments/'
setwd(path)

configs <- list(
  list(model = 'null_model',      tag1 = 'random_cartel'),
  list(model = 'phenotype_model', tag1 = 'phenotype_cartel'),
  list(model = 'control_model',   tag1 = 'control')
)

sizes <- c('size_25', 'size_5', 'size_250', 'size_125')

for (cfg in configs) {
  for (size in sizes) {
    for (r in paste0('r', 0:2)) {
      tryCatch({
        base <- file.path(cfg$model, size, 'proportion_0.50', r, 'output')
        el   <- fread(file.path(base, 'edgelist.csv'))
        nl   <- fread(file.path(base, 'nodelist.csv'))

        t1    <- merge(el, nl, by.x = 'source', by.y = 'node_id')[, .(source, syear = year, target)]
        nl_el <- merge(t1, nl[, .(node_id, year, author_id)], by.x = 'target', by.y = 'node_id')[,
                  .(source, syear, target, tyear = year, tauthor_id = author_id)]

        cartel <- nl[cartel_id != -1, unique(author_id)]
        df     <- nl_el[tauthor_id %in% cartel][, .N, by = .(tauthor_id, tyear)]
        df[, c('tag1', 'tag2', 'size') := .(cfg$tag1, r, size)]

        fwrite(df, file = sprintf('/u/chackoge/scratch/citation_analysis/%s_%s_%s.csv', gsub('_model', '', cfg$model), size, r))
        message(sprintf('Done: %s / %s / %s', cfg$model, size, r))

        rm(el, nl, t1, nl_el, df); gc()

      }, error = function(e) {
        message(sprintf('ERROR at %s / %s / %s: %s', cfg$model, size, r, e$message))
      })
    }
  }
}

