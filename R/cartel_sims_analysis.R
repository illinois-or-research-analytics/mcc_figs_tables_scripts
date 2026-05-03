# import cartel sims into a single data.table
setwd('~/Desktop/abm/cartels')

library(data.table); library(stringr); library(ggplot2)
rm(list=ls())

files <- list.files(pattern = "^(control|null|phenotype)_size_.*\\.csv$")

dt_list <- lapply(files, function(f) {
  dt <- fread(f)
  
  # Assign model label
  dt[, model := fcase(
    grepl("control",   f), "ctrl",
    grepl("phenotype", f), "cartel-p",
    grepl("null",      f), "cartel-r"
  )]
  
  # Extract integer from size_X pattern
  dt[, cartel_size := as.integer(sub(".*_size_(\\d+)_.*", "\\1", f))]
  
  dt
})

data <- rbindlist(dt_list, fill = TRUE)
data1 <- data[,.(tauthor_id, tyear, N, rep=tag2, model, cartel_size)]
percs <- data1[,as.list(quantile(N,probs=seq(0,1,by=0.05))),.(model,cartel_size, rep)]
mpercs <- data.table::melt(percs,id.vars=c('model','cartel_size', 'rep'))
mpercs[,variable:=str_sub(variable,end=-2)]
mpercs$variable <- factor(mpercs$variable, levels = seq(0, 100, by = 5))

p <- ggplot(data=mpercs, aes(x=variable,y=log10(value),group=model)) + 
geom_point(aes(color=model)) + geom_line(aes(color=model)) + 
facet_grid(rep~cartel_size) + 
scale_x_discrete(breaks = as.character(seq(0, 100, by = 10))) + xlab("percentile") + 
ylab("citations per cartel author") + theme_bw() + geom_hline(yintercept=2) + 
theme(axis.text.x = element_text(angle = -70, vjust = 0.5, hjust=0)) 

ggsave(p,file='cartel_percs.png',device="png")
ggsave(p,file='cartel_percs.pdf',device="pdf")
ggsave(p,file='cartel_percs.eps',device="eps")

