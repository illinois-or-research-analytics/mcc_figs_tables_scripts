# Script to compare the accumulation of citations to authors who may
# be cartel members (either random or phenotype behavior) or not.
# Run on campys_cluster


library(data.table)
# null model (cartel members who cite otehr cartel members randomly)
rm(list=ls())
setwd('/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/3_18_min_cartel_experiments/null_model/size_25/proportion_0.50/r0/output')
el <- fread('edgelist.csv')
nl <- fread('nodelist.csv')
t1 <- merge(el,nl,by.x='source',by.y='node_id')
t1 <- t1[,.(source,syear=year,target)]
nl_el <-merge(t1,nl[,.(node_id,year,author_id)],by.x='target',by.y='node_id')[,.(source,syear,target,tyear=year,tauthor_id=author_id)]
cartel <- nl[cartel_id != -1,unique(author_id)]
temp <- nl_el[tauthor_id %in% cartel]
df <- temp[,.N,by=c('tauthor_id','tyear')]
fwrite(df,file='~/nulls.csv')

# phenotype model (cartel members who follow agent-
rm(list=ls())
setwd('/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/3_18_min_cartel_experiments/phenotype_model/size_25/proportion_0.50/r0/output')
el <- fread('edgelist.csv')
nl <- fread('nodelist.csv')
t1 <- merge(el,nl,by.x='source',by.y='node_id')
t1 <- t1[,.(source,syear=year,target)]
nl_el <-merge(t1,nl[,.(node_id,year,author_id)],by.x='target',by.y='node_id')[,.(source,syear,target,tyear=year,tauthor_id=author_id)]
cartel <- nl[cartel_id != -1,unique(author_id)]
temp <- nl_el[tauthor_id %in% cartel]
df <- temp[,.N,by=c('tauthor_id','tyear')]
fwrite(df,file='~/phenotype.csv')

# control model
rm(list=ls())
setwd('/projects/illinois/eng/cs/chackoge/illinoiscomputes/abm/v6/abm_outputs/3_18_min_cartel_experiments/control_model/size_25/proportion_0.50/r0/output')
el <- fread('edgelist.csv')
nl <- fread('nodelist.csv')
t1 <- merge(el,nl,by.x='source',by.y='node_id')
t1 <- t1[,.(source,syear=year,target)]
nl_el <-merge(t1,nl[,.(node_id,year,author_id)],by.x='target',by.y='node_id')[,.(source,syear,target,tyear=year,tauthor_id=author_id)]
cartel <- nl[cartel_id != -1,unique(author_id)]
temp <- nl_el[tauthor_id %in% cartel]
df <- temp[,.N,by=c('tauthor_id','tyear')]
fwrite(df,file='~/control.csv')

