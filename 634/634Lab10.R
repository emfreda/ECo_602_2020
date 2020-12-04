require(here)
levels(rope$rope.type)
#gives us number of groups when you use length() on this


rm(list = ls())

rope <- read.csv(here("data/rope.csv")) 
rope$rope.type <- factor(rope$rope.type)

n_obs <- length(rope$rope.type)
n_groups <- length(levels(rope$rope.type))

grand_mean <- mean(rope$p.cut)
resids_rope <- c(grand_mean - rope$p.cut)
ss_tot <- sum(resids_rope^2)
df_tot <- n_obs - 1

agg_sq_resids <- aggregate(
                  x = rope$p.cut,
                  by = list(rope$rope.type),
                  FUN = function(x) sum((x - mean(x))^2))
ss_within <- sum(agg_sq_resids$x)
df_within <- n_obs - n_groups

ss_among <- ss_tot - ss_within
df_among <- n_groups - 1 

ms_within <- ss_within / df_within
ms_among <- ss_among / df_among

f_ratio <- ms_among / ms_within
f_pval <- pf(f_ratio, df1 = df_among,
                 df2 = df_within, lower.tail = F)
