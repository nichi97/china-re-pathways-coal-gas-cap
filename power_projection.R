library(tidyverse)

# a dataframe that tracks the capacity projection
master_df <- tibble(year = seq(from=2020, to=2060, length.out=5))

# Nuclear power projection, (GW)
# According to Xiao & Jiang 2018, Currently we have 58 GW, and 
nuclear_cap <- seq(from=58, to=218, length.out=5)

master_df <- 
  master_df %>% mutate(nuclear = nuclear_cap)

# BECCS
BECCS_cap <- c(c(0),seq(from=0, to=110, length.out=4))

master_df <- 
  master_df %>% mutate(BECCS = BECCS_cap)

# Coal CHP CCS
CHP_cap <- rep(246, 5)

# assuming to be the same 
master_df <- 
  master_df %>% mutate(CHP = CHP_cap)

# Hydro GEIDCO 

master_df <- 
  master_df %>% mutate(Hydro = c(0, 441, 505.5, 570, 580))




