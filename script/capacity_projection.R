library(tidyverse)

# a dataframe that tracks the capacity projection
master_df <- tibble(year = seq(from=2020, to=2060, length.out=5))

# Nuclear power projection, (GW)
# According to Xiao & Jiang 2018, Currently we have 58 GW, and 
nuclear_cap <- seq(from=58, to=218, length.out=4)
nuclear_cap <- c(nuclear_cap, 218)

master_df <- 
  master_df %>% mutate(nuclear = nuclear_cap)

# BECCS
BECCS_cap <- c(c(0),seq(from=0, to=110, length.out=4))

master_df <- 
  master_df %>% mutate(BECCS = BECCS_cap)

# Coal CHP CCS
CHP_cap <- seq(from=498, to=246, length.out=5)

# assuming to be the same 
master_df <- 
  master_df %>% mutate(CHP = CHP_cap)

# Hydro GEIDCO 

master_df <- 
  master_df %>% mutate(Hydro = c(0, 441, 505.5, 570, 580))

# coal 
coal_df <- read_csv("./data/processed_data/coal_capacity.csv")
master_df <- 
master_df %>% bind_cols(coal_df$cumCap) %>% rename(Coal=...6)


# gas
gas_df <- read_csv("./data/processed_data/gas_cap.csv")

master_df <- 
  master_df %>% bind_cols(gas_df$cumCap) %>% rename(Gas =...7)

coal_gas_ccs <- read_csv("./data/processed_data/coal_gas_ccs_cap.csv")

coal_gas_ccs <- 
  coal_gas_ccs %>% add_row(year = 2020, coal_ccs = 0, gas_ccs = 0) %>% 
  arrange(year)


coal_gas_ccs <- 
  coal_gas_ccs %>% select(coal_ccs, gas_ccs)

master_df <- 
  master_df %>% bind_cols(coal_gas_ccs)

master_df <- 
  master_df %>% 
  mutate(Coal = Coal - BECCS - coal_ccs,
         Gas = Gas - gas_ccs)
# gas and coal ccs




write_csv(master_df, "./data/processed_data/capacity_trajectory_final.csv")
