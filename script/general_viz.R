library(tidyverse)

df <- read_csv("./data/processed_data/capacity_trajectory_final2.csv")

df_longer <- 
df %>% pivot_longer(2:7, names_to = "resource", values_to="generation_capacity")

df_longer %>% 
  ggplot(aes(fill = resource, x = year, y = generation_capacity)) + 
  geom_area()

ggsave("./figures/generation_capacity_stack_w_gas.png")




