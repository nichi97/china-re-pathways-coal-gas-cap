library(tidyverse)

gas <- read_csv("./data/raw_data/gas_unit.csv")
coal <- read_csv("./data/raw_data/coal_plant_tracker.csv")

# ---------------------- coal -------------------------------
china_coal <- coal %>% filter(Country == "China")

# deal with the mess in year 
# "19x0's -> assign 19x5 as Year. e.g. 1980's -> 1985
china_coal <- 
china_coal %>% mutate(Year = if_else(str_detect(Year, regex("19.0's")),
                               str_c(str_sub(Year, 1,3), "5"), 
                               Year))

# 1960-1966 -> 1963
china_coal <- 
  china_coal %>% mutate(Year = if_else(Year == "1960-1966", "1963", Year))
  
write_csv(china_coal, "./data/processed_data/china_coal.csv")

china_gas <- gas %>% filter(Country == "China")
write_csv(china_gas, "./data/processed_data/china_gas.csv")

# --------------------------- power sector emission -----------------------
power_emis <- read_csv("./data/raw_data/power_sector_emission_proj_2.csv")
power_emis <- 
  power_emis %>% mutate(Year = round(power_emis$Year))

write_csv(power_emis, "./data/processed_data/power_sector_emission_2.csv")

