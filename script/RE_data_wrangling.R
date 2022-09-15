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
  power_emis %>% mutate(Year = round(power_emis$Year)) %>% 
  add_row(Year = 2060, CO2_emission = -5.5)

power_emis <- 
  power_emis %>% 
  add_row(Year = 2055, 
          CO2_emission = (power_emis[[7, "CO2_emission"]] + 
                            power_emis[[8, "CO2_emission"]]) / 2) %>% 
  mutate(CO2_emission = CO2_emission * 100) %>% 
  arrange(Year)

write_csv(power_emis, "./data/processed_data/power_sector_emission_2.csv")

# ------------------------- Negative emission data --------------------------

#' According to the paper, the negative emission at 2060 is 550 missiln ton 
#' for the entire electricity sector. We are assuming that BECCS starts to be 
#' built starting 2040, and the growth rate is linear from 2040 to 2060. 
#' Given these assumptions, we can derive the negative emission from BECCS
#' from 2040 to 2060.

negative_emission <- 
  tibble(years = seq(2030, 2060, 10),
         CO2_emission = seq(from=0, to=-550, length.out=4))

write_csv(negative_emission, "./data/processed_data/negative_emission.csv")


# ----------------------------- merge coal and gas ---------------------------

gas <- read_csv("./data/processed_data/china_gas.csv")
coal <- read_csv("./data/processed_data/china_coal.csv")

# clean the capacity column
gas <- 
  gas %>% 
  na_if("not found") %>% 
  mutate(`Start year` = if_else(`Start year` == "2012-2013", 
                 "2012", 
                 `Start year`)) %>% 
  mutate(`Capacity elec. (MW)` = as.numeric(`Capacity elec. (MW)`), 
         `Start year` = as.numeric(`Start year`)) %>% 
  filter(!is.na(`Start year`))

# Calculate an estimate for gas CO2 emission, using a capacity factor of 
# 0.3 as calculated per https://cec.org.cn/detail/index.html?3-307614
# and a emission factor of 0.5 tCO2/tce -> 0.2 tCO2/MWh
gas <- 
  gas %>% mutate(`Annual CO2 (million tonnes / annum)` = 
                   `Capacity elec. (MW)` * 365 * 24 * 0.3 * 0.5 / 1e6)

# make sure gas and coal has the same column names
gas_merge <- 
  gas %>% 
  rename(Plant = `Plant name`, 
         `Chinese Name` = `Plant name (local script)`,
         `Capacity (MW)` = `Capacity elec. (MW)`,
         Year = `Start year`, 
         `RETIRED` = `Retired year`, 
         `Planned Retire` = `Planned retire`, 
         `Tracker ID` = `GEM unit ID`) %>% 
  select(Plant, `Chinese Name`, `Capacity (MW)`, `Status`, Year, `RETIRED`,
         `Planned Retire`, Latitude, Longitude, `Tracker ID`, 
         `Annual CO2 (million tonnes / annum)`, `Subnational unit (province, state)`) %>% 
  mutate(resource = "gas")

write_csv(gas_merge, "./data/processed_data/gas_merge_ready.csv")


# clean gas
  
coal_merge <- 
  coal %>% 
  mutate(resource = "coal")

coal_gas_merge <- bind_rows(coal_merge, gas_merge)
coal_gas_merge <- 
  coal_gas_merge %>% filter(!is.na(Year)) %>% 
  filter(!is.na(`Annual CO2 (million tonnes / annum)`))
  

write_csv(coal_gas_merge, "./data/processed_data/coal_gas_merge_final.csv")
  