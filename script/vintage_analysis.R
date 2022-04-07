library(tidyverse)

coal <- read_csv("./data/processed_data/china_coal.csv")
gas <- read_csv("./data/processed_data/china_gas.csv")

# ----------------- Exploratory analysis --------------------------------------------

# Check the trend in retirement year for coal
df_coal_retire_trend <- 
  coal %>% filter(Status == "retired")

df_coal_retire_trend <- 
  df_coal_retire_trend %>% mutate(lifeSpan = RETIRED - Year)

# mean lifespan: 22 years
avg_lifetime <- mean(df_coal_retire_trend$lifeSpan, na.rm=T) %>% floor()

# lifespan vs starting year
ggplot(df_coal_retire_trend, aes(y = lifeSpan, x = Year)) + 
  geom_point(position = "jitter") + 
  geom_smooth() + 
  labs(title = "Lifespan of Chinese Coal Plants over Time", 
       x = "Operation start year", 
       y = "Lifespan")

# lifespan vs year of retirement: very little predictive power.
ggplot(df_coal_retire_trend, aes(y = lifeSpan, x = RETIRED)) + 
  geom_point(position = "jitter") + 
  geom_smooth() + 
  labs(title = "Lifespan of Chinese Coal Plants over Years of Retirement")

# Maybe this is because all the plants are retiring at the same time?
# Well... not exactly.
# there has been a sharp increase in retirement since 2005. 
df_coal_retire_trend %>% 
ggplot(aes(x = RETIRED)) + 
  geom_histogram(bins=15) + 
  labs(title = "")

# Now, let's look at operating plants: what is the relationship between the 
# starting year and planned retirement year?
df_operating_coal <- coal %>% filter(Status == "operating")

df_coal_planned_retire_trend <- coal %>% filter(Status == "operating" & 
                                             !is.na(Year) & 
                                             !is.na(`Planned Retire`))

df_coal_planned_retire_trend <- 
  df_coal_planned_retire_trend %>% mutate(lifespan = `Planned Retire` - `Year`)

df_coal_planned_retire_trend %>% 
  ggplot(aes(x = Year, y = lifespan)) + 
  geom_point(position="jitter") + 
  labs(y = "Planned lifespan", x = "Operation Start Year", 
       title = "Planned Lifespan of Coal Plants over Time")
  
df_coal_planned_retire_trend %>% 
ggplot(aes(x = `Planned Retire`)) + 
  geom_histogram(bins=15)


# ------------------------- Coal ----------------------------------------------------

# 1. Filter out cancelled, shelved, pre-permit, mothballed, and announced coal plants 
coal_vintage <- coal %>% filter(!Status %in% c("cancelled", "shelved", 
                                               "pre-permit", "mothballed", "announced"))

# Initialize list of years that acts like a dictionary. Store (year, change in capacity) pair
list_name = (coal_vintage$Year %>% min(na.rm=T)):2080 %>% as.character()
year_ls <- vector("list", length = length(list_name))
names(year_ls) <- list_name
for (i in seq_along(year_ls)){
  year_ls[[i]] <- 0
}

# run the main loop that calculate change in capacity for each year
for(i in 1:nrow(coal_vintage)){
  print(i)
  curr_row = coal_vintage %>% slice(i)
  curr_status <- curr_row$Status
  curr_retire_yr <- curr_row$RETIRED
  curr_start_yr <- curr_row$Year
  curr_plan_retire_yr <- curr_row$`Planned Retire`
  curr_cap <- curr_row$`Capacity (MW)`
  
  # 1. Retired, with start and retirement year
  # add cap at start year and subtract at retire year
  if (curr_status == "retired" & !is.na(curr_start_yr) & !is.na(curr_retire_yr)){
    year_ls[[as.character(curr_start_yr)]] <- year_ls[[as.character(curr_start_yr)]] + curr_cap
    year_ls[[as.character(curr_retire_yr)]] <- year_ls[[as.character(curr_retire_yr)]] - curr_cap
    
  # 2. retired plants without start year but has retirement year 
  # assume average lifetime 22 years (average lifetime of retired plants in GEM)
  # add cap at calculated start year and subtract at retire year
  } else if (curr_status == "retired" & is.na(curr_start_yr) & !is.na(curr_retire_yr)){
    calc_start_yr <- curr_retire_yr - avg_lifetime
    year_ls[[as.character(calc_start_yr)]] <- year_ls[[as.character(calc_start_yr)]] + curr_cap
    year_ls[[as.character(curr_retire_yr)]] <- year_ls[[as.character(curr_retire_yr)]] - curr_cap
  }
}

# 2.1  

