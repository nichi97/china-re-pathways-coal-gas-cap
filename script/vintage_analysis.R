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

# In retired plants, lifespan vs starting year
ggplot(df_coal_retire_trend, aes(y = lifeSpan, x = Year)) + 
  geom_point(position = "jitter") + 
  geom_smooth() + 
  labs(title = "Lifespan of Chinese Coal Plants over Time", 
       x = "Operation start year", 
       y = "Lifespan")

# linear model between lifespan and year
retire_model <- lm(lifeSpan ~ Year, df_coal_retire_trend)



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
coal_vintage <- coal %>% 
  filter(!Status %in% c("cancelled", "shelved",  "pre-permit", "mothballed", "announced"))

# Initialize list of years that acts like a dictionary. Store (year, change in capacity) pair
list_name = (coal_vintage$Year %>% min(na.rm=T)):2080 %>% as.character()
year_ls <- vector("list", length = length(list_name))
names(year_ls) <- list_name
for (i in seq_along(year_ls)){
  year_ls[[i]] <- 0
}

# change this to set default planned lifetime and construction for coal plants
PLANNED_LIFETIME <- 30
CONSTRUCTION_TIME <- 5 

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
    
  # 3. retired plant with start year but not retirement year 
  # Use start year to predict lifetime
  # We can do this because strong relationship between lifetime and start year (r^2 > 0.8) 
  } else if (curr_status == "retired" & !is.na(curr_start_yr) & is.na(curr_retire_yr)){
    curr_frame <- data.frame(Year = curr_start_yr)
    calc_retire_yr <- predict(retire_model, curr_frame) %>% round() + curr_start_yr 
    calc_retire_yr <- calc_retire_yr %>% as.character()
    year_ls[[as.character(curr_start_yr)]] <- year_ls[[as.character(curr_start_yr)]] + curr_cap
    year_ls[[as.character(calc_retire_yr)]] <- year_ls[[as.character(calc_retire_yr)]] - curr_cap
    
  # 4. retired plant without start year nor retirement year
  } else if (curr_status == "retired" & is.na(curr_start_yr) & is.na(curr_retire_yr)){
    next
    
  # 5. Operating plants with start year and planned retirement year and planned retirement year > 2022
  # calculate normally
  } else if (curr_status == "operating" & !is.na(curr_start_yr) & 
             !is.na(curr_plan_retire_yr & curr_plan_retire_yr > 2022)){
    year_ls[[as.character(curr_start_yr)]] <-
      year_ls[[as.character(curr_start_yr)]] + curr_cap
    year_ls[[as.character(curr_plan_retire_yr)]] <- 
      year_ls[[as.character(curr_plan_retire_yr)]] - curr_cap
    
  # 6. operating plants with start year, planned retirement year, but planned retirement year < 2022
  # 7. Operating plants with start year but no planned retirement year 
  # GEM need to update their data 
  # disregard planned retirement year, use planned_lifetime to calculate retirement year
  } else if ((curr_status == "operating" & !is.na(curr_start_yr) & 
             !is.na(curr_plan_retire_yr & curr_plan_retire_yr <= 2022)) | 
             (curr_status == "operating" & !is.na(curr_start_yr) & 
              is.na(curr_plan_retire_yr))){
    
    calc_planned_retire_yr <- curr_start_yr + planned_lifetime
    
    year_ls[[as.character(curr_start_yr)]] <-
      year_ls[[as.character(curr_start_yr)]] + curr_cap
    year_ls[[as.character(calc_planned_retire_yr)]] <- 
      year_ls[[as.character(calc_planned_retire_yr)]] - curr_cap
    
  # 8. under construction
  } else if (curr_status == "construction")
}

# 2.1  

