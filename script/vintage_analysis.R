library(tidyverse)
library(ggpubr)
library(plotly)
coal <- read_csv("./data/processed_data/china_coal.csv")

# ----------------- Exploratory analysis --------------------------------------------

# Check the trend in retirement year for coal
df_coal_retire_trend <- 
  coal %>% filter(Status == "retired")

df_coal_retire_trend <- 
  df_coal_retire_trend %>% mutate(lifeSpan = RETIRED - Year)

# mean lifespan: 22 years
avg_lifetime <- mean(df_coal_retire_trend$lifeSpan, na.rm=T) %>% floor()

# In retired plants, lifespan vs starting year
lifespan_plot <- ggplot(df_coal_retire_trend, aes(y = lifeSpan, x = Year)) + 
  geom_point(position = "jitter") + 
  geom_smooth() + 
  labs(title = "Lifespan of Chinese Coal Plants over Time", 
       x = "Operation start year", 
       y = "Lifespan")

ggsave("./figures/lifespan_plot.png", lifespan_plot, unit="cm", width = 20)

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
retirement_over_time_plot <- 
  df_coal_retire_trend %>% 
  ggplot(aes(x = RETIRED)) + 
    geom_histogram(bins=15) + 
    labs(title = "Number of retired plants over time")
ggsave("./figures/retirement_over_time.png")

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

df_coal_planned_retire_trend %>% 
ggplot(aes(x = `lifespan`)) + 
 geom_histogram(bins=20)

# ---------------------------- Gas ----------------------------------------
gas <- read_csv("./data/processed_data/gas_merge_ready.csv")
gas_vintage <- gas %>% 
    filter(!Status %in% c("cancelled", "shelved",  "pre-permit", "mothballed", "announced")) %>% 
    filter(!(Status == "operating" & is.na(Year) & is.na(`Planned Retire`)))
list_name = (coal_vintage$Year %>% min(na.rm=T)):2080 %>% as.character()
year_ls <- vector("list", length = length(list_name))
names(year_ls) <- list_name
for (i in seq_along(year_ls)){
  year_ls[[i]] <- 0
}

 for(i in 1:nrow(coal_vintage)){
    curr_row = gas_vintage %>% slice(i)
    curr_status <- curr_row$Status
    curr_retire_yr <- curr_row$RETIRED
    curr_start_yr <- curr_row$Year
    curr_plan_retire_yr <- curr_row$`Planned Retire`
    curr_cap <- curr_row$`Capacity (MW)`
    curr_resource <- curr_row$resource
    
    if (!is.na(curr_cap) & !is.na(curr_start_yr)){
      curr_retire_yr <- curr_start_yr + 40
      year_ls[[as.character(curr_start_yr)]] <- year_ls[[as.character(curr_start_yr)]] + curr_cap
      year_ls[[as.character(curr_retire_yr)]] <- year_ls[[as.character(curr_retire_yr)]] - curr_cap
    }
 }

cum_cap_df <- year_ls %>% cumsum() %>% as.tibble() %>% 
  mutate(year = (min(gas_vintage$Year, na.rm=T)):2080) %>% 
  rename(cumCap = value) %>% 
  mutate(cumCap = cumCap / 1000)

focused_gas_df <- 
  cum_cap_df %>% filter(year %in% c(2020, 2030, 2040, 2050, 2060))
    
focused_gas_df

write_csv(focused_gas_df, "./data/processed_data/gas_cap.csv")

# change this to set default planned lifetime and construction for coal plants
this_year <- Sys.Date() %>% str_sub(1,4) %>% as.numeric()
trouble_maker <- vector("numeric", 0)


# ------------------------- Coal ----------------------------------------------------
calc_coal_cum_cap <- function(PLANNED_LIFETIME, CONSTRUCTION_TIME){
  
  #' @param PLANNED_LIFETIME: default lifetime of a coal fired power plant
  #' @param CONSTRUCTION_TIME: default time spends on constructing a coal fired power plant
  #' @return a data frame that store year and cumulative capacity 
  
  df_coal_retire_trend <- 
    coal %>% filter(Status == "retired")
  
  df_coal_retire_trend <- 
    df_coal_retire_trend %>% mutate(lifeSpan = RETIRED - Year)
  
  avg_lifetime <- mean(df_coal_retire_trend$lifeSpan, na.rm=T) %>% floor()
  
  # 1. Filter out cancelled, shelved, pre-permit, mothballed, and announced coal plants 
  coal_vintage <- coal %>% 
    filter(!Status %in% c("cancelled", "shelved",  "pre-permit", "mothballed", "announced")) %>% 
    filter(!(Status == "operating" & is.na(Year) & is.na(`Planned Retire`)))
  
  # Initialize list of years that acts like a dictionary. Store (year, change in capacity) pair
  list_name = (coal_vintage$Year %>% min(na.rm=T)):2080 %>% as.character()
  year_ls <- vector("list", length = length(list_name))
  names(year_ls) <- list_name
  for (i in seq_along(year_ls)){
    year_ls[[i]] <- 0
  }
  
  # change this to set default planned lifetime and construction for coal plants
  this_year <- Sys.Date() %>% str_sub(1,4) %>% as.numeric()
  trouble_maker <- vector("numeric", 0)
  # run the main loop that calculate change in capacity for each year
  for(i in 1:nrow(coal_vintage)){
    curr_row = coal_vintage %>% slice(i)
    curr_status <- curr_row$Status
    curr_retire_yr <- curr_row$RETIRED
    curr_start_yr <- curr_row$Year
    curr_plan_retire_yr <- curr_row$`Planned Retire`
    curr_cap <- curr_row$`Capacity (MW)`
    curr_resource <- curr_row$resource
    
    if (curr_resource == "gas" & !is.na(curr_cap) & !is.na(curr_start_yr)){
      curr_retire_yr <- curr_start_yr + 40
      year_ls[[as.character(curr_start_yr)]] <- year_ls[[as.character(curr_start_yr)]] + curr_cap
      year_ls[[as.character(curr_retire_yr)]] <- year_ls[[as.character(curr_retire_yr)]] - curr_cap
      
    # 1. Retired, with start and retirement year
    # add cap at start year and subtract at retire year
    }else if (curr_status == "retired" & !is.na(curr_start_yr) & !is.na(curr_retire_yr)){
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
    # 11. Permitted plants with start year 
    # GEM need to update their data 
    # disregard planned retirement year, use PLANNED_LIFETIME to calculate retirement year
    } else if ((curr_status == "operating" & !is.na(curr_start_yr) & 
               !is.na(curr_plan_retire_yr & curr_plan_retire_yr <= 2022)) | 
               (curr_status == "operating" & !is.na(curr_start_yr) & 
                is.na(curr_plan_retire_yr)) | 
               (curr_status == "permitted" & !is.na(curr_start_yr))){
      
      calc_planned_retire_yr <- curr_start_yr + PLANNED_LIFETIME
      
      year_ls[[as.character(curr_start_yr)]] <-
        year_ls[[as.character(curr_start_yr)]] + curr_cap
      year_ls[[as.character(calc_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_planned_retire_yr)]] - curr_cap
      
    # 8. under construction with start year 
    # calculate planned retirement assuming PLANNED_LIFETIME
    } else if (curr_status == "construction" & !is.na(curr_start_yr)){
      calc_construct_planned_retire_yr <- curr_start_yr + PLANNED_LIFETIME 
      year_ls[[as.character(curr_start_yr)]] <-
        year_ls[[as.character(curr_start_yr)]] + curr_cap
      year_ls[[as.character(calc_construct_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_construct_planned_retire_yr)]] - curr_cap
      
    # 9. Plant under construction without operation year 
    # assume CONSTRUCTION_TIME and PLANNED_LIFETIME
    } else if (curr_status == "construction" & is.na(curr_start_yr)){
      calc_construct_start_yr <- this_year + CONSTRUCTION_TIME 
      calc_construct_planned_retire_yr <- calc_construct_start_yr + PLANNED_LIFETIME 
      year_ls[[as.character(calc_construct_start_yr)]] <-
        year_ls[[as.character(calc_construct_start_yr)]] + curr_cap
      year_ls[[as.character(calc_construct_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_construct_planned_retire_yr)]] - curr_cap
      
    # 10. permitted without curr_start_yr. 
    # Assume unif(5,10) years to kick start the construction 
    # assume CONSTRUCTION_TIME and PLANNED_LIFETIME
    } else if (curr_status == "permitted" & is.na(curr_start_yr)){
      pre_construction_year <- runif(1, 5, 10) %>% round()
      calc_construct_start_yr <- this_year + pre_construction_year + CONSTRUCTION_TIME
      calc_construct_planned_retire_yr <- calc_construct_start_yr + PLANNED_LIFETIME
      year_ls[[as.character(calc_construct_start_yr)]] <-
        year_ls[[as.character(calc_construct_start_yr)]] + curr_cap
      year_ls[[as.character(calc_construct_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_construct_planned_retire_yr)]] - curr_cap   
      
    } else{
      trouble_maker <- append(trouble_maker, i)
    }
  }
  cum_cap_df <- year_ls %>% cumsum() %>% as.tibble() %>% 
    mutate(year = (min(coal_vintage$Year, na.rm=T)):2080) %>% 
    rename(cumCap = value) 
  
  return(cum_cap_df)
  
}

 
longer_lifetime_df <- calc_coal_cum_cap(40, 5)
longer_lifetime_df <- 
  longer_lifetime_df %>% mutate(cumCap = cumCap / 1000)

long_plot <- ggplot(longer_lifetime_df, aes(y = cumCap, x = year)) + 
  geom_line() + 
  geom_area() + 
  xlim(2000, 2080) + 
  labs(y = "Cumulative Capacity (GW)", 
       title = "Cumulative Coal Capacity, 40 years planned lifetime") + 
  scale_y_continuous(breaks=seq(0,1200, 200)) + 
  theme(text=element_text(size=20))

long_plot

focused_coal_df <- 
  longer_lifetime_df %>% filter(year %in% c(2020, 2030, 2040, 2050, 2060))

write_csv(focused_coal_df, "./data/processed_data/coal_capacity.csv")

coal_vintage_plot <- ggarrange(short_plot, mid_plot, long_plot, 
          ncol=1, nrow=3)

coal_vintage_plot
ggsave("./figures/coal_vintage_plot.png", coal_vintage_plot, dpi = 400, 
       units = "cm", height = 30, width = 40)

coal_retirement_plots <- ggarrange(lifespan_plot, 
                                   retirement_over_time_plot,
                                   labels=c("A", "B"))

coal_retirement_plots
ggsave("./figures/coal_retirementplots.png", coal_retirement_plots, dpi = 400, 
       units = "cm", height = 15, width = 30)

# save intermediate variables for ggplotly
saveRDS(short_plot, file="./temp/short_plot.rds")
saveRDS(mid_plot, file="./temp/mid_plot.rds")
saveRDS(long_plot, file="./temp/long_plot.rds")
saveRDS(lifespan_plot, file="./temp/lifespan_plot.rds")
saveRDS(retirement_over_time_plot, file="./temp/retirement_over_time_plot.rds")
          
          
          
          
          

