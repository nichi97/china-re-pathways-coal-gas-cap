library(tidyverse)

coal <- read_csv("./data/processed_data/china_coal.csv")

calc_coal_cum_emis <- function(PLANNED_LIFETIME, CONSTRUCTION_TIME){
  
  #' @param PLANNED_LIFETIME: default lifetime of a coal fired power plant
  #' @param CONSTRUCTION_TIME: default time spends on constructing a coal fired power plant
  #' @return a data frame that contains year and cumulative emission 
  
  df_coal_retire_trend <- 
    coal %>% filter(Status == "retired")
  
  df_coal_retire_trend <- 
    df_coal_retire_trend %>% mutate(lifeSpan = RETIRED - Year)
  
  retire_model <- lm(lifeSpan ~ Year, df_coal_retire_trend)
  
  df_coal_retire_trend <- 
    df_coal_retire_trend %>% mutate(lifeSpan = RETIRED - Year)
  
  avg_lifetime <- mean(df_coal_retire_trend$lifeSpan, na.rm=T) %>% floor()
  
  retrofit_years = seq(from=2030, to=2060, by=10)
  
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
    curr_emis <- curr_row$`Annual CO2 (million tonnes / annum)`
    
    # 1. Retired, with start and retirement year
    # add emis at start year and subtract at retire year
    if (curr_status == "retired" & !is.na(curr_start_yr) & !is.na(curr_retire_yr)){
      year_ls[[as.character(curr_start_yr)]] <- year_ls[[as.character(curr_start_yr)]] + curr_emis
      year_ls[[as.character(curr_retire_yr)]] <- year_ls[[as.character(curr_retire_yr)]] - curr_emis
      
    # 2. retired plants without start year but has retirement year 
    # assume average lifetime 22 years (average lifetime of retired plants in GEM)
    # add cap at calculated start year and subtract at retire year
    } else if (curr_status == "retired" & is.na(curr_start_yr) & !is.na(curr_retire_yr)){
      calc_start_yr <- curr_retire_yr - avg_lifetime
      year_ls[[as.character(calc_start_yr)]] <- year_ls[[as.character(calc_start_yr)]] + curr_emis
      year_ls[[as.character(curr_retire_yr)]] <- year_ls[[as.character(curr_retire_yr)]] - curr_emis
      
    # 3. retired plant with start year but not retirement year 
    # Use start year to predict lifetime
    # We can do this because strong relationship between lifetime and start year (r^2 > 0.8) 
    } else if (curr_status == "retired" & !is.na(curr_start_yr) & is.na(curr_retire_yr)){
      curr_frame <- data.frame(Year = curr_start_yr)
      calc_retire_yr <- predict(retire_model, curr_frame) %>% round() + curr_start_yr 
      calc_retire_yr <- calc_retire_yr %>% as.character()
      year_ls[[as.character(curr_start_yr)]] <- year_ls[[as.character(curr_start_yr)]] + curr_emis
      year_ls[[as.character(calc_retire_yr)]] <- year_ls[[as.character(calc_retire_yr)]] - curr_emis
      
    # 4. retired plant without start year nor retirement year
    } else if (curr_status == "retired" & is.na(curr_start_yr) & is.na(curr_retire_yr)){
      next
      
    # 5. Operating plants with start year and planned retirement year and planned retirement year > 2022
    # calculate normally
    } else if (curr_status == "operating" & !is.na(curr_start_yr) & 
               !is.na(curr_plan_retire_yr & curr_plan_retire_yr > 2022)){
      year_ls[[as.character(curr_start_yr)]] <-
        year_ls[[as.character(curr_start_yr)]] + curr_emis
      year_ls[[as.character(curr_plan_retire_yr)]] <- 
        year_ls[[as.character(curr_plan_retire_yr)]] - curr_emis
      
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
        year_ls[[as.character(curr_start_yr)]] + curr_emis
      year_ls[[as.character(calc_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_planned_retire_yr)]] - curr_emis
      
    # 8. under construction with start year 
    # calculate planned retirement assuming PLANNED_LIFETIME
    } else if (curr_status == "construction" & !is.na(curr_start_yr)){
      calc_construct_planned_retire_yr <- curr_start_yr + PLANNED_LIFETIME 
      year_ls[[as.character(curr_start_yr)]] <-
        year_ls[[as.character(curr_start_yr)]] + curr_emis
      year_ls[[as.character(calc_construct_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_construct_planned_retire_yr)]] - curr_emis
      
    # 9. Plant under construction without operation year 
    # assume CONSTRUCTION_TIME and PLANNED_LIFETIME
    } else if (curr_status == "construction" & is.na(curr_start_yr)){
      calc_construct_start_yr <- this_year + CONSTRUCTION_TIME 
      calc_construct_planned_retire_yr <- calc_construct_start_yr + PLANNED_LIFETIME 
      year_ls[[as.character(calc_construct_start_yr)]] <-
        year_ls[[as.character(calc_construct_start_yr)]] + curr_emis
      year_ls[[as.character(calc_construct_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_construct_planned_retire_yr)]] - curr_emis
      
    # 10. permitted without curr_start_yr. 
    # Assume unif(5,10) years to kick start the construction 
    # assume CONSTRUCTION_TIME and PLANNED_LIFETIME
    } else if (curr_status == "permitted" & is.na(curr_start_yr)){
      pre_construction_year <- runif(1, 5, 10) %>% round()
      calc_construct_start_yr <- this_year + pre_construction_year + CONSTRUCTION_TIME
      calc_construct_planned_retire_yr <- calc_construct_start_yr + PLANNED_LIFETIME
      year_ls[[as.character(calc_construct_start_yr)]] <-
        year_ls[[as.character(calc_construct_start_yr)]] + curr_emis
      year_ls[[as.character(calc_construct_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_construct_planned_retire_yr)]] - curr_emis   
      
    } else{
      trouble_maker <- append(trouble_maker, i)
    }
    
    

  }
  cum_emis_df <- year_ls %>% cumsum() %>% as.tibble() %>% 
    mutate(year = (min(coal_vintage$Year, na.rm=T)):2080) %>% 
    rename(cum_emis = value) 
  
  return(cum_emis_df)
  
}


calc_coal_cum_emis_retrofitted <- 
function(coal, PLANNED_LIFETIME, CONSTRUCTION_TIME, 
         capture_rate = 0.92){
  
  #' @param PLANNED_LIFETIME: default lifetime of a coal fired power plant
  #' @param coal: The coal dataset 
  #' @param CONSTRUCTION_TIME: default time spends on constructing a coal fired power plant
  #' @return a data frame that contains year and cumulative emission 
  
  df_coal_retire_trend <- 
    coal %>% filter(Status == "retired")
  
  df_coal_retire_trend <- 
    df_coal_retire_trend %>% mutate(lifeSpan = RETIRED - Year)
  
  retire_model <- lm(lifeSpan ~ Year, df_coal_retire_trend)
  
  df_coal_retire_trend <- 
    df_coal_retire_trend %>% mutate(lifeSpan = RETIRED - Year)
  
  avg_lifetime <- mean(df_coal_retire_trend$lifeSpan, na.rm=T) %>% floor()
  
  # Filter out cancelled, shelved, pre-permit, mothballed, and announced coal plants 
  coal_vintage <- coal %>% 
    filter(!Status %in% c("cancelled", "shelved",  "pre-permit", "mothballed", "announced")) %>% 
    filter(!(Status == "operating" & is.na(Year) & is.na(`Planned Retire`))) %>% 
    mutate(final_end_year = 0)
  

  
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
    curr_emis <- curr_row$`Annual CO2 (million tonnes / annum)`
    
    # 1. Retired, with start and retirement year
    # add emis at start year and subtract at retire year
    if (curr_status == "retired" & !is.na(curr_start_yr) & !is.na(curr_retire_yr)){
      year_ls[[as.character(curr_start_yr)]] <- year_ls[[as.character(curr_start_yr)]] + curr_emis
      year_ls[[as.character(curr_retire_yr)]] <- year_ls[[as.character(curr_retire_yr)]] - curr_emis
      coal_vintage[[i, "final_end_year"]] <- curr_retire_yr
      
    # 2. retired plants without start year but has retirement year 
    # assume average lifetime 22 years (average lifetime of retired plants in GEM)
    # add cap at calculated start year and subtract at retire year
    } else if (curr_status == "retired" & is.na(curr_start_yr) & !is.na(curr_retire_yr)){
      calc_start_yr <- curr_retire_yr - avg_lifetime
      year_ls[[as.character(calc_start_yr)]] <- year_ls[[as.character(calc_start_yr)]] + curr_emis
      year_ls[[as.character(curr_retire_yr)]] <- year_ls[[as.character(curr_retire_yr)]] - curr_emis
      coal_vintage[[i, "final_end_year"]] <- curr_retire_yr
      
    # 3. retired plant with start year but not retirement year 
    # Use start year to predict lifetime
    # We can do this because strong relationship between lifetime and start year (r^2 > 0.8) 
    } else if (curr_status == "retired" & !is.na(curr_start_yr) & is.na(curr_retire_yr)){
      curr_frame <- data.frame(Year = curr_start_yr)
      calc_retire_yr <- predict(retire_model, curr_frame) %>% round() + curr_start_yr 
      year_ls[[as.character(curr_start_yr)]] <- year_ls[[as.character(curr_start_yr)]] + curr_emis
      year_ls[[as.character(calc_retire_yr)]] <- year_ls[[as.character(calc_retire_yr)]] - curr_emis
      
      coal_vintage[[i, "final_end_year"]] <- calc_retire_yr 
      
    # 4. retired plant without start year nor retirement year
    } else if (curr_status == "retired" & is.na(curr_start_yr) & is.na(curr_retire_yr)){
      next
      
    # 5. Operating plants with start year and planned retirement year and planned retirement year > 2022
    # calculate normally
    } else if (curr_status == "operating" & !is.na(curr_start_yr) & 
               !is.na(curr_plan_retire_yr & curr_plan_retire_yr > 2022)){
      year_ls[[as.character(curr_start_yr)]] <-
        year_ls[[as.character(curr_start_yr)]] + curr_emis
      year_ls[[as.character(curr_plan_retire_yr)]] <- 
        year_ls[[as.character(curr_plan_retire_yr)]] - curr_emis
      
      coal_vintage[[i, "final_end_year"]] <- curr_plan_retire_yr 
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
        year_ls[[as.character(curr_start_yr)]] + curr_emis
      year_ls[[as.character(calc_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_planned_retire_yr)]] - curr_emis
      
      coal_vintage[[i, "final_end_year"]] <- calc_planned_retire_yr 
    # 8. under construction with start year 
    # calculate planned retirement assuming PLANNED_LIFETIME
    } else if (curr_status == "construction" & !is.na(curr_start_yr)){
      calc_construct_planned_retire_yr <- curr_start_yr + PLANNED_LIFETIME 
      year_ls[[as.character(curr_start_yr)]] <-
        year_ls[[as.character(curr_start_yr)]] + curr_emis
      year_ls[[as.character(calc_construct_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_construct_planned_retire_yr)]] - curr_emis
      
      coal_vintage[[i, "final_end_year"]] <- calc_construct_planned_retire_yr 
      
    # 9. Plant under construction without operation year 
    # assume CONSTRUCTION_TIME and PLANNED_LIFETIME
    } else if (curr_status == "construction" & is.na(curr_start_yr)){
      calc_construct_start_yr <- this_year + CONSTRUCTION_TIME 
      calc_construct_planned_retire_yr <- calc_construct_start_yr + PLANNED_LIFETIME 
      year_ls[[as.character(calc_construct_start_yr)]] <-
        year_ls[[as.character(calc_construct_start_yr)]] + curr_emis
      year_ls[[as.character(calc_construct_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_construct_planned_retire_yr)]] - curr_emis
      
      coal_vintage[[i, "final_end_year"]] <- calc_construct_planned_retire_yr 
    # 10. permitted without curr_start_yr. 
    # Assume unif(5,10) years to kick start the construction 
    # assume CONSTRUCTION_TIME and PLANNED_LIFETIME
    } else if (curr_status == "permitted" & is.na(curr_start_yr)){
      pre_construction_year <- runif(1, 5, 10) %>% round()
      calc_construct_start_yr <- this_year + pre_construction_year + CONSTRUCTION_TIME
      calc_construct_planned_retire_yr <- calc_construct_start_yr + PLANNED_LIFETIME
      year_ls[[as.character(calc_construct_start_yr)]] <-
        year_ls[[as.character(calc_construct_start_yr)]] + curr_emis
      year_ls[[as.character(calc_construct_planned_retire_yr)]] <- 
        year_ls[[as.character(calc_construct_planned_retire_yr)]] - curr_emis   
      
      coal_vintage[[i, "final_end_year"]] <- calc_construct_planned_retire_yr 
      
    } else{
      trouble_maker <- append(trouble_maker, i)
    }
  }
  
  # retrofitting 
  retrofit_candidate <- 
    coal_vintage %>% filter(!is.na(Year)) %>% 
    mutate(age_at_2030 = 2030 - Year) %>% 
    filter(age_at_2030 < 20) %>% 
    filter(`Capacity (MW)` >= 600) %>% 
    filter(Status == "operating")
  
  for(loop_year in c(2030, 2040, 2050)){
    
    emis_gap <- 
      emis_proj %>% filter(Year == loop_year) %>% pull(CO2_emission)
    
    # calculate the remaining lifetime as of year loop_year
    retrofit_candidate <- 
      retrofit_candidate %>% 
      mutate(lifetime_remain = PLANNED_LIFETIME - (this_year - Year)) %>% 
      mutate(CO2_reduction = `Annual CO2 (million tonnes / annum)` * 
               capture_rate) %>% 
      mutate(ttl_CO2_reduction = CO2_reduction * lifetime_remain) %>% 
      arrange(desc(ttl_CO2_reduction))
    
    # filter out plants that have not been built yet
    curr_retrofit_candidate <- retrofit_candidate %>% 
      filter(ttl_CO2_reduction > 0)
    
    while(emis_gap > 0 && nrow(curr_retrofit_candidate) != 0){
      curr_cand <- curr_retrofit_candidate %>% slice(1)
      curr_emis_reduct <- curr_cand$CO2_reduction
      curr_gen_cap <- curr_cand$`Capacity (MW)`
      curr_final_year <- curr_cand$final_end_year
      curr_candidate <- curr_cand$`Tracker ID`
      
      loop_year_ch <- loop_year %>% as.character()
      curr_final_year_ch <- curr_final_year %>% as.character()
      
      # take away from year_ls
      year_ls[[loop_year_ch]] <- year_ls[[loop_year_ch]] - curr_emis_reduct
      year_ls[[curr_final_year_ch]] <- year_ls[[curr_final_year_ch]] + curr_emis_reduct 
      # delete from curr_retrofit_candidate
      curr_retrofit_candidate <- curr_retrofit_candidate[-1,]
      # delete from retrofit_candidate
      retrofit_candidate <- retrofit_candidate %>% 
        filter(`Tracker ID` != curr_candidate)
      # delete from coal_vintage
      coal_vintage <- coal_vintage %>% 
        filter(`Tracker ID` != curr_candidate)
      # subtract from emis_gap
      emis_gap <- emis_gap - curr_emis_reduct
      
    }
    
  }
  cum_emis_df <- year_ls %>% cumsum() %>% as.tibble() %>% 
    mutate(year = (min(coal_vintage$Year, na.rm=T)):2080) %>% 
    rename(cum_emis = value) 
  
  return(cum_emis_df)
  
}

retrofitted_rst <- calc_coal_cum_emis_retrofitted(40, 5)

longer_lifetime_df <- calc_coal_cum_emis(40, 5)

# ------------------ Compare coal emis with emis proj --------

emis_proj <- read_csv("./data/processed_data/power_sector_emission_2.csv")

# convert from 亿吨 to million ton 
emis_proj <- 
  emis_proj %>% mutate(CO2_emission = CO2_emission * 100)

# -550 is from the original paper, perceiving the electricity
# sector as a carbon sink for the other sectors
emis_proj <- 
  emis_proj %>% add_row(Year = 2060, CO2_emission = -550)

ggplot(emis_proj, aes(x = Year, y = CO2_emission)) + 
  geom_area(data=longer_lifetime_df, aes(y = cum_emis, x = year)) + 
  geom_point(col="red") + 
  geom_line(col="red") + 
  xlim(2020, 2060) + 
  scale_y_continuous(breaks = seq(from = -1000, to = 5000, by = 1000),
                     limits = c(-1000, 5000)) +
  labs(y = "CO2 emission (million ton)", 
       caption = "The red line is the projected CO2 emission for the entire electricity sector under the 2C scenario developed by 
       ICCSD (清华大学气候变化与可持续发展研究院). The shaded area is the emission from coal-fired
       power plants under natural phaseout. 40 years of lifetime is assumed.") + 
  ggtitle("Coal natural phaseout CO2 emission vs 2C scenario CO2 emission") + 
  theme(plot.caption = element_text(hjust=0))
  
ggsave("./figures/coal_phaseout_vs_proj_emis.png", units="cm", width=20)




