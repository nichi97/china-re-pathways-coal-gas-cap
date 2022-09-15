library(tidyverse)
library(sf)
library(tmap)

plants_df <- read_csv("./data/processed_data/coal_gas_merge_final.csv")

compute_plants_retirement_schedule <- 
function(plant_df,
         coal_lifetime = 40, gas_lifetime = 40, construction_time = 5, 
         capture_rate = 0.92){
  
  #' @param coal: The coal plant dataset 
  #' @param retrofit_years: the years at which the retrofit decisions are made.
  #' @param negative_emission: a dataframe that takes in both the year and the amount of negative emission at given years
  #' @param coal_lifetime: default lifetime of a coal fired power plant
  #' @param gas_lifetime: the default lifetime of a gas fired power plant
  #' @param construction_time: default time spends on constructing a coal fired power plant
  #' @param capture_rate: The capture rate for CCS technology
  #' @return All the plants that are not yet processed
  #' @export retirement_schedule a csv file that contains the retirement schedule of 
  #' all the power plants.
  
  df_coal_retire_trend <- 
    plant_df %>% filter(Status == "retired")
  
  df_coal_retire_trend <- 
    df_coal_retire_trend %>% mutate(lifeSpan = RETIRED - Year)
  
  retire_model <- lm(lifeSpan ~ Year, df_coal_retire_trend)
  
  df_coal_retire_trend <- 
    df_coal_retire_trend %>% mutate(lifeSpan = RETIRED - Year)
  
  avg_lifetime <- mean(df_coal_retire_trend$lifeSpan, na.rm=T) %>% floor()
  
  # Filter out cancelled, shelved, pre-permit, mothballed, and announced coal plants 
  plant_vintage <- plant_df %>% 
    filter(!Status %in% c("cancelled", "shelved",  "pre-permit", "mothballed", "announced")) %>% 
    filter(!(Status == "operating" & is.na(Year) & is.na(`Planned Retire`))) %>% 
    mutate(final_end_year = NA, final_start_year = NA)
  
  # Initialize list of years that acts like a dictionary. Store (year, change in capacity) pair
  list_name = (plant_vintage$Year %>% min(na.rm=T)):2080 %>% as.character()
  year_ls <- vector("list", length = length(list_name))
  names(year_ls) <- list_name
  for (i in seq_along(year_ls)){
    year_ls[[i]] <- 0
  }
  
  # change this to set default planned lifetime and construction for coal plants
  this_year <- Sys.Date() %>% str_sub(1,4) %>% as.numeric()
  trouble_maker <- vector("numeric", 0)
  # run the main loop that calculate change in capacity for each year
  for(i in 1:nrow(plant_vintage)){
    curr_row = plant_vintage %>% slice(i)
    curr_status <- curr_row$Status
    curr_retire_yr <- curr_row$RETIRED
    curr_start_yr <- curr_row$Year
    curr_plan_retire_yr <- curr_row$`Planned Retire`
    curr_emis <- curr_row$`Annual CO2 (million tonnes / annum)`
    curr_resource <- curr_row$resource
    
   # Gas do not have any predicted retirement year yet. Assume 40 years of 
   # lifetime 
   if(curr_resource == "gas" & !is.na(curr_start_yr)){
      
      plant_vintage[[i, "final_start_year"]] <- curr_start_yr
      plant_vintage[[i, "final_end_year"]] <- curr_start_yr + gas_lifetime
      
     
    # 1. Retired, with start and retirement year
    # add emis at start year and subtract at retire year
    } else if (curr_status == "retired" & !is.na(curr_start_yr) & !is.na(curr_retire_yr)){

      plant_vintage[[i, "final_start_year"]] <- curr_start_yr
      plant_vintage[[i, "final_end_year"]] <- curr_retire_yr
      
      
    # 2. retired plants without start year but has retirement year 
    # assume average lifetime 22 years (average lifetime of retired plants in GEM)
    # add cap at calculated start year and subtract at retire year
    } else if (curr_status == "retired" & is.na(curr_start_yr) & !is.na(curr_retire_yr)){
      calc_start_yr <- curr_retire_yr - avg_lifetime

      plant_vintage[[i, "final_start_year"]] <- calc_start_yr
      plant_vintage[[i, "final_end_year"]] <- curr_retire_yr
      
    # 3. retired plant with start year but not retirement year 
    # Use start year to predict lifetime
    # We can do this because strong relationship between lifetime and start year (r^2 > 0.8) 
    } else if (curr_status == "retired" & !is.na(curr_start_yr) & is.na(curr_retire_yr)){
      curr_frame <- data.frame(Year = curr_start_yr)
      calc_retire_yr <- predict(retire_model, curr_frame) %>% round() + curr_start_yr 
      
      plant_vintage[[i, "final_start_year"]] <- curr_start_yr
      plant_vintage[[i, "final_end_year"]] <- calc_retire_yr 
      
    # 4. retired plant without start year nor retirement year
    } else if (curr_status == "retired" & is.na(curr_start_yr) & is.na(curr_retire_yr)){
      next
      
    # 5. Operating plants with start year and planned retirement year and planned retirement year > 2022
    # calculate normally
    } else if (curr_status == "operating" & !is.na(curr_start_yr) & 
               !is.na(curr_plan_retire_yr & curr_plan_retire_yr > 2022)){
      
      plant_vintage[[i, "final_start_year"]] <- curr_start_yr
      plant_vintage[[i, "final_end_year"]] <- curr_plan_retire_yr 
    # 6. operating plants with start year, planned retirement year, but planned retirement year < 2022
    # 7. Operating plants with start year but no planned retirement year 
    # 11. Permitted plants with start year 
    # GEM need to update their data 
    # disregard planned retirement year, use coal_lifetime to calculate retirement year
    } else if ((curr_status == "operating" & !is.na(curr_start_yr) & 
               !is.na(curr_plan_retire_yr & curr_plan_retire_yr <= 2022)) | 
               (curr_status == "operating" & !is.na(curr_start_yr) & 
                is.na(curr_plan_retire_yr)) | 
               (curr_status == "permitted" & !is.na(curr_start_yr))){
      
      calc_planned_retire_yr <- curr_start_yr + coal_lifetime
      
      plant_vintage[[i, "final_start_year"]] <- curr_start_yr
      plant_vintage[[i, "final_end_year"]] <- calc_planned_retire_yr 
      
    # 8. under construction with start year 
    # calculate planned retirement assuming coal_lifetime
    } else if (curr_status == "construction" & !is.na(curr_start_yr)){
      calc_construct_planned_retire_yr <- curr_start_yr + coal_lifetime 
      
      plant_vintage[[i, "final_start_year"]] <- curr_start_yr
      plant_vintage[[i, "final_end_year"]] <- calc_construct_planned_retire_yr 
      
    # 9. Plant under construction without operation year 
    # assume construction_time and coal_lifetime
    } else if (curr_status == "construction" & is.na(curr_start_yr)){
      calc_construct_start_yr <- this_year + construction_time 
      calc_construct_planned_retire_yr <- calc_construct_start_yr + coal_lifetime 
      
      plant_vintage[[i, "final_start_year"]] <- calc_construct_start_yr
      plant_vintage[[i, "final_end_year"]] <- calc_construct_planned_retire_yr 
    # 10. permitted without curr_start_yr. 
    # Assume unif(5,10) years to kick start the construction 
    # assume construction_time and coal_lifetime
    } else if (curr_status == "permitted" & is.na(curr_start_yr)){
      pre_construction_year <- runif(1, 5, 10) %>% round()
      calc_construct_start_yr <- this_year + pre_construction_year + construction_time
      calc_construct_planned_retire_yr <- calc_construct_start_yr + coal_lifetime
      
      plant_vintage[[i, "final_start_year"]] <- calc_construct_start_yr
      plant_vintage[[i, "final_end_year"]] <- calc_construct_planned_retire_yr 
      
    } else{
      trouble_maker <- append(trouble_maker, i)
    }
  }
  write_csv(plant_vintage, "./data/processed_data/plants_retirement_schedule.csv")
}

compute_plants_retirement_schedule(plants_df)



 