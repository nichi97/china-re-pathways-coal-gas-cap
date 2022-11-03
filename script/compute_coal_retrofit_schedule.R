library(tidyverse)
library(sf)
library(tmap)

coal <- read_csv("./data/processed_data/china_coal.csv")
emis_proj <- read_csv("./data/processed_data/power_sector_emission_2.csv")

calc_coal_cum_emis_retrofitted <- 
function(plant_df, emis_proj, retrofit_years, negative_emission,
         coal_lifetime = 40, gas_lifetime = 40, construction_time = 5, 
         capture_rate = 0.92){
  
  #' @param coal: The coal plant dataset 
  #' @param emis_proj: The emission projection, including both year and emission
  #' @param retrofit_years: the years at which the retrofit decisions are made.
  #' @param negative_emission: a dataframe that takes in both the year and the amount of negative emission at given years
  #' @param coal_lifetime: default lifetime of a coal fired power plant
  #' @param gas_lifetime: the default lifetime of a gas fired power plant
  #' @param construction_time: default time spends on constructing a coal fired power plant
  #' @param capture_rate: The capture rate for CCS technology
  #' @return a list that contains two objects. First, `emis` is the emission trajectory 
  #' under the current scheme of retrofitting. Second, `retrofit` is a dataframe containing
  #' information of the plants and the years at which these plants are retrofitted as determined
  #' by the algorithm.
  
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
    
    curr_final_start_year <-
      plant_vintage[[i, "final_start_year"]] %>% 
      as.character()
      
    curr_final_end_year <- 
      plant_vintage[[i, "final_end_year"]] %>% 
      as.character()
    
    year_ls[[curr_final_start_year]] <- year_ls[[curr_final_start_year]] + curr_emis
    year_ls[[curr_final_end_year]] <- year_ls[[curr_final_end_year]] - curr_emis
  }
  
  # retrofitting 
  retrofit_candidate <- 
    plant_vintage %>% filter(!is.na(Year)) %>% 
    mutate(age_at_2030 = 2030 - Year) %>% 
    filter(age_at_2030 < 25) %>% 
    filter(`Capacity (MW)` >= 400) %>% 
    filter(Status == "operating")
  
  # subtract negative emission 
 
  for(i in 1:nrow(negative_emission)){
    curr_row <- negative_emission %>% slice(i)
    curr_year <- curr_row$years %>% as.character()
    curr_emission <- curr_row$CO2_emission 
    
    # curr_emission is negative, so add it to subtract
    year_ls[[curr_year]] <- year_ls[[curr_year]] + curr_emission
  }
  
  # for those that does not have a start year or end year, filter it out.
  plant_vintage <- 
    plant_vintage %>% filter(!is.na(final_start_year) & !is.na(final_end_year))
  
  # record which plants was retrofitted, in which year
  # initialize and then add row to it later on
  retrofit_record <- tibble()
  
  for(loop_year in retrofit_years){
    
    # unretrofitted total emission
    cum_emis_df <- year_ls %>% cumsum() %>% as.tibble() %>% 
      mutate(year = (min(plant_vintage$Year, na.rm=T)):2080) %>% 
      rename(cum_emis = value) 
  
    # calculate the gap between proj and actual 
    emis_gap <- 
      (cum_emis_df %>% filter(year == loop_year) %>% pull(cum_emis)) - 
      (emis_proj %>% filter(Year == loop_year) %>% pull(CO2_emission)) 
    
    # calculate the remaining lifetime as of year loop_year
    retrofit_candidate <- 
      retrofit_candidate %>% 
      mutate(lifetime_remain = coal_lifetime - (this_year - Year)) %>% 
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
      
      # add to retrofit_record, adding the year in which the 
      # plant is retrofitted
      curr_cand_w_year <- 
        curr_cand %>% mutate(retrofit_year = loop_year)
      retrofit_record <- 
        retrofit_record %>% bind_rows(curr_cand_w_year)
      # take away from year_ls
      year_ls[[loop_year_ch]] <- year_ls[[loop_year_ch]] - curr_emis_reduct
      year_ls[[curr_final_year_ch]] <- year_ls[[curr_final_year_ch]] + curr_emis_reduct 
      # delete from curr_retrofit_candidate
      curr_retrofit_candidate <- curr_retrofit_candidate[-1,]
      # delete from retrofit_candidate
      retrofit_candidate <- retrofit_candidate %>% 
        filter(`Tracker ID` != curr_candidate)
      # delete from coal_vintage
      plant_vintage <- plant_vintage %>% 
        filter(`Tracker ID` != curr_candidate)
      # subtract from emis_gap
      emis_gap <- emis_gap - curr_emis_reduct
      
    }
    
  }
  # calculate the total emission after retrofitting 
  post_cum_emis_df <- year_ls %>% cumsum() %>% as.tibble() %>% 
    mutate(year = (min(plant_vintage$Year, na.rm=T)):2080) %>% 
    rename(cum_emis = value) 
  
  rst <- list()
  rst$emis <- post_cum_emis_df
  rst$retrofit <- retrofit_record
  
  return(rst)
}

emis_proj <- read_csv("./data/processed_data/power_sector_emission_2.csv")
negative_emis <- read_csv("./data/processed_data/negative_emission.csv")
coal_gas_merge <- read_csv("./data/processed_data/coal_gas_merge_final.csv")

retrofitted_rst <- calc_coal_cum_emis_retrofitted(coal_gas_merge, emis_proj,
                                                  seq(2040, 2060, 5),
                                                  negative_emis)

emis_under_retrofit <- retrofitted_rst$emis 

retrofit_schedule <- retrofitted_rst$retrofit 
write_csv(retrofit_schedule, "./data/processed_data/retrofit_schedule.csv")



# ------------------ Compare coal emis with emis proj --------

ggplot(emis_proj, aes(x = Year, y = CO2_emission)) + 
  geom_area(data=emis_under_retrofit, aes(y = cum_emis, x = year)) + 
  geom_point(col="red") + 
  geom_line(col="red") + 
  xlim(2020, 2060) + 
  scale_y_continuous(breaks = seq(from = -1000, to = 5000, by = 1000),
                     limits = c(-1000, 5000)) +
  labs(y = "CO2 emission (million ton)", 
       caption = "The red line is the projected CO2 emission for the entire electricity sector under the 2C scenario developed by 
       ICCSD (清华大学气候变化与可持续发展研究院). The shaded area represents the emission from coal-fired
       power plants and gas fired power plants under retrofitting. 40 years of lifetime is assumed.") + 
  ggtitle("Coal & Gas emission trajectory with retrofitting and BECCS") + 
  theme(plot.caption = element_text(hjust=0))
 
ggsave("./figures/coal_phaseout_retrofit_beccs_gas.png", units="cm", width=20,
       height=10)


# ----------------------- Retrofitted Gas and Coal plants viz -----------

china_shp <- st_read("./data/spatial_data/gadm40_CHN_1.shp")
retrofit_plants_rst <- retrofitted_rst$retrofit

retrofit_sf <- 
  st_as_sf(retrofit_plants_rst, coords = c("Longitude", "Latitude"))

retrofit_sf <- 
  retrofit_sf %>% mutate(retrofit_year_ch = as.character(retrofit_year))

st_crs(retrofit_sf) <- 4326

retrofit_map <- 
  tm_shape(china_shp) + 
  tm_borders(alpha = 0.4) + 
  tm_shape(retrofit_sf) + 
  tm_dots(col="retrofit_year_ch", shape = "resource", size=0.1) +
  tm_layout(title="Retrofitting Decisions by Year and Resources")
  
tmap_save(retrofit_map, "./figures/coal_gas_retrofit_time.png", 
          units = "cm", height = 17, width = 20)

# ------------------------ Coal and Gas CCS capacity ----------------------------
calc_retrofit_cap <- function(retrofit_plants, eff_factor = 0.95){
  list_name = (retrofit_plants$retrofit_year%>% min(na.rm=T)):2080 %>% as.character()
  year_ls <- vector("list", length = length(list_name))
  names(year_ls) <- list_name
  for (i in seq_along(year_ls)){
    year_ls[[i]] <- 0
  }
  
  for(i in 1:nrow(retrofit_plants)){
    curr_row = retrofit_plants %>% slice(i)
    curr_retire_yr <- curr_row$final_end_year %>% as.character()
    curr_start_yr <- curr_row$retrofit_year %>% as.character()
    # by default, assume 5% power loss due to CCS.
    curr_cap <- curr_row$`Capacity (MW)` * eff_factor
    
    year_ls[[curr_start_yr]] <- year_ls[[curr_start_yr]] + curr_cap
    year_ls[[curr_retire_yr]] <- year_ls[[curr_retire_yr]] - curr_cap
  }
  
  post_cum_emis_df <- year_ls %>% cumsum() %>% as.tibble() %>% 
    mutate(year = (min(retrofit_plants$retrofit_year, na.rm=T)):2080) %>% 
    rename(cum_emis = value) 
  
}
  
retrofit_coal <- retrofit_plants %>% filter(resource == "coal")
retrofit_gas<- retrofit_plants %>% filter(resource == "gas")

coal_ccs_rst <- calc_retrofit_cap(retrofit_coal)
gas_ccs_rst <- calc_retrofit_cap(retrofit_gas) 

gas_cap_vct <- 
gas_ccs_rst %>% filter(year %in% c(2020, 2030, 2040, 2050, 2060)) %>% 
  pull(cum_emis) / 1000

gas_cap_vct <- c(0, gas_cap_vct)

df <- 
  coal_ccs_rst %>% filter(year %in% c(2020, 2030, 2040, 2050, 2060)) %>% 
  mutate(cum_emis = cum_emis / 1000) %>% 
  mutate(gas_ccs = gas_cap_vct) %>% 
  rename(coal_ccs = cum_emis)

write_csv(df, "./data/processed_data/coal_gas_ccs_cap.csv")

  