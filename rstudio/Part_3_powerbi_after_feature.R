library(dplyr)
library(readr)

# Load the original dataset
df <- read_csv("E:\\6th attempt\\powerbi_dataset\\Dataset_For_powerBI.csv")

# Remove redundant and unnecessary columns
df <- df %>%
  select(-c(date.x, time.x, date.y, time.y, datetime, 
            local_authority_district, local_authority_ons_district, icon))

# Create new analytical columns
df <- df %>%
  mutate(
    # KSI Flag
    ksi_flag = if_else(collision_severity %in% c(1, 2), 1L, 0L),
    
    # Police Reported Weather
    police_reported_weather = case_when(
      weather_conditions %in% c(1, 4) ~ "Clear",
      weather_conditions %in% c(2, 5) ~ "Rain",
      weather_conditions %in% c(3, 6) ~ "Snow",
      weather_conditions == 7         ~ "Fog",
      weather_conditions == 8         ~ "Other",
      TRUE                            ~ "Unknown"
    ),
    
    # Weather Type
    weather_type = case_when(
      freezing_risk_flag == 1 ~ "Freezing Risk",
      heavy_rain_flag == 1    ~ "Heavy Rain",
      TRUE                    ~ "Clear/Other"
    )
  )

# Fix ONLY NA values in visibility
df <- df %>%
  mutate(
    visibility = if_else(
      is.na(visibility),
      case_when(
        police_reported_weather == "Clear" ~ 38.7,
        temp <= 2 ~ 2.0,
        TRUE ~ median(visibility, na.rm = TRUE)
      ),
      visibility
    )
  )

# Weather Match Flag (after visibility is cleaned)
df <- df %>%
  mutate(
    weather_match_flag = case_when(
      police_reported_weather %in% c("Unknown", "Other") ~ 0L,
      
      police_reported_weather == "Snow" &
        !is.na(snow) & snow > 0 ~ 1L,
      
      police_reported_weather == "Rain" &
        (is.na(snow) | snow == 0) &
        !is.na(precip) & precip > 0 ~ 1L,
      
      police_reported_weather == "Fog" &
        !is.na(visibility) & visibility < 5 ~ 1L,
      
      police_reported_weather == "Clear" &
        (is.na(snow) | snow == 0) &
        (is.na(precip) | precip == 0) &
        !is.na(visibility) & visibility >= 5 ~ 1L,
      
      TRUE ~ 0L
    )
  )

# Save the cleaned dataset
write_csv(df, "E:\\6th attempt\\powerbi_dataset\\final_For_powerBI.csv")
