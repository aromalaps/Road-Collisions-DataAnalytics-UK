# 1. Load necessary libraries for data manipulation, visualization, and cleaning
library(tidyverse)   # for data manipulation and visualization
library(lubridate)   # for working with date and time
library(hms)         # for time-specific operations
library(skimr)       # for quick data quality checks
library(ggplot2)     # for plotting
library(dplyr)      # data manipulation
library(janitor)    # clean column names and duplicates
library(readr)      # read/write CSVs
library(scales)     # format numbers in plots





# Load STATS19 collision data set (raw)
collisions_raw <- read_csv("E:\\6th attempt\\final_collision.csv")

# Load hourly weather data (raw)
weather_raw <- read_csv("E:\\6th attempt\\visual_crossing_dataset_lon_ma_2.csv")

# Check structure of data sets to understand columns and data types
dplyr::glimpse(collisions_raw)
dplyr::glimpse(weather_raw)

# Initial data quality checks using skimr
skim(collisions_raw)   # checks for missing values, data types, duplicates
skim(weather_raw)

# Clean column names in collisions dataset to make them consistent
collisions <- collisions_raw %>%
  janitor::clean_names()

# Create a single datetime column and an hourly bucket for collisions
collisions <- collisions %>%
  mutate(
    collision_datetime = as.POSIXct(date) + as.numeric(time, units = "secs"),
    collision_hour     = lubridate::floor_date(collision_datetime, unit = "hour")
  )

# Filter dataset to only include Greater London and Greater Manchester
collisions <- collisions %>%
  filter(region %in% c("Greater London", "Greater Manchester"))

# Compare old names vs cleaned names for reference
cleaned_names <- collisions_raw %>%
  janitor::clean_names() %>%
  colnames()

tibble(
  original = colnames(collisions_raw),
  cleaned  = cleaned_names
) %>%
  filter(original != cleaned)

# Check first 10 rows of datetime conversion
collisions %>%
  distinct(collision_datetime, collision_hour) %>%
  head(10)

# Check unique regions and number of rows after filtering
unique(collisions$region)
nrow(collisions)

# Replace invalid placeholder values (-1) with NA in selected fields
collisions <- collisions %>%
  mutate(
    local_authority_district = na_if(local_authority_district, -1),
    speed_limit              = na_if(speed_limit, -1),
    junction_control         = na_if(junction_control, -1),
    junction_detail          = na_if(junction_detail, -1),
    weather_conditions       = na_if(weather_conditions, -1),
    road_surface_conditions  = na_if(road_surface_conditions, -1)
  )

# Check if any -1 values remain
sum(collisions$speed_limit == -1, na.rm = TRUE)

# Keep only relevant variables for merging and modeling
collisions <- collisions %>%
  select(
    collision_index,
    collision_year,
    date,
    time,
    region,
    borough_name,
    local_authority_district,
    local_authority_ons_district,
    collision_severity,
    number_of_vehicles,
    number_of_casualties,
    weather_conditions,
    road_type,
    first_road_class,
    speed_limit,
    collision_hour
  )

# Remove duplicate rows
collisions <- collisions %>%
  distinct()

# Quick checks of dataset after cleaning
nrow(collisions)
summary(collisions$collision_year)
sum(is.na(collisions$collision_hour))

# Clean weather dataset

weather <- weather_raw %>%
  clean_names() %>%
  mutate(
    region = str_to_title(region),                 # match collisions spelling
    datetime = dmy_hm(datetime, tz = "UTC"),        # CORRECT parse: "01-01-2010 00:00"
    weather_hour = floor_date(datetime, "hour")
  ) %>%
  filter(region %in% c("Greater London", "Greater Manchester"))


# Compare column name changes
name_comparison <- tibble(
  original = colnames(weather_raw),
  cleaned  = colnames(weather)[1:ncol(weather_raw)]
) %>% 
  filter(original != cleaned)
print(name_comparison)

# Keep only required weather variables and create additional time features
weather <- weather %>%
  select(
    region,
    datetime,
    weather_hour,
    temp,
    humidity,
    precip,
    snow,
    cloudcover,
    visibility,
    icon,
    conditions,
    date,
    time 
  ) %>%
  mutate(
    weather_date  = as.Date(weather_hour),
    weather_year  = year(weather_hour),
    weather_month = month(weather_hour),
    weather_dow   = wday(weather_hour, label = TRUE),
    weather_hod   = hour(weather_hour)
  )

# Create hazard feature flags for heavy rain and freezing risk
weather <- weather %>%
  mutate(
    heavy_rain_flag    = if_else(precip >= 2.5, 1L, 0L),
    freezing_risk_flag = if_else(temp <= 2, 1L, 0L)
  )

# Save cleaned collisions and weather datasets
readr::write_csv(collisions, "E:/6th attempt/cleaning/collisions_cleaned.csv")
readr::write_csv(weather, "E:/6th attempt/cleaning/weather_cleaned.csv")


# ----------------------------
# Merge cleaned datasets
# ----------------------------
collisions <- read.csv("E:/6th attempt/cleaning/collisions_cleaned.csv")
weather <- read.csv("E:/6th attempt/cleaning/weather_cleaned.csv")


# Standardize text formatting and datetime for merging
collisions_clean <- collisions %>%
  mutate(
    region = trimws(tolower(region)),
    collision_hour = trimws(as.character(collision_hour))
  )

weather_clean <- weather %>%
  mutate(
    region = trimws(tolower(region)),
    weather_hour = trimws(as.character(weather_hour))
  )

# Merge collisions with corresponding weather data
collisions_weather  <- collisions_clean %>%
  left_join(weather_clean, by = c("region" = "region", "collision_hour" = "weather_hour"))


# ------------------------------------------------------

# For POWERBI VISUALISATION Save merged dataset 

# ------------------------------------------------------

write.csv(collisions_weather, "E:/6th attempt/powerbi_dataset/Dataset_For_powerBI.csv", row.names = FALSE)



# Load merged dataset and check for missing values
collisions_weather <- read.csv("E:/6th attempt/powerbi_dataset/Dataset_For_powerBI.csv")

print(paste("Empty rows remaining:", sum(is.na(collisions_weather$temp))))
colSums(is.na(collisions_weather))

# Identify columns that are all zero or all NA
zero_cols <- names(collisions_weather)[colSums(collisions_weather != 0, na.rm = TRUE) == 0]
empty_cols <- names(collisions_weather)[colSums(!is.na(collisions_weather)) == 0]
print(zero_cols)
print(empty_cols)

# ----------------------------
# Build a full borough-hour panel
# ----------------------------
# Standardize region names for consistency
collisions2 <- collisions %>%
  mutate(region = str_to_lower(str_trim(region)))

weather2 <- weather %>%
  mutate(region = str_to_lower(str_trim(region)))

# Aggregate collisions by borough and hour
collisions_by_borough_hour <- collisions_weather %>%
  mutate(region = str_to_lower(str_trim(region))) %>%
  group_by(region, borough_name, collision_hour) %>%
  summarise(
    collision_count = n(),
    ksi_count       = sum(collision_severity %in% c(1, 2), na.rm = TRUE),
    .groups = "drop"
  )

# Create a grid of all hours and boroughs for each region
hours_by_region <- weather2 %>%
  distinct(region, weather_hour) %>%
  rename(collision_hour = weather_hour)

boroughs_by_region <- collisions2 %>%
  distinct(region, borough_name)

borough_hour_grid <- boroughs_by_region %>%
  inner_join(hours_by_region, by = "region", relationship = "many-to-many")

# Merge collision counts with full borough-hour grid
borough_hour <- borough_hour_grid %>%
  left_join(collisions_by_borough_hour,
            by = c("region", "borough_name", "collision_hour")) %>%
  mutate(
    collision_count = replace_na(collision_count, 0L),
    ksi_count       = replace_na(ksi_count, 0L),
    collision_flag  = if_else(collision_count > 0, 1L, 0L)
  )

# ----------------------------
# Handle missing visibility values based on conditions
# ----------------------------


# Merge additional weather features back
borough_hour <- borough_hour %>%
  left_join(
    weather2 %>%
      select(region, weather_hour,
             temp, humidity, precip, snow, cloudcover, visibility,
             icon, conditions,
             heavy_rain_flag, freezing_risk_flag,
             weather_date, weather_year, weather_month, weather_dow, weather_hod) %>%
      rename(collision_hour = weather_hour),
    by = c("region", "collision_hour")
  )

borough_hour <- borough_hour %>%
  mutate(
    visibility = case_when(
      is.na(visibility) & conditions == "Clear" ~ 38.7,
      is.na(visibility) & temp <= 2 ~ 2.0,
      is.na(visibility) ~ median(visibility, na.rm = TRUE),
      TRUE ~ visibility
    )
  )
# Check distribution of hazard flags
table(borough_hour$heavy_rain_flag)
table(borough_hour$freezing_risk_flag)

# -------------------------------------------------------------------

# SAVE CLEANED BOROUGH-HOUR ADDED DATASET ADDEDED FOR VERIFICATION
  
# -------------------------------------------------------------------
write.csv(borough_hour, "E:/6th attempt/cleaning/borough_hour_cleaned.csv", row.names = FALSE)

# Quick summary to confirm
print("Dataset Saved Successfully!")
print(paste("Total Rows:", nrow(borough_hour)))
print(paste("Missing Visibility Values:", sum(is.na(borough_hour$visibility))))

# Ensure numeric types for weather features
borough_hour <- borough_hour %>%
  mutate(across(c(temp, humidity, precip, snow, cloudcover, visibility),
                as.numeric))

# Convert collision_hour to proper datetime format
borough_hour <- borough_hour %>%
  mutate(
    collision_hour = ymd_hms(
      str_replace_all(collision_hour, c("T" = " ", "Z" = "")),
      tz = "UTC"
    )
  )

# Confirm and inspect collision probabilities by hour
borough_hour %>%
  mutate(hour = hour(collision_hour)) %>%
  group_by(hour) %>%
  summarise(collision_probability = mean(collision_flag), .groups = "drop") %>%
  arrange(hour) %>%
  print(n = 24)







# ---------------------------------------------------
# Create weather_type ONCE (For multiple plots)
# ---------------------------------------------------
borough_hour_weather <- borough_hour %>%
  mutate(
    weather_type = case_when(
      freezing_risk_flag == 1 ~ "Freezing Risk",
      heavy_rain_flag == 1 ~ "Heavy Rain",
      TRUE ~ "Clear/Other"
    )
  )

# ---------------------------------------------------
# Plot 1: Collision probability by weather type
# ---------------------------------------------------
borough_hour_weather %>%
  group_by(weather_type) %>%
  summarise(
    collision_probability = mean(collision_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(weather_type, collision_probability)) +
  geom_col() +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Collision Probability by Weather Condition (Borough Hour)",
    x = "Weather Condition",
    y = "Probability of ≥1 collision"
  )

# ---------------------------------------------------
# Plot 2: Hourly collision probability
# ---------------------------------------------------
borough_hour %>%
  mutate(hour = hour(collision_hour)) %>%
  group_by(hour) %>%
  summarise(
    collision_probability = mean(collision_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  ggplot(aes(hour, collision_probability)) +
  geom_line() +
  scale_x_continuous(breaks = 0:23) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Hourly Collision Probability Pattern",
    x = "Hour of Day",
    y = "Probability of ≥1 collision"
  )

# ---------------------------------------------------
# Plot 3 : Collision probability by weather & region
# ---------------------------------------------------
weather_region_summary <- borough_hour_weather %>%
  group_by(region, weather_type) %>%
  summarise(
    collision_probability = mean(collision_flag, na.rm = TRUE),
    .groups = "drop"
  )

weather_region_summary %>%
  ggplot(
    aes(
      x = weather_type,
      y = collision_probability,
      colour = region,
      group = region
    )
  ) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Collision Probability by Weather Type and Region",
    x = "Weather Condition",
    y = "Probability of at least one collision",
    colour = "Region",
    caption = "Unit of analysis: borough-hour"
  )

# ---------------------------------------------------
# Save model-ready data
# ---------------------------------------------------
readr::write_csv(
  borough_hour,
  "E:/6th attempt/FINAL DATASET MODELLING/FINAL_DATASET_FOR_TRAINING.csv"
)

# ---------------------------------------------------
# Reload & NA check for visibility
# ---------------------------------------------------
df_ready <- read.csv("E:/6th attempt/FINAL DATASET MODELLING/FINAL_DATASET_FOR_TRAINING.csv")

na_count <- sum(is.na(df_ready$visibility))

print(paste("Total NA visibility hours:", na_count))





