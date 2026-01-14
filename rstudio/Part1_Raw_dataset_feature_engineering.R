# Load required packages
library(dplyr)      # For data manipulation
library(lubridate)  # For date and time parsing
library(readr)      # For reading CSV files

# Load dataset
data <- read_csv("E:\\fifth_attempt_5\\downloaded dataset fromstat19\\data_1.csv")

# Create a proper datetime column combining date and time
data <- data %>%
  mutate(
    date = dmy(date),  # Convert date column from "dd/mm/yyyy" to Date object
    datetime = as.POSIXct(paste(date, time), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")  # Combine date and time
  )

# Filter dataset for the required time range: 1 Jan 2010 to 31 Dec 2024
data_2010_2024 <- data %>%
  filter(datetime >= as.POSIXct("2010-01-01 00:00:00", tz = "UTC"),
         datetime <= as.POSIXct("2024-12-31 23:59:59", tz = "UTC"))

# Filter raw data for the year (2024) and get dimensions
current_year_data <- data %>% filter(collision_year == 2024)
dim(current_year_data)   # Rows and columns
nrow(current_year_data)  # Number of rows
ncol(current_year_data)  # Number of columns

# Define ONS codes for Greater London boroughs
greater_london_codes <- c(
  "E09000001","E09000002","E09000003","E09000004","E09000005","E09000006",
  "E09000007","E09000008","E09000009","E09000010","E09000011","E09000012",
  "E09000013","E09000014","E09000015","E09000016","E09000017","E09000018",
  "E09000019","E09000020","E09000021","E09000022","E09000023","E09000024",
  "E09000025","E09000026","E09000027","E09000028","E09000029","E09000030",
  "E09000031","E09000032","E09000033"
)

# Define ONS codes for Greater Manchester boroughs
greater_manchester_codes <- c(
  "E08000001","E08000002","E08000003","E08000004","E08000005",
  "E08000006","E08000007","E08000008","E08000009","E08000010"
)

# Combine London and Manchester codes for filtering
target_codes <- c(greater_london_codes, greater_manchester_codes)

# Filter dataset to include only Greater London and Greater Manchester boroughs
london_manchester_data <- data_2010_2024 %>%
  filter(local_authority_ons_district %in% target_codes)

# Count total rows after filtering
nrow(london_manchester_data)

# Create a new column 'region' based on local_authority_ons_district
london_manchester_data <- london_manchester_data %>%
  mutate(
    region = case_when(
      local_authority_ons_district %in% greater_london_codes ~ "Greater London",
      local_authority_ons_district %in% greater_manchester_codes ~ "Greater Manchester",
      TRUE ~ NA_character_
    )
  )

# Create a lookup table for borough names based on ONS codes
borough_lookup <- tibble(
  local_authority_ons_district = c(
    "E09000001","E09000002","E09000003","E09000004","E09000005","E09000006",
    "E09000007","E09000008","E09000009","E09000010","E09000011","E09000012",
    "E09000013","E09000014","E09000015","E09000016","E09000017","E09000018",
    "E09000019","E09000020","E09000021","E09000022","E09000023","E09000024",
    "E09000025","E09000026","E09000027","E09000028","E09000029","E09000030",
    "E09000031","E09000032","E09000033",
    "E08000001","E08000002","E08000003","E08000004","E08000005",
    "E08000006","E08000007","E08000008","E08000009","E08000010"
  ),
  borough_names = c(
    "City of London","Barking and Dagenham","Barnet","Bexley","Brent","Bromley",
    "Camden","Croydon","Ealing","Enfield","Greenwich","Hackney",
    "Hammersmith and Fulham","Haringey","Harrow","Havering","Hillingdon","Hounslow",
    "Islington","Kensington and Chelsea","Kingston upon Thames","Lambeth","Lewisham","Merton",
    "Newham","Redbridge","Richmond upon Thames","Southwark","Sutton","Tower Hamlets",
    "Waltham Forest","Wandsworth","Westminster",
    "Bolton","Bury","Manchester","Oldham","Rochdale",
    "Salford","Stockport","Tameside","Trafford","Wigan"
  )
)

# Join borough names into the filtered dataset
london_manchester_data <- london_manchester_data %>%
  left_join(borough_lookup, by = "local_authority_ons_district")

# Check total rows and columns after join
dim(london_manchester_data)

# Count number of accidents by region
london_manchester_data %>% count(region)

# Count number of accidents by region and borough
london_manchester_data %>% count(region, borough_names, sort = TRUE)

# Drop the datetime column as it's no longer needed
london_manchester_data <- london_manchester_data %>%
  select(-datetime)

# Create a mapping vector to fix local_authority_district codes using borough names
mapping <- c(
  "Barking and Dagenham" = 17, "Barnet" = 28, "Bexley" = 18, "Bolton" = 100, "Brent" = 12,
  "Bromley" = 20, "Bury" = 101, "Camden" = 3, "City of London" = 570, "Croydon" = 19,
  "Ealing" = 31, "Enfield" = 11, "Greenwich" = 7, "Hackney" = 4, "Haringey" = 2,
  "Harrow" = 30, "Havering" = 15, "Hillingdon" = 26, "Hounslow" = 24, "Islington" = 2,
  "Lambeth" = 10, "Lewisham" = 6, "Manchester" = 102, "Merton" = 22, "Newham" = 5,
  "Oldham" = 104, "Redbridge" = 13, "Rochdale" = 106, "Salford" = 107, "Southwark" = 9,
  "Stockport" = 109, "Sutton" = 21, "Tameside" = 110, "Tower Hamlets" = 4, 
  "Trafford" = 112, "Waltham Forest" = 13, "Wandsworth" = 9, "Westminster" = 28, "Wigan" = 114,
  "Hammersmith and Fulham" = 27, "Kensington and Chelsea" = 1, 
  "Kingston upon Thames" = 23, "Richmond upon Thames" = 25
)

# Replace -1 with NA and fill missing local_authority_district using mapping
london_manchester_data <- london_manchester_data %>%
  mutate(
    local_authority_district = na_if(local_authority_district, -1),
    local_authority_district = ifelse(
      is.na(local_authority_district),
      mapping[borough_names],
      local_authority_district
    )
  )

# Save the cleaned and mapped dataset
write_csv(london_manchester_data, 
          "E:/fifth_attempt_5/Raw dataset/data_2_local_authority_fixed.csv")
