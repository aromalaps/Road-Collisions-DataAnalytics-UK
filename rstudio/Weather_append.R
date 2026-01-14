library(tidyverse)

# -----------------------------
# File paths
# -----------------------------
london_file <- "E:/weather/london_weather.csv"
manchester_file <- "E:/weather/manchester_weather.csv"

# -----------------------------
# Read datasets
# -----------------------------
weather_london <- read_csv(london_file, show_col_types = FALSE)
weather_manchester <- read_csv(manchester_file, show_col_types = FALSE)

# -----------------------------
# Rename column: name -> Region
# -----------------------------
weather_london <- weather_london %>%
  rename(Region = name)

weather_manchester <- weather_manchester %>%
  rename(Region = name)

# -----------------------------
# Optional but recommended: enforce identical column order
# -----------------------------
weather_manchester <- weather_manchester %>%
  select(colnames(weather_london))

# -----------------------------
# Append datasets
# -----------------------------
weather_combined <- bind_rows(weather_london, weather_manchester)

# -----------------------------
# Save combined dataset
# -----------------------------
write_csv(
  weather_combined,
  "E:/weather/weather_london_manchester_combined.csv"
)

# -----------------------------
# Quick validation
# -----------------------------
glimpse(weather_combined)
count(weather_combined, Region)

