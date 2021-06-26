library(tidyverse)
library(lubridate)
## Latitude (42.625, 52.125) and Longitude (229.875, 236.625)

df_raw <- read_csv("https://coastwatch.pfeg.noaa.gov/erddap/griddap/ncdcOisst21Agg.csv?sst%5B(2010-01-01T12:00:00Z):(2021-06-10T12:00:00Z)%5D%5B(0.0)%5D%5B(42.625):(52.125)%5D%5B(229.875):(236.625)%5D&.draw=surface&.vars=longitude%7Clatitude%7Csst&.colorBar=%7C%7C%7C2%7C20%7C&.bgColor=0xffccccff") %>% 
  # Get rid of miscellaneous zlev in first row
  slice(-1) %>% 
  # zlev is a column of zeroes, so get rid of that
  dplyr::select(-zlev) %>% 
  # Convert into date
  mutate(time = ymd_hms(time),
         # Extract out day so I can just filter for first day in each month
         day = day(time)) %>% 
  # filter for first day in each month
  filter(day == 1) %>% 
  dplyr::select(-day) %>% 
  # Set column names
  rename(date = time,
         lat = latitude,
         lon = longitude) %>% 
  # Convert date column to Date type
  mutate(date = as.Date(date),
         lat = as.numeric(lat),
         lon = as.numeric(lon),
         sst = as.numeric(sst))


# mask out Puget Sound, Strait of Juan de Fuca and Georgia Strait
masks <- list(c(235.4488, 236.884, 47.87651, 50.13138),
              c(232.2913, 233.8987, 50.28689, 51.60871),
              c(234.4154, 235.9654, 49.04283, 50.09251))

for(m1 in masks) {
  # index of Puget Sound, Strait of Juan de Fuca or Georgia Strait
  mask_loc <- df_raw$lat <= m1[4] & df_raw$lat >= m1[3] &
    df_raw$lon <= m1[2] & df_raw$lon >= m1[1]
  # Change to NA
  df_raw$sst[mask_loc] <- NA
}

write_csv(df_raw, "data-raw/df_raw.csv")
usethis::use_data(df_raw, overwrite = TRUE)
