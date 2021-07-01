#' Read in image csv
#'
#' @description
#' processCSV will read in a csv file of SST and return the matrices needed for various ML tasks.
#'
#'
#' @details
#' `stats::kmeans()` wants a matrix where each row is a sample of your data. So we want each
#' row of our data matrix to be a date and the columns will be the pixels in the image.
#'
#' @param file Name of the csv file
#' @param aspect_ratio c(width, height). This is `c(length(unique(lons)), length(unique(lats)))`
#' @param lat_range What range to subset
#' @param long_range What range to subset
#' @param has.alt If TRUE, then remove 2nd column
#'
#' @return The function returns `dat.wide` which is the original data (in the specified lat/lon box)
#' where each row is a date and each column is a pixel in the image grid.
#' `dat.wide` may have NAs (say from land if working with ocean
#' data). `dat.clean` is the data with NA pixels (i.e. land) removed.
#' `pos.loc` is the location of the
#' non-NA pixels (columns in `dat.wide`) so that the image can be reconstructed after
#' k clustering is performed.
#'
#' @export
processCSV <- function(file, lat_range, long_range, has.alt = FALSE) {

  # read the file and drop first row,
  # which contains miscellaneous info
  dat_raw <- read_csv(file) %>% 
    slice(2:n())
  
  # If has_alt is TRUE, drop second column (latitude)
  if (has_alt) {
    dat_raw <- dat_raw %>% 
      select(-2)
  } 
  
  # Process dataset
  dat_processed <- dat_raw %>% 
    # Set column names
    rename(date = time,
           lat = latitude,
           lon = longitude) %>% 
    # Convert date column to Date type
    mutate(date = as.Date(date)) %>% 
    # Filter out unwanted latitudes, longitudes
    filter(between(lat, lat_range[1], lat_range[2]),
           between(lon, long_range[1], long_range[2]))
  
  # Check if same n for each date
  n.by.date <- tapply(dat_processed$sst, dat_processed$date, function(x){sum(is.na(x))})
  if(any((n.by.date-n.by.date[1])!=0)) {
    stop("There's a problem. Should be same n for each date.")
  } 
  
  # Pivot Wider
  dat_wide <- dat_processed %>% 
    pivot_wider(names_from = date, values_from = sst)
  
  # which row are non-NA?
  pos_loc <- which(!is.na(dat_wide[,3]))
  
  # Remove NA
  dat_clean <- dat_wide %>% 
    # remove the rows that are NA
    na.omit() 
  
  # Note: We transpose since kmeans() wants variates in columns
  return(list(dat=t(dat_wide), dat_clean=t(dat_clean), pos_loc=pos_loc))
}
