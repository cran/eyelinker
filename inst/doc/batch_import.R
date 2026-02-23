## ----results="hide", message=FALSE--------------------------------------------
# Import libraries required for the vignette
require(eyelinker)
require(dplyr)
require(tibble)
require(purrr)

## ----results="hide", message=FALSE--------------------------------------------
# Get full paths for all compressed .asc files in _Data/asc folder
ascs <- list.files(
  "./_Data/asc", pattern = "*.asc.gz",
  full.names = TRUE, recursive = TRUE
)

# Get paths of example files for batch import
ascs <- c(
    system.file("extdata/mono250.asc.gz", package = "eyelinker"),
    system.file("extdata/mono500.asc.gz", package = "eyelinker"),
    system.file("extdata/mono1000.asc.gz", package = "eyelinker")
)

## -----------------------------------------------------------------------------
# Batch import and merge saccade data for all files
sacc_dat <- map_df(ascs, function(f) {
  # Extract saccade data frame from file
  df <- read_asc(f, samples = FALSE)$sacc
  # Extract ID from file name and append to data as first column
  id <- gsub(".asc.gz", "", basename(f))
  df <- add_column(df, asc_id = id, .before = 1)
  # Return data frame
  df
})

# Batch import file metadata
asc_info <- map_df(ascs, function(f) {
  # Extract metadata data frame from file
  df <- read_asc(f, samples = FALSE)$info
  # Extract ID from file name and append to data as first column
  id <- gsub(".asc.gz", "", basename(f))
  df <- add_column(df, asc_id = id, .before = 1)
  # Return data frame
  df
})

## -----------------------------------------------------------------------------
sacc_dat

## -----------------------------------------------------------------------------
asc_info %>%
  select(c(asc_id, model, sample.rate, left, right, cr, screen.x, screen.y))

## -----------------------------------------------------------------------------
# Batch import full eye data (excluding raw samples) for all files
eyedat <- lapply(ascs, function(f) {
  # Since importing can be slow, print out progress message for each file
  cat(paste0("Importing ", basename(f), "...\n"))
  # Actually import the data
  read_asc(f, samples = FALSE)
})

# Extract names of files (excluding suffix) and use them as participant IDs
asc_ids <- gsub(".asc.gz", "", basename(ascs))
names(eyedat) <- asc_ids

# Parse fixation data from list
fix_dat <- map_df(asc_ids, function(id) {
  # Grab fixation data from each file in the list & append ID
  eyedat[[id]]$fix %>%
    add_column(asc_id = id, .before = 1)
})

# Parse blink data from list
sacc_dat <- map_df(asc_ids, function(id) {
  # Grab saccade data from each file in the list & append ID
  eyedat[[id]]$sacc %>%
    add_column(asc_id = id, .before = 1)
})

## -----------------------------------------------------------------------------
cache_path <- "./eyedata_cache.Rds"

if (file.exists(cache_path)) {
  # If cached eye data already exists, load that to save time
  eyedat <- readRDS(cache_path)

} else {
  # Otherwise, import all raw .asc files and cache them
  # [Insert import code that generates eyedat here]

  # Save the imported data for next run
  saveRDS(eyedat, file = cache_path)
}

