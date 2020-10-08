#######################################################################
## FoRTE 2020 Canopy reflectance data extraction & compilation
## Fall 2020
## Lisa Haber
## Trying to use same code previously used for compiling LICOR files
#######################################################################


#1. Extract and utilize canopy reflectance (CID) files from the shared FoRTE data drive.

# load required packages
library(dplyr)
library(readr)
library(googledrive)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidyverse)
library(naniar)

## had to manually load data into data_reflectance_2020 folder due to googledrive download malfunction
## only loaded .csv files, not the .pngs associated with each measurement

# Get a (fresh) list of the downloaded data we're working with
files <- list.files(data_dir, pattern = "Calculations.csv", full.names = TRUE)

DATA_PATTERN <- "Layer, Calculation, Value"

nalist = c("âˆž")

# trying the for-loop
filedata <- list()
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  # text_raw <- readLines(f, skipNul = TRUE)
  # data_start <- grep(DATA_PATTERN, text_raw)
  
  read.csv(f, header = TRUE, sep = ",", quote = "\"", as.is = TRUE, skip = 32) %>%
   na_if("âˆž") %>%
  mutate(Filename = basename(f)) ->
    filedata[[f]]
}

head(filedata)
  
# Combine data into a single data frame for analysis
filedata %>% 
  bind_rows %>% 
  as_tibble %>% 
  separate(Filename, into = c("Focal_Tree", "Leaf_Sample", "Filename_date", "CID_Gibberish"), 
           remove = FALSE) ->
  refdata  





















HEADER_PATTERN <- "Title, Color, Formula, Category, Notes"
DATA_PATTERN <- "Layer, Calculation, Value"

# Scan through all the data files and read data into list structure
filedata <- list()
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  text_raw <- readLines(f, skipNul = TRUE)
  data_start <- grep(DATA_PATTERN, text_raw)
  
  if(length(data_start)) {
    data_raw <- text_raw[data_start+1:length(text_raw)] %>% na.omit
    line_lengths <- lapply(strsplit(data_raw, "\t"), length) %>% unlist
    data_rows <- line_lengths == line_lengths[1] %>%
     
    
    # OK, now read the data into a data frame and add the 'comments'
    con <- textConnection(data_raw[data_rows])
    read.table(con, header = TRUE, stringsAsFactors = FALSE) %>% 
      mutate(Filename = basename(f)) ->
      filedata[[f]]
    close(con)
  }
}

# Combine data into a single data frame for analysis
filedata %>% 
  bind_rows %>% 
  as_tibble %>% 
  mutate(Timestamp = mdy_hms(Timestamp)) %>%  # change to a POSIXct object
  separate(Filename, into = c("Focal_Tree", "Leaf_Sample", "Filename_date"), remove = FALSE) ->
  licordata

