#######################################################################
## FoRTE 2019 Canopy reflectance data extraction & compilation
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


data_dir <- "./data_reflectance_2019/"


# Get a (fresh) list of the downloaded data we're working with
files <- list.files(data_dir, pattern = "*_Calculations.csv", full.names = TRUE)


require(data.table)
# this way imports the filenames
files <- list.files(path = "./data_reflectance_2019/", pattern = "*_Calculations.csv", full.names = TRUE)
#read the files from the list
l <- lapply( files, fread )
#names the list using the basename from `l`
# this also is the step to manipuly the filesnamaes to whatever you like
names(l) <- basename( files )
#bind the rows from the list togetgher, putting the filenames into the colum "id"
dt <- rbindlist( l, idcol = "filename" )
x <- dt

# Combine data into a single data frame for analysis
x %>%
  bind_rows %>%
  as_tibble %>%
  separate(filename, into = c("Focal_Tree", "Leaf_Sample", "Filename_date", "CID_Gibberish"),
           remove = FALSE) %>%
  data.frame() ->   refdata
# 




### Calculations and Indices from CID
require(data.table)
# this way imports the filenames
files <- list.files(path = "./data_reflectance_2019/", pattern = "*_Calculations.csv", full.names = TRUE)
#custom read
read_data <- function(z){
  data <- fread(z, skip = 32)
}
#read the files from the list
l <- lapply( files, read_data)
#names the list using the basena
#names the list using the basename from `l`
# this also is the step to manipuly the filesnamaes to whatever you like
names(l) <- basename( files )
#bind the rows from the list togetgher, putting the filenames into the colum "id"
dt <- rbindlist( l, idcol = "filename" )
z <- dt

# Combine data into a single data frame for analysis
z %>%
  bind_rows %>%
  as_tibble %>%
  separate(filename, into = c("Focal_Tree", "Leaf_Sample", "Filename_date", "CID_Gibberish"),
           remove = FALSE) %>%
  data.frame() ->   indices

# filter
indices %>%
  select(-filename, -Layer, -CID_Gibberish) %>%
  data.frame() -> indices

# replace all nonalphanumeric characters
indices$Value <- gsub("[^0-9.-]", NA, indices$Value)


#preparing NDVI data set
indices$Value <- as.numeric(indices$Value)

ndvi <- indices %>%
  filter(Calculation == "NDVI") %>%
  mutate(Date = ymd(Filename_date))

#add column for first reading, second reading in summer 2019
ndvi$Sample_Order <- 
  ifelse(ndvi$Date < "2019-07-05",
         "Early Summer",
         ifelse(ndvi$Date >= "2019-07-30",
                "Late Summer",
                "Other"
         ))


# getting ready to graph late season NDVI
ndvi <- ndvi %>%
  select(Focal_Tree, Calculation, Value, Date, Sample_Order) %>%
  filter(Sample_Order == "Late Summer") %>%
  group_by(Focal_Tree) %>%
  summarize(mean_NDVI = mean(Value, na.rm = TRUE))

write.csv(ndvi, "ndvi_tree_means_2019.csv", row.names = FALSE)

ndvi_g <- ndvi %>%
  ggplot(aes(x = Focal_Tree, y = mean_NDVI)) +
  geom_point()


