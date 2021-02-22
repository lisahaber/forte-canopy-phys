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



### META DATA FOR CALCULATIONS
## had to manually load data into data_reflectance_2020 folder due to googledrive download malfunction
## only loaded .csv files, not the .pngs associated with each measurement
data_dir <- "./data_reflectance_2020/"


# Get a (fresh) list of the downloaded data we're working with
files <- list.files(data_dir, pattern = "*_Calculations.csv", full.names = TRUE)


require(data.table)
# this way imports the filenames
files <- list.files(path = "./data_reflectance_2020/", pattern = "*_Calculations.csv", full.names = TRUE)
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
files <- list.files(path = "./data_reflectance_2020/", pattern = "*_Calculations.csv", full.names = TRUE)
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
  as.data.frame() ->   indices

# filter
indices %>%
  dplyr::select(-filename, -Layer, -CID_Gibberish) %>%
  data.frame() -> indices

# replace all nonalphanumeric characters
indices$Value <- gsub("[^0-9.-]", NA, indices$Value)

# graphing raw data for NDVI
indices$Value <- as.numeric(indices$Value)

ndvi_2020 <- indices %>%
  filter(Calculation == "NDVI") %>%
  dplyr::select(Focal_Tree, Calculation, Value) %>%
  group_by(Focal_Tree) %>%
  summarize(mean_NDVI = mean(Value, na.rm = TRUE))

write.csv(ndvi_2020, "ndvi_tree_means_2020.csv", row.names = FALSE)


##### graphing 2019 and 2020 ndvi together for exploration
ndvi_1 <- read.csv("ndvi_tree_means_2019.csv")
ndvi_2 <- read.csv("ndvi_tree_means_2020.csv")
NDVI_combined <- bind_rows(ndvi_1, ndvi_2)

NDVI_combined$Year <- as.factor(NDVI_combined$Year)

ndvi_g <- NDVI_combined %>%
  ggplot(aes(x = Focal_Tree, y = mean_NDVI)) +
  geom_point(aes(color = Year))



#### RAW WAVELENGTH DATA
# this way imports the filenames
files <- grep(list.files(path = "./data_reflectance_2020/",, full.names = TRUE), pattern = "*_Calculations.csv", invert = TRUE, value = TRUE)

# custom read function
read_data <- function(z){
  data <- fread(z, skip = 6)
}
#read the files from the list
l <- lapply( files, read_data)
#names the list using the basename from `l`
# this also is the step to manipuly the filesnamaes to whatever you like
names(l) <- basename( files )
#bind the rows from the list togetgher, putting the filenames into the colum "id"
dt <- rbindlist( l, idcol = "filename" )
y <- dt

# seperate filename
y %>%
  bind_rows %>%
  as_tibble %>%
  separate(filename, into = c("Focal_Tree", "Leaf_Sample", "Filename_date", "CID_Gibberish"),
           remove = FALSE) %>%
  data.frame() ->   specdata


specdata %>%
  select(-filename, -CID_Gibberish) %>%
  data.frame() -> specdata

refdata %>%
  select(-filename, CID_Gibberish) %>%
  data.frame() -> refdata

#df <- left_join(specdata, refdata)


#####################################
## Looking at PRI 

pri_2020 <- indices %>%
  filter(Calculation == "PRI") %>%
  dplyr::select(Focal_Tree, Calculation, Value) %>%
  group_by(Focal_Tree) %>%
  summarize(mean_PRI = mean(Value, na.rm = TRUE))

pri_g <- pri_2020 %>%
  ggplot(aes(x = Focal_Tree, y = mean_PRI)) +
  geom_point()







