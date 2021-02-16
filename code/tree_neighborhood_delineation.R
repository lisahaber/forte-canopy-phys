############################################################
## Delineation of Tree Neighborhoods for Canopy Tree Study
## L Haber
## December 2020
############################################################

# loading the required packages
library(ggplot2)
library(ggmap)
require(plyr)
require(dplyr)
require(tidyverse)
require(ggforce)
require(splitstackshape)
require(data.table)
library(forcats)
require(ggridges)
library(dismo)
library(sf)
library(rgeos)
library(sp)

# The palette with black:
cbbPalette <-c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6aC02", "#a6761d", "#666666")


#bring in inventory data

#set data directory
data_dir <- "./data/inventory/"

#merge a bunch of .csvs
multmerge = function(path){
  filenames=list.files(path=path, full.names=TRUE)
  rbindlist(lapply(filenames, fread))
}

#importing all the data
inventory <- multmerge(data_dir)

#convert to data frame
inventory <- as(inventory, "data.frame")
# inventory$Haglof_ID <- as.factor(inventory$Haglof_ID) #do I need to make Haglof_ID a factor, too?


#adding subplot
source("./code/addNewData.r")
allowedVars <- c("PlotID")

#add subplot
df <- addNewData("./data/inventory_lookup_table.csv", inventory, allowedVars)

# #remove empty lines from haglof
# 
# inventory <- na.omit(inventory, cols = "Tag")
# #
# df$group <- as.factor(substr(df$SubplotID, 0, 1))
# 
# #
# df$plotID <- as.factor(substr(df$SubplotID, 1, 3))

# cleaning up df
# names(df)[names(df) == "DBH_cm"] <- "dbh"
# df$dbh <- as.numeric(df$dbh)
df$PlotID <- as.factor(df$PlotID)
df$Species <- as.factor(df$Species)


# merge with spatial data
# could not get code from .Rmd for disturbance stem maps to work here, only in that file
# no idea why -- but wrote .csv from the .Rmd and importing here instead.

stem <- read.csv("spatial_tree_data_canopy_study.csv")
#
#bring in conversion to leaf area
allo.df <- read.csv("./data/dbh_to_leaf_area_conversions.csv")

allo.df %>%
  filter(component == "FL") -> allo.fl
stem <- merge(stem, allo.fl)

stem$leaf.mass <- stem$a * (stem$dbh^stem$b)

stem <- droplevels(stem)


attach(stem)
stem$genus[stem$Species == "ACPE"] <- "Acer"
stem$genus[stem$Species == "ACRU"] <- "Acer"
stem$genus[stem$Species == "ACSA"] <- "Acer"
stem$genus[stem$Species == "BEPA"] <- "Betula"
stem$genus[stem$Species == "PIRE"] <- "Pinus"
stem$genus[stem$Species == "PIST"] <- "Pinus"
stem$genus[stem$Species == "QURU"] <- "Quercus"
stem$genus[stem$Species == "AMEL"] <- "Other"
stem$genus[stem$Species == "TSCA"] <- "Tsuga"
stem$genus[stem$Species == "FAGR"] <- "Fagus"
stem$genus[stem$Species == "POGR"] <- "Populus"
stem$genus[stem$Species == "POTR"] <- "Populus"
stem$genus[stem$Species == "unknown"] <- "Other"

stem$genus <- as.factor(stem$genus)

attach(stem)
stem$sla[stem$genus == "Acer"] <- 19
stem$sla[stem$genus == "Betula"] <- 20.82
stem$sla[stem$Species == "PIRE"] <- 5.39 #penner and deblonde ref.
stem$sla[stem$Species == "PIST"] <- 12.5 #abrams & kubiske, 1990
stem$sla[stem$genus == "Quercus"] <- 14.2
stem$sla[stem$genus == "Other"] <- 19
stem$sla[stem$genus == "Tsuga"] <- 5.84
stem$sla[stem$genus == "Fagus"] <- 35
stem$sla[stem$genus == "Populus"] <- 15.89

stem$lai <- stem$leaf.mass * stem$sla

stem %>%
  filter(dbh >= 8) -> stem

####################################
###
# Assigned Disturbance Level Per Plot - Group D
#  group plot disturbance
#      D    1           0
#      D    2          85
#      D    3          45
#      D    4          65
#
# Group D Treatment Assignments
#  plot plot.side treatment
#    D1    bottom         E
#    D1       top         W
#    D2    bottom         W
#    D2       top         E
#    D3       top         W
#    D3    bottom         E
#    D4       top         W
#    D4    bottom         E
#
###
# ALL of the treatments for canopy tree neighborhoods are "top-down"
##################
message("D01, CONTROL")  
stem %>%
  filter(PlotID == "D01") %>%
  arrange(lai) -> df

# plot lai
sum.lai <- sum(df$lai)

# they all live
df$fate <- "live"

#look at output
table(df$fate)

ggplot(data = df, aes(x = Longitude, y = Latitude, size = (dbh), color = genus, shape = fate)) +
  geom_point(alpha = 1)+
  scale_colour_manual(values=cbbPalette, limits = levels(stem$genus))+
  scale_shape_manual(values=c(19))+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  ggtitle("D01 - Control")+
  theme_classic()

D01 <- df


# identify focal trees
unique(df$Tag)

df$Focal <- ifelse(grepl(".E..", df$Tag), 
                   "Focal",
                   ifelse(grepl(".W..", df$Tag),
                   "Focal", "Other"))

# map the df dataframe again to highlight focal trees
d01map <-  ggplot(data = df, aes(x = Longitude, y = Latitude, size = (dbh), color = Focal, shape = fate)) +
  geom_point(alpha = 1)+
  # scale_colour_manual(values=cbbPalette, limits = levels(stem$genus))+
  scale_shape_manual(values=c(19))+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  ggtitle("D01 - Control")+
  theme_classic()

############################
## adding focal tree neighborhoods
d <- df

# select long, lat for coords for next step
xy <- d[,c(13,12)]

# make a spatial points data frame
# add CRS information for Michigan State Plane Central (https://spatialreference.org/ref/esri/102289/proj4/)
d.spdf <- SpatialPointsDataFrame(coords = xy, data = d, proj4string = 
                                   CRS("+proj=lcc +lat_1=44.18333333333333 +lat_2=45.7 +lat_0=43.31666666666667 +lon_0=-84.36666666666666 +x_0=6000000 +y_0=0 +ellps=GRS80 +datum=WGS84 +units=m +no_defs"))
# now, convert to an sf object
st_as_sf(d.spdf)
st_crs(d.spdf)

#######################################################3

# WGS84 projection
# buffering -- need a subset of data which is just the focal trees in their own dataframe
# will use these to buffer and then use an intersection to grab trees in the buffered area


# Trying code that JT found: https://gis.stackexchange.com/questions/292327/creating-buffers-around-points-and-merging-with-spatialpolygonsdataframe-to-crea
# convert data frame to sf object

tree_sf <- st_as_sf(d.spdf)

# plot data to check coordinates still make sense
plot(x=tree_sf$Longitude, y=tree_sf$Latitude)

# transform from SPDF to sf object so you can use the buffering function st_buffer
# only want buffers around the focal trees, so first subset your data frame for just the focal trees
focal_tree_buff <- df %>%
  filter(Focal == "Focal") %>%
  dplyr::select(Tag, Latitude, Longitude, dbh)

# now, this subsetted data frame needs to become a sf object so you can generate the buffer layer

focal_tree_buff_sf <- st_as_sf(focal_tree_buff, coords = c("Longitude", "Latitude"))

# now assign metric coordinate system to the focal tree data
st_crs(focal_tree_buff_sf) = st_crs(d.spdf)

focal_tree_buff_m <- st_transform(focal_tree_buff_sf, "+proj=lcc +lat_1=44.18333333333333 +lat_2=45.7 +lat_0=43.31666666666667 +lon_0=-84.36666666666666 +x_0=6000000 +y_0=0 +datum=WGS84 +units=m +no_defs")

tree_buffer <- st_buffer(focal_tree_buff_m, 10)
trees_with_buff <- st_intersection(tree_buffer, tree_sf)

plot(tree_buffer)

###############################################33
message("D02, 85% Disturbance")  
stem %>%
  filter(PlotID == "D02") %>%
  arrange(lai) -> df

# plot lai
sum.lai <- sum(df$lai)

# identify focal trees
unique(df$Tag)

df$Focal <- ifelse(grepl(".E..", df$Tag), 
                   "Focal",
                   ifelse(grepl(".W..", df$Tag),
                          "Focal", "Other"))

# # they all live
# df$fate <- "live"
# 
# #look at output
# table(df$fate)

ggplot(data = df, aes(x = Longitude, y = Latitude, size = (dbh), color = Focal)) +
  geom_point(alpha = 1)+
  # scale_colour_manual(values=cbbPalette, limits = levels(stem$genus))+
  scale_shape_manual(values=c(19))+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  ggtitle("D02 - 85%")+
  theme_classic()


head(stem)
hist(stem$dbh)
