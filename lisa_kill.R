# loading the required packages
library(ggplot2)
require(plyr)
require(dplyr)
require(tidyverse)
require(ggforce)
require(splitstackshape)
require(data.table)
library(forcats)
require(ggridges)

# The palette with black:
cbbPalette <-c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", "#66a61e", "#e6ab02", "#a6761d", "#666666")


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

#remove empty lines from haglof
inventory <- na.omit(inventory, cols = "Tag")

#adding subplot
source("./code/addNewData.r")
allowedVars <- c("PlotID")

#add subplot
df <- addNewData("./data/inventory_lookup_table.csv", inventory, allowedVars)


# cleaning up df
# names(df)[names(df) == "DBH_cm"] <- "dbh"
# df$dbh <- as.numeric(df$dbh)
# df$SubplotID <- as.factor(df$SubplotID)
# df$Species <- as.factor(df$Species)


# merge with spatial data
# using plot A01W
data_dir <- "./data/haglof/"

#import all the .csv files
spatial.data <- multmerge(data_dir)

# make a data frame
spatial.data <- as(spatial.data, "data.frame")

#rename columns
names(spatial.data)[1] <- "Subplot"
names(spatial.data)[2] <- "Plot_Radius"
names(spatial.data)[3] <- "Tag"
names(spatial.data)[4] <- "Tree_Spc"
names(spatial.data)[5] <- "Tree_Dia"
names(spatial.data)[6] <- "Tree_Hgt"
names(spatial.data)[7] <- "Tree_PosTex1"
names(spatial.data)[8] <- "Tree_PosTex2"
names(spatial.data)[9] <- "Tree_PosTex3"
names(spatial.data)[10] <- "Tree_Local_x"
names(spatial.data)[11] <- "Tree_Local_y"
names(spatial.data)[12] <- "Tree_Local_Dist"
names(spatial.data)[13] <- "Tree_Local_Angle"
names(spatial.data)[14] <- "Tree_Angle_ToPlotCenter"
names(spatial.data)[15] <- "Latitude"
names(spatial.data)[16] <- "Longitude"
names(spatial.data)[17] <- "Tree_Nr"


spatial.data %>%
  select("Subplot", "Tag", "Tree_Dia", "Latitude", "Longitude") %>%
  mutate(dbh = Tree_Dia*0.1) -> jim

#merging
stem <-  merge(df, jim, all.x = TRUE)

# cleaning up missing data
# stem$Species[stem$Species == "FAGR#"] <- "FAGR"
# stem$Species[stem$Species == "POGR ?"] <- "POGR"
# stem$Species[stem$Species == "TSCA ?"] <- "TSCA"
# stem$Species[stem$Species == "?"] <- "unknown"
# stem$Species[stem$Species == "UNKNOWN"] <- "unknown"
# stem$Species[stem$Species == "QUR"] <- "QURU"
# stem$Species[stem$Species == "ADRU"] <- "ACRU"
#
stem <- subset(stem, Species != "SNAG")
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
stem$sla[stem$genus == "Pinus"] <- 8.12
stem$sla[stem$genus == "Quercus"] <- 14.2
stem$sla[stem$genus == "Other"] <- 19
stem$sla[stem$genus == "Tsuga"] <- 5.84
stem$sla[stem$genus == "Fagus"] <- 35
stem$sla[stem$genus == "Populus"] <- 15.89

stem$lai <- stem$leaf.mass * stem$sla

## D01


###
# group plot disturbance
#      D    1           0
#      D    2          85
#      D    3          45
#      D    4          65

####################################
message("D01, 0%, Control")

stem %>%
  filter(PlotID == "D01") %>%
  arrange(lai) -> df

# they all live
df$fate <- "live"

#loook at output
table(df$fate)

ggplot(data = df, aes(x = Longitude, y = Latitude, size = (dbh), color = genus, shape = fate)) +
  geom_point(alpha = 1)+
  scale_colour_manual(values=cbbPalette, limits = levels(stem$genus))+
  scale_shape_manual(values=c(19))+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  ggtitle("D01 - Control")+
  theme_classic()

```

## D02

```{r D02, echo=FALSE,  warning = FALSE}
# Assigned Disturbance Level Per Plot - Group D
#  group plot disturbance
#      D    1           0
#      D    2          85
#      D    3          45
#      D    4          65

#####################################
message("D02, 85%")
stem %>%
  filter(PlotID == "D02" & Health_status != "D" & lai != "NA") -> df

sum.lai <- sum(df$lai)


message("Plot LAI")
plot.lai <- sum.lai/ 1000
print(plot.lai)

#modified lai
target.lai <- 0.85 * sum.lai

df %>% 
  filter(Health_status == "G") %>%
  summarise(sum(lai)) -> girdled.lai

df %>% 
  filter(Health_status == "L") %>%
  summarise(sum(lai)) -> live.lai

adj.target.lai <- target.lai - girdled.lai

# looping in
x <- 0

# subsetting
df %>% 
  filter(Health_status == "L") %>%
  arrange(dbh) -> live.df

x <- 0  
for (i in 1:nrow(live.df)) {
  x <- x + live.df$lai[i]
  
  if(x < (adj.target.lai)){
    live.df$fate[i] <- "kill"}
  else {
    live.df$fate[i] <- "live"
  }
  
}

df %>% 
  filter(Health_status == "G") -> gird.df

gird.df$fate <- "kill"

combo.df <- rbind(live.df, gird.df)
#look at output
table(combo.df$fate)



x11()
ggplot(data = combo.df, aes(x = Longitude, y = Latitude, size = (dbh), color = genus, shape = fate)) +
  geom_point(alpha = 1)+
  scale_colour_manual(values=cbbPalette, limits = levels(stem$genus))+
  scale_shape_manual(values=c(1, 19))+
  # geom_text(aes(label=Nr),hjust=0, vjust=0)+
  # guides(fill=FALSE, alpha=FALSE, size=FALSE)+
  ggtitle("D02 - 85%")+
  theme_classic()
