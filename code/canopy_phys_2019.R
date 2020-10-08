#############################################
######################################################################
## FoRTE 2019 Canopy Physiology data extraction & compilation
## Fall 2019
## Lisa Haber
## Reusing/modifying code from 2018
#######################################################################

## This year, our LICOR file names for canopy focal trees are different (changed to save time in the field). New format (example): 1EQ1_01_20190701: 4-digit tree/crown ID, 2-digit leaf sample ID, YYYYMMDD

#1. Extract and utilize canopy physiology (LICOR 6400XT) files from the shared FoRTE data drive.

# load required packages
library(dplyr)
library(readr)
library(googledrive)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidyverse)

# Direct Google Drive link to "FoRTE/data/canopy_leaf_physiology/2019"
# as_id("https://drive.google.com/drive/u/0/folders/17yVbBxtsmrIvbAEFB8PAZauBpU_IkZXw") %>% 
#   drive_ls ->
#   gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data2019/"
if(!dir.exists(data_dir)) dir.create(data_dir)

# # Download data
# for(f in seq_len(nrow(gdfiles))) {
#   cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
#   drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
# }

# Get a (fresh) list of the downloaded data we're working with
# Filenames we want end with four digits and no file extension
files <- list.files(data_dir, pattern = "[0-9]{8}$", full.names = TRUE)
HEADER_PATTERN <- "\"OPEN \\d\\.\\d\\.\\d"
DATA_PATTERN <- "\\$STARTOFDATA\\$"


# Scan through all the data files and read data into list structure
filedata <- list()
for(f in files) {
  cat(" Reading ", f, "...\n", sep = "")
  text_raw <- readLines(f, skipNul = TRUE)
  data_start <- grep(DATA_PATTERN, text_raw)
  first_comment <- text_raw[data_start - 1] # there's always a comment on this line
  
  if(length(data_start)) {
    # What makes this tricky is that there can be additional comments WITHIN the data frame
    # Who on earth thought that was a good idea?!?
    data_raw <- text_raw[data_start+1:length(text_raw)] %>% na.omit
    line_lengths <- lapply(strsplit(data_raw, "\t"), length) %>% unlist
    data_rows <- line_lengths == line_lengths[1]
    comments <- paste(which(!data_rows), data_raw[!data_rows], sep = ". ") %>%
      paste(first_comment, ., sep = "; ") %>%
      gsub('\"', "", .)
    
    # OK, now read the data into a data frame and add the 'comments'
    con <- textConnection(data_raw[data_rows])
    read.table(con, header = TRUE, stringsAsFactors = FALSE) %>% 
      mutate(Filename = basename(f),
             Timestamp = text_raw[grep(HEADER_PATTERN, text_raw) + 1],
             Comments = paste(comments, collapse = "; ")) ->
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

##############################
# get means of 5 observations per leaf to use in analysis

licordata_means_2019 <- licordata %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo)) %>%
  separate(Filename, into = c("Focal_Tree", "Leaf_Sample", "Filename_date"), remove = FALSE) %>%
  mutate(Date = ymd(Filename_date),
         Tree_Sample = paste(Focal_Tree, Leaf_Sample)) %>%
  filter(MeanPhoto >= 0)
licordata_means_2019  

all( licordata_means_2019$MeanPhoto >= 0) #check for presence of negative values

licordata_means_2019$Species <- 
# add species column
    ifelse(grepl("..A.", licordata_means_2019$Focal_Tree),
      "Acer",
     ifelse(grepl("..P.", licordata_means_2019$Focal_Tree),
      "Populus",
    ifelse(grepl("..Q.", licordata_means_2019$Focal_Tree),
      "Quercus", "Other"
    )))
head(licordata_means_2019)

# write.table(licordata_means_2019,"Mean_Photo_Cond_CANOPY_2019.txt",sep="\t",row.names=FALSE)  
# write.csv(licordata_means_2019, "Mean_Photo_Cond_CANOPY_2019.csv", row.names = FALSE)

#add year column
licordata_means_2019$Year <- 
  ifelse(grepl("2019....", licordata_means_2019$Filename_date),
         2019,
         "Other"
  )
head(licordata_means_2019)

#add treatment severity column
licordata_means_2019$Severity <- 
  # add species column
  ifelse(grepl("1...", licordata_means_2019$Focal_Tree),
         0,
         ifelse(grepl("2...", licordata_means_2019$Focal_Tree),
                85,
                ifelse(grepl("3...", licordata_means_2019$Focal_Tree),
                       45, 
                       ifelse(grepl("4...", licordata_means_2019$Focal_Tree),
                              65, "Other"
                ))))
head(licordata_means_2019)

#add column for first reading, second reading in summer 2019
licordata_means_2019$Sample_Order <- 
  # add species column
  ifelse(licordata_means_2019$Date < "2019-07-05",
         "Early Summer",
         ifelse(licordata_means_2019$Date >= "2019-07-30",
                "Late Summer",
                 "Other"
                       ))
head(licordata_means_2019)


#now, group leaf samples by species, treatment, and by sample order, and average
# licordata_means_2019 <- licordata_means_2019 %>%
#   group_by(Severity, Sample_Order) %>%
#   summarise(Mean_Asat = mean(MeanPhoto))
# head(licordata_means_2019)

# write.csv(licordata_means_2019, "Mean_Species_CANOPY_2019.csv", row.names = FALSE)

licordata_means_2019 <- licordata_means_2019 %>%
  filter(Sample_Order == "Late Summer")

head(licordata_means_2019)


## try to get 2018 and 2019 data in one df
# join by tree ID, "Focal_Tree" variable



licordata_means_2019 <- licordata_means_2019 %>%
  group_by(Focal_Tree, Species, Severity, Year) %>%
  summarize(mean_Asat = mean(MeanPhoto))

write.csv(licordata_means_2019, "Means_By_Crown_Asat_values_DRep_2019.csv", row.names = FALSE)


#########################################
# graph 2018 & 2019 data

data2018 <- read.csv("Means_By_Crown_Asat_values_DRep_2018.csv")
data2019 <- read.csv("Means_By_Crown_Asat_values_DRep_2019.csv")

all_data <- bind_rows(data2018, data2019)
head(all_data)

all_data$Severity <- as.factor(all_data$Severity)
all_data$Species <- as.factor(all_data$Species)

forte <- c("#000000", "#009E73", "#0072B2", "#D55E00")

Canopy_boxplots <- all_data %>%
  na.omit() %>%
  group_by(Severity) %>%
  ggplot(aes(x = Severity, y = mean_Asat, group_by(Severity), fill = Severity)) + 
  theme_classic(base_size = 13) + 
  geom_boxplot(show.legend = FALSE) +
  xlab("Severity (% LAI Lost)") +
  ylab(bquote('Mean photosynthetic rate ( '*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')) + 
  facet_grid(~Year) +
  scale_fill_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00"))

Canopy_boxplots

## by species
library(viridis)
Species_Canopy_boxplots <- data2019 %>%
  na.omit() %>%
  ggplot(aes(x = Species, y = mean_Asat, fill = Species)) + 
  theme_classic(base_size = 13) + 
  geom_boxplot(show.legend = FALSE) +
  xlab("Tree genus") +
  ylab(bquote('Mean photosynthetic rate ( '*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')) + 
  # facet_grid(~Year) +
  scale_fill_viridis_d()

Species_Canopy_boxplots


#### Model Run
## using code from Max Grigri; same model run by him and also by Kayla Mathes on their summer 2019 data sets
library(agricolae)
# clean up my all_data df for model run
all_data$Severity <- as.factor(all_data$Severity)
all_data$Species <- as.factor(all_data$Species)
# all_data$Replicate <- as.factor(all_data$Replicate)
all_data$Year <- as.factor(all_data$Year)
# recode 0 to 0.00 to help me when with the post-hoc output
all_data$Severity <- recode_factor(all_data$Severity, "0" = "0.00")
# model_run <- all_data %>%
#   ungroup() %>%
#   select(Year, Severity, Species, mean_Asat) %>%
#   filter(!is.nan(mean_Asat))
# overall anova split-split plot model run
CanAsatmodel <- aov(mean_Asat ~ Severity*Species*Year, data=all_data)
summary(CanAsatmodel)
# post hoc analsis
# creating an object for each part of the model
# gla <- Asatmodel$gl.a
# glb <- Asatmodel$gl.b
# glc <- Asatmodel$gl.c
# # doing the same as above for each error term
# Ea <- Asatmodel$Ea
# Eb <- Asatmodel$Eb
# Ec <- Asatmodel$Ec
# running an LSD post hoc test: Species
LSD_output <- LSD.test(CanAsatmodel, "Species", console = TRUE)
# # get post-hoc output into a df
# output_df <- data.frame(tibble::rownames_to_column(LSD_output$groups))
# # seperate into severity and date columns
# output_df <- transform(output_df, Severity = substr(rowname, 1, 4),
#                        date = substr(rowname, 6, 15))
# # sort by date and severity so that we can look which severities differ within a week
# output_df <- output_df %>%
#   arrange(date, severity)
# 



