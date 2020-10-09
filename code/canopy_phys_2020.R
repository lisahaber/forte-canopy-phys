#######################################################################
## FoRTE 2020 Canopy Physiology data extraction & compilation
## Fall 2020
## Lisa Haber
## Reusing/modifying code from 2019
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

#Direct Google Drive link to "FoRTE/data/canopy_leaf_physiology/2020"
as_id("https://drive.google.com/drive/u/1/folders/1GpCageV4RBSqxuatyQUuYbsfTUFyUbXl") %>%
  drive_ls ->
  gdfiles


# Create a new data directory for files, if necessary
data_dir <- "data2020/"
if(!dir.exists(data_dir)) dir.create(data_dir)

# Download data
for(f in seq_len(nrow(gdfiles))) {
  cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
  drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
}

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

licordata_means_2020 <- licordata %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo)) %>%
  separate(Filename, into = c("Focal_Tree", "Leaf_Sample", "Filename_date"), remove = FALSE) %>%
  mutate(Date = ymd(Filename_date),
         Tree_Sample = paste(Focal_Tree, Leaf_Sample)) %>%
  filter(MeanPhoto >= 0)
licordata_means_2020  

all( licordata_means_2020$MeanPhoto >= 0) #check for presence of negative values

licordata_means_2020$Species <- 
  # add species column
  ifelse(grepl("..A.", licordata_means_2020$Focal_Tree),
         "Acer",
         ifelse(grepl("..P.", licordata_means_2020$Focal_Tree),
                "Populus",
                ifelse(grepl("..Q.", licordata_means_2020$Focal_Tree),
                       "Quercus", "Other"
                )))
head(licordata_means_2020)

#add year column
licordata_means_2020$Year <- 
  ifelse(grepl("2020....", licordata_means_2020$Filename_date),
         2020,
         "Other"
  )

licordata_means_2020$Year <- as.numeric(licordata_means_2020$Year)
licordata_means_2020$Year


#add treatment severity column
licordata_means_2020$Severity <- 
  # add species column
  ifelse(grepl("1...", licordata_means_2020$Focal_Tree),
         0,
         ifelse(grepl("2...", licordata_means_2020$Focal_Tree),
                85,
                ifelse(grepl("3...", licordata_means_2020$Focal_Tree),
                       45, 
                       ifelse(grepl("4...", licordata_means_2020$Focal_Tree),
                              65, "Other"
                       ))))
head(licordata_means_2020)

# Group by focal tree and summarize
 licordata_means_2020<- licordata_means_2020 %>%
  group_by(Focal_Tree, Species, Severity, Year) %>%
  summarize(mean_Asat = mean(MeanPhoto))
 
# add column for plot
licordata_means_2020$Plot <- 
   ifelse(grepl("1E..", licordata_means_2020$Focal_Tree),
          "D01E",
          ifelse(grepl("1W..", licordata_means_2020$Focal_Tree),
                 "D01W",
            ifelse(grepl("2E..", licordata_means_2020$Focal_Tree),
                 "D02E",
                 ifelse(grepl("2W..", licordata_means_2020$Focal_Tree),
                        "D02W",
                 ifelse(grepl("3E..", licordata_means_2020$Focal_Tree),
                        "D03E", 
                        ifelse(grepl("3W..", licordata_means_2020$Focal_Tree),
                               "D03W",
                        ifelse(grepl("4E..", licordata_means_2020$Focal_Tree),
                               "D04E", 
                               ifelse(grepl("4W..", licordata_means_2020$Focal_Tree),
                                      "D04W", "Other"
                        ))))))))


#write csv for 2020 data
write.csv(licordata_means_2020, "Means_By_Crown_Asat_values_DRep_2020.csv", row.names = FALSE)
 
#########################################
# graph 2018, 2019, and 2020 data

data2018 <- read.csv("Means_By_Crown_Asat_values_DRep_2018.csv")
data2019 <- read.csv("Means_By_Crown_Asat_values_DRep_2019.csv")
data2020 <- read.csv("Means_By_Crown_Asat_values_DRep_2020.csv")

all_data <- bind_rows(data2018, data2019, data2020)
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
Species_Canopy_boxplots <- all_data %>%
  na.omit() %>%
  ggplot(aes(x = Species, y = mean_Asat, fill = Species)) + 
  theme_classic(base_size = 13) + 
  geom_boxplot(show.legend = FALSE) +
  xlab("Tree genus") +
  ylab(bquote('Mean photosynthetic rate ( '*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')) +
  facet_grid(~Year) +
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

# df <- df.residual(CanAsatmodel)
# MSerror <- deviance(CanAsatmodel)/df
# 
LSD_output <- HSD.test(CanAsatmodel, "Species", group = TRUE, console = TRUE)

# post hoc analsis
library(emmeans)
library(rstatix)
PostHoc <- all_data %>%
  group_by(Year) %>%
  emmeans_test(mean_Asat ~ Species)
# running an LSD post hoc test: Species
LSD_output <- LSD.test(CanAsatmodel, "Severity", console = TRUE)
# get post-hoc output into a df
output_df <- data.frame(tibble::rownames_to_column(LSD_output$groups))
# seperate into severity and date columns
output_df <- transform(output_df, Severity = substr(rowname, 1, 4),
                       date = substr(rowname, 6, 15))
# sort by date and severity so that we can look which severities differ within a week
output_df <- output_df %>%
  arrange(date, severity)



