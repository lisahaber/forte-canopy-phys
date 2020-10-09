#############################################
######################################################################
## FoRTE 2018 Canopy Physiology data extraction & compilation
## Fall 2018
## Lisa Haber
## Reusing/modifying code from 2018
#######################################################################

## LICOR file names for canopy focal trees are formatted: D01E_acru_can01_07222018: 4-digit plot ID, 4-letter species code, 5-character leaf sample ID, MMDDYYYY

#1. Extract and utilize canopy physiology (LICOR 6400XT) files from the shared FoRTE data drive.

# load required packages
library(dplyr)
library(readr)
library(googledrive)
library(ggplot2)
library(tidyr)
library(lubridate)
library(tidyverse)
# 
# # Direct Google Drive link to "FoRTE/data/canopy_leaf_physiology/2019"
# as_id("https://drive.google.com/drive/u/0/folders/17yVbBxtsmrIvbAEFB8PAZauBpU_IkZXw") %>% 
#   drive_ls ->
#   gdfiles
# 
# # Create a new data directory for files, if necessary
data_dir <- "data/"
# if(!dir.exists(data_dir)) dir.create(data_dir)
# 
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
  separate(Filename, into = c("Plot", "Species", "Sample", "Filename_date"), remove = FALSE) ->
  licordata

##############################
# get means of 5 observations per leaf to use in analysis

licordata_means_2018 <- licordata %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo)) %>%
  separate(Filename, into = c("Plot", "Species", "Sample", "Filename_date"), remove = FALSE) %>%
  mutate(Date = mdy(Filename_date))
licordata_means_2018  

#import look up table with Tree IDs
tree_IDs <- read.csv("Mean_Photo_Cond_Canopy_2018_WithTreeIDs.csv")

licordata_means_2018 <- bind_cols(licordata_means_2018, tree_IDs)

licordata_means_2018 <- licordata_means_2018 %>%
  filter(MeanPhoto >= 0)

all( licordata_means_2018$MeanPhoto >= 0) #check for presence of negative values


#####################################

#add treatment severity column
licordata_means_2018$Severity <- 
  # add species column
  ifelse(grepl("..1.", licordata_means_2018$Plot),
         0,
         ifelse(grepl("..2.", licordata_means_2018$Plot),
                85,
                ifelse(grepl("..3.", licordata_means_2018$Plot),
                       45, 
                       ifelse(grepl("..4.", licordata_means_2018$Plot),
                              65, "Other"
                       ))))
head(licordata_means_2018)

#add year column
licordata_means_2018$Year <- 
  ifelse(grepl("....2018", licordata_means_2018$Filename_date),
         2018,
        "Other"
                       )
head(licordata_means_2018)

# change species column
#add treatment severity column
licordata_means_2018$Species <- 
ifelse(grepl("acru", licordata_means_2018$Species1),
       "Acer",
       ifelse(grepl("pogr", licordata_means_2018$Species1),
              "Populus",
              ifelse(grepl("quru", licordata_means_2018$Species1),
                     "Quercus", "Other"
              )))
head(licordata_means_2018)


licordata_means_2018 <- licordata_means_2018 %>%
  rename(Focal_Tree = TreeID) %>%
  group_by(Focal_Tree, Plot, Species, Severity, Year) %>%
  summarize(mean_Asat = mean(MeanPhoto))

write.csv(licordata_means_2018, "Means_By_Crown_Asat_values_DRep_2018.csv", row.names = FALSE)

#########################################
# graph 2018 data

# boxplots <- licordata_means_2018 %>%
#   na.omit() %>%
#   group_by(Severity) %>%
#   ggplot(aes(x = Severity, y = MeanPhoto, fill = Severity)) +
#   theme_classic(base_size = 14) +
#   geom_boxplot() +
#   xlab("Severity (% LAI Lost)") +
#   ylab(bquote('Mean photosynthetic rate ('*mu~ 'mol' ~CO[2]~ m^-2~s^-1*')')) +
#   scale_fill_manual(values = c("#000000", "#009E73", "#0072B2", "#D55E00"))


#### Model Run
## 2018
library(agricolae)

# clean up my df for model run
licordata_means_2018 <- licordata_means_2018 %>%
  select(Plot, Species, Severity, MeanPhoto) %>%
  group_by(Plot, Species) %>%
  summarize(MeanSpeciesPhoto = mean(MeanPhoto))
head(licordata_means_2018)
licordata_means_2018$Severity <- as.factor(licordata_means_2018$Severity)
licordata_means_2018$Species <- as.factor(licordata_means_2018$Species)
# recode 0 to 0.00 to help me when with the post-hoc output
licordata_means_2018$Severity <- recode_factor(licordata_means_2018$Severity, "0" = "0.00")
model_run <- licordata_means_2018 %>%
  ungroup() %>%
  select(Severity, Species, MeanPhoto) %>%
  filter(!is.nan(MeanPhoto))
# overall anova split-split plot model run
NPPmodel <- with(model_run, ssp.plot(replicate, treatment, week, severity,
                                     kgC_per_ha_day))
# post hoc analsis
# creating an object for each part of the model
gla <- NPPmodel$gl.a
glb <- NPPmodel$gl.b
glc <- NPPmodel$gl.c
# doing the same as above for each error term
Ea <- NPPmodel$Ea
Eb <- NPPmodel$Eb
Ec <- NPPmodel$Ec
# running an LSD post hoc test: severity-week
LSD_ouput <- with(model_run, LSD.test(kgC_per_ha_day, severity:week, glc, Ec,
                                      console = TRUE))
# LSD post hoc: severity-treatment
LSD_ouput <- with(model_run, LSD.test(kgC_per_ha_day, severity:treatment, glc, Ec,
                                      console = TRUE))
# get post-hoc output into a df
output_df <- data.frame(tibble::rownames_to_column(LSD_ouput$groups))
# seperate into severity and date columns
output_df <- transform(output_df, severity = substr(rowname, 1, 4),
                       date = substr(rowname, 6, 15))
# sort by date and severity so that we can look which severities differ within a week
output_df <- output_df %>%
  arrange(date, severity)
