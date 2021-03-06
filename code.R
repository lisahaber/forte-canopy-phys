####################################
## Lisa T. Haber                  ##
## 2019.06.04                     ##
## FoRTE Canopy Physiology        ##
####################################

# This code extracts 2018 canopy physiology (LICOR 6400XT) files from the shared FoRTE data drive.
## (These data are already extracted to the data directory in this project.)

# load required packages
library(dplyr)
library(readr)
library(googledrive)
library(ggplot2)
library(tidyr)
library(lubridate)

# Direct Google Drive link to "FoRTE/data/subcanopy_leaf_physiology"
as_id("https://drive.google.com/drive/u/0/folders/1z9kDZfHjANUFafadc5HDLSlcTNSeThCQ") %>% 
  drive_ls ->
  gdfiles

# Create a new data directory for files, if necessary
data_dir <- "data/"
if(!dir.exists(data_dir)) dir.create(data_dir)

# Download data
for(f in seq_len(nrow(gdfiles))) {
  cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
  drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
}

# Get a (fresh) list of the downloaded data we're working with
# Filenames we want end with eight digits and no file extension
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


#############################################################
# get means of 5 observations per leaf to use in analysis

licordata_means <- licordata %>%
  group_by(Filename, Species) %>%
  summarize(MeanPhoto = mean(Photo))
licordata_means  

write.csv(licordata_means,"Mean_Photo_Cond_Canopy_2018.csv",row.names=FALSE)


#############################################
# power analysis #1: 216 leaf samples 
# leaves as experimental units (rather than trees)
#############################################

## use package 'pwr'
install.packages("pwr")
library(pwr)

cohen.ES(test = "r", size = "medium")
## for effect size "medium" in correlation test, effect size value is 0.3

cohen.ES(test = "f2", size = "medium")
## for effect size "medium" in GLM, effect size value is 0.15

pwr.r.test(r = 0.3, sig.level = 0.05, power = 0.8, alternative = "greater")
pwr.r.test(r = 0.5, sig.level = 0.05, power = 0.8, alternative = "greater")
# for correlation, sample size = 67 for medium effect or 23 for a large effect

amax <- read.csv("Mean_Photo_Cond_Canopy_2018_ForPowerAnalysisManuallyCleaned.csv")
amax <- amax %>%
  filter(MeanPhoto > 0)


#model
fit <- aov(MeanPhoto ~ Species, data = amax)

#within
anova(fit)["Residuals", "Mean Sq"]
# within-Species variance is 9.675847

#between
anova(fit)["Species", "Mean Sq"]
# between Species variance is 1376.35

## This is HUGE between-group variance; wondering if I screwed up somehow...like, the groups do NOT look this non-overlapping in the boxplots generated below.

power.anova.test(groups = 3, n = 72,
                 within.var = 9.675847, 
                 between.var = 1376.35, 
                 sig.level = 0.05, power = NULL)

# result is a power = 1. 

# leaf box plot
boxplot(MeanPhoto ~ Species, data = amax)

##################################################################
# power analysis #2: for trees rather than individual leaves
# 72 trees as experimental unit
##################################################################
amax2 <- read.csv("Mean_Photo_Cond_Canopy_2018_WithTreeIDs.csv")
amax2 <- amax2 %>%
  filter(MeanPhoto > 0)

amax2 <- amax2 %>%
  group_by(TreeID, Species) %>%
  summarize(MeanPhotoTree = mean(MeanPhoto))

amax2

#model
fit <- aov(MeanPhotoTree ~ Species, data = amax2)
summary(fit)

#within
anova(fit)["Residuals", "Mean Sq"]
# within-Species variance is 7.6

#between
anova(fit)["Species", "Mean Sq"]
# between Species variance is 437.5


power.anova.test(groups = 3, n = 24,
                 within.var = 7.6, 
                 between.var = 437.5, 
                 sig.level = 0.05, power = NULL)

# again, power =1

# tree box plot
boxplot(MeanPhotoTree ~ Species, data = amax2)

###################################################
# Jeff's suggested code
amax %>%
  group_by(Species) %>%
  summarise(amax_mu = mean(MeanPhoto)) -> species.means

sample.sd <- sd(amax$MeanPhoto)

p <- power.anova.test(groups = 3,
                      within.var = var(species.means$amax_mu),
                      between.var = sample.sd,
                      sig.level = 0.05, power = 0.8, n = NULL)
#solve for n
#you can change the power to whatever
this gets like 21?

