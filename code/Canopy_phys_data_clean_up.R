####################################
## Lisa T. Haber                  ##
## 2020.03.02                     ##
## FoRTE 2018 Canopy Physiology     ##
## Cleaned Data Set for Data Paper##
####################################

## This code extracts "cleaned" Licor phys data for the FoRTE data paper effort being led by Jeff Atkins, spring 2020

## To clean the 2018 data set, I did the following:

#1. used only the last five logged values from each Licor file, as a small subset of measurements from 2018 and 2019 included more than the standard 5 logs due to error

## note that Licor file naming scheme changed from 2018 to 2019 to streamline, reduce redundancy and error. 

# load required packages
library(tidyverse)
library(googledrive)
library(lubridate)

## First code chunk: originally written by BBL

# Create a new data directory for files, if necessary
data_dir <- "data/"
if(!dir.exists(data_dir)) dir.create(data_dir)

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
## exclude the unwanted dates: "07052018" & "07102018"
filedata %>% 
  bind_rows %>% 
  as_tibble %>% 
  mutate(Timestamp = mdy_hms(Timestamp)) %>%  # change to a POSIXct object
  separate(Filename, into = c("Plot", "Species", "Sample", "Filename_date"), remove = FALSE) ->
  licordata

unique(licordata$Filename_date)

## Second code chunk: exclude excessive observations
# figure out which dates/files have more than 5 observations
excessobs <- licordata %>%
  subset(Obs >= 6)

dates <- excessobs$Filename

view(excessobs)
print(unique(dates))

# remove excess observations
# figure out which rows in dataframe need removal (easiest to just do this by hand at this point)
view(licordata)

# now, exclude them. Note that this means the affected files will have observations beginning at "2" or "6" or "11", not "1"
drop <- c(106:108, 114:118, 189:193, 199:208, 374, 390:399, 470:479, 540:554, 625:629,
          690, 816:820, 826:830, 866:870, 931:935, 1241:1245, 1266:1270, 1281:1282,
          1288:1292)

licordata1 <- licordata[-drop, ]

view(licordata1)  

nrow(licordata1)  

write.csv(licordata1, "Canopy_LI6400_Phys_Data_Clean_2018.csv", row.names = FALSE)

### check to see what overall non-negative sample size is
licordata1_means <- licordata1 %>%
  group_by(Filename) %>%
  summarize(MeanPhoto = mean(Photo)) %>%
  filter(MeanPhoto >= 0)

nrow(licordata1_means)


