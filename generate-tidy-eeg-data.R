#
# This script was run to convert the segmented EEG data to tidy format.
#
# Raw segmented data are stored in the /data/raw folder
# Tidy data produced by this script are in /data/generated
#

library(readr)
library(tidyr)
library(stringr)
library(dplyr)

# EEG Data ####

all.files <- dir('data/raw/eeg')

all.data <- NA
for(f in all.files){
  subject <- f %>% stringr::str_sub(start=14,end=15)
  lag.condition <- f %>% stringr::str_extract("(Lag)[2,8]+")
  distractor.condition <- f %>% stringr::str_extract("(?<=Lag[2,8])\\w+")
  file.data <- read_table2(paste0('data/raw/eeg/',f), col_names = as.character(1:129))
  file.data$t <- seq(from=-200, to=1195, by=5)
  file.data.tidy <- file.data %>% gather(key="electrode", value=voltage, 1:129)
  file.data.tidy$subject <- subject
  file.data.tidy$lag.condition <- lag.condition
  file.data.tidy$distractor.condition <- distractor.condition
  if(is.na(all.data)){
    all.data <- file.data.tidy
  } else {
    all.data <- rbind(all.data, file.data.tidy)
  }
}


write_csv(all.data, path="data/tidy/eeg-data-tidy.csv")
