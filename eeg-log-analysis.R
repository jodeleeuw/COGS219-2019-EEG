library(readr)
library(dplyr)
library(stringr)
library(tidyr)

all.logs <- dir('data/raw/eeglogs/')

f <- all.logs[1]

summary.log <- NA
for(f in all.logs){
  log.file <- read_tsv(paste0('data/raw/eeglogs/',f))
  subject_id <- f %>% stringr::str_sub(start=14,end=15)
  good.by.condition <- log.file %>% 
    filter(`Category` %in% c('Lag2Negative', 'Lag2Neutral', 'Lag2Baseline', 'Lag8Negative', 'Lag8Neutral', 'Lag8Baseline')) %>%
    mutate(good = as.numeric(`Segment Good` == "true")) %>%
    group_by(`Category`) %>%
    summarize(CountGood = sum(good), ProportionGood = mean(good)) %>%
    mutate(Subject = subject_id) %>%
    select(Subject, Category, CountGood, ProportionGood)
  if(is.na(summary.log)){
    summary.log <- good.by.condition
  } else {
    summary.log <- rbind(summary.log, good.by.condition)
  }
}

overall <- summary.log %>% group_by(Subject) %>% summarize(C = sum(CountGood), P = C/600)
write_csv(overall, 'data/tidy/eeg-segment-count.csv')
