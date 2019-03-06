library(readr)
library(stringr)

all.behavioral.files <- dir('data/raw/behavioral/')

raw.behavioral.data <- NA
#f <- all.behavioral.files[34]
for(f in all.behavioral.files){
  subject_id <- f %>% stringr::str_sub(start=13,end=14)
  this.p.data <- read_csv(paste0('data/raw/behavioral/',f),locale = locale(encoding = 'UTF-8'))
  this.p.data$participant_id <- subject_id
  if(is.na(all.behavioral.data)){
    raw.behavioral.data <- this.p.data
  } else {
    raw.behavioral.data <- rbind(raw.behavioral.data, this.p.data)
  }
}

# basic check on data length

n.per.subject <- raw.behavioral.data %>% group_by(participant_id) %>% summarize(N=n())

# clearly we have some data corruption issues in some files. can we identify trials by
# looking specifically for the critical test trials?

test.behavioral.data <- raw.behavioral.data %>% 
  filter(phase=='test', correct %in% c("true", "false"), distractor_type %in% c("bas", "neu", "neg"), target_lag %in% c(2,8)) %>% 
  select(participant_id, trial_index, distractor_type, target_lag, target_orientation, distractor_position, correct, rt, button_pressed, phase)

# we should have 600 trials per subject

n.test.per.subject <- test.behavioral.data %>% group_by(participant_id) %>% summarize(N=n())

write_csv(test.behavioral.data, 'data/tidy/behavioral-data-tidy.csv')

#guess_encoding('data/raw/behavioral/participant_39_behavioral.csv')
