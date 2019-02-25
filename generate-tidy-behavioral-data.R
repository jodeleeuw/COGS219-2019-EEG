library(readr)

all.behavioral.files <- dir('data/raw/behavioral/')

all.behavioral.data <- NA
for(f in all.behavioral.files){
  this.p.data <- read_csv(paste0('data/raw/behavioral/',f))
  if(is.na(all.behavioral.data)){
    all.behavioral.data <- this.p.data
  } else {
    all.behavioral.data <- rbind(all.behavioral.data, this.p.data)
  }
}

write_csv(all.behavioral.data, 'data/tidy/behavioral-data-tidy.csv')
