library(readr)
library(dplyr)
library(mapproj)
library(ggplot2)

sensor.locations <- read_csv('sensorreference.csv')

sensor.locations <- sensor.locations %>%
  mutate(
    length = sqrt(x^2 + y^2 + z^2),
    x.unit = x / length,
    y.unit = y / length,
    z.unit = z / length,
    lat = asin(z.unit) * 90 / (pi / 2),
    long = atan2(x.unit, y.unit) * 90 / (pi / 2)
  )

z <- mapproject(sensor.locations$long, sensor.locations$lat, projection = "azequidistant", orientation=c(90,0,0))

sensor.locations$x.project <- z$x
sensor.locations$y.project <- -z$y

ggplot(sensor.locations, aes(x=x.project, y=y.project, label=electrode))+
  geom_point()+
  geom_text()+
  #geom_text(aes(x=x.unit, y=y.unit), color="red") +
  theme_minimal()+
  coord_equal()
  
  
