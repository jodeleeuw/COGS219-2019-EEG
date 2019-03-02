library(readr)
library(ggplot2)
library(dplyr)
library(ez)

eeg.data <- read_csv('data/tidy/eeg-data-tidy.csv')

# example of finding the difference waves between the lag8 baseline and lag8 negative distractor conditions
# filtering to a single electrode just for the demonstration

# N2 analysis

# subtract lag 8 baseline from lag 8 neu/neg conditions, averaged over the
# relevant electrodes (64, 68, 69 LEFT & 89, 94, 95 RIGHT).

lag8.n2 <- eeg.data %>%
  filter(electrode %in% c(64, 68, 69, 89, 94, 95)) %>%
  filter(lag.condition == "Lag8") %>%
  group_by(subject, t, distractor.condition) %>%
  summarize(M.voltage = mean(voltage)) 

lag8.n2.summary <- lag8.n2 %>%
  group_by(distractor.condition, t) %>%
  summarize(voltage = mean(M.voltage), SE = sd(M.voltage)/sqrt(n()))

ggplot(lag8.n2.summary, aes(x=t, y=voltage, ymin=voltage-SE, ymax=voltage+SE, fill=distractor.condition))+
  geom_ribbon(alpha=0.5)+
  geom_line(aes(color=distractor.condition), size=1)+
  theme_bw()

lag8.n2.baseline <- lag8.n2 %>%
  filter(distractor.condition == "Baseline") %>%
  mutate(baseline.voltage = M.voltage) %>%
  select(subject, t, baseline.voltage)

lag8.n2.difference <- lag8.n2 %>%
  inner_join(lag8.n2.baseline) %>%
  mutate(difference.wave = M.voltage - baseline.voltage) %>%
  select(subject, t, distractor.condition, difference.wave)

lag8.n2.difference.summary <- lag8.n2.difference %>%
  group_by(t, distractor.condition) %>%
  summarize(M = mean(difference.wave), SE = sd(difference.wave) / sqrt(n()))

ggplot(lag8.n2.difference.summary, aes(x=t, y=M, ymin=M-SE, ymax=M+SE))+
  geom_ribbon(aes(fill=distractor.condition), alpha=0.3)+
  geom_line(aes(color=distractor.condition), size=1)+
  scale_x_continuous(limits=c(-200,1000), expand=c(0,0))+
  theme_bw()
