library(readr)
library(dplyr)
library(ggplot2)

all.behavioral.data <- read_csv('data/tidy/behavioral-data-tidy.csv')

subject.level <- all.behavioral.data %>% 
  filter(phase == 'test', target_lag %in% c(2,8)) %>%
  mutate(correct = as.numeric((correct == 'true'))) %>%
  group_by(participant_id, distractor_type, target_lag) %>%
  summarize(accuracy = mean(correct))

summary.data <- subject.level %>%
  group_by(distractor_type, target_lag) %>%
  summarize(M=mean(accuracy), SD=sd(accuracy), SE=sd(accuracy)/sqrt(n()))

ggplot(subject.level, aes(x=target_lag, y=accuracy, color=distractor_type, group=interaction(participant_id, distractor_type)))+
  geom_line(size=2)+
  facet_wrap(.~participant_id)+
  theme_minimal()
  
  
ggplot(summary.data, aes(x=target_lag, y=M, color=distractor_type, group=distractor_type))+
  geom_line(size=1)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=M-SE, ymax=M+SE), size=1,width=0.2)+
  theme_bw()+
  theme(panel.grid=element_blank())
  

t.test(~accuracy, data=subject.level, mu = 0)









library(ez)
ezANOVA(subject.level, dv=accuracy, wid=participant_id, within=c(target_lag, distractor_type))
