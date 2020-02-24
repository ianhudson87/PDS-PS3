rm(list=ls())

### PART 1

# get tibble
primaryPolls<-read.csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv', stringsAsFactors = F)
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%Y")
primaryPolls<-primaryPolls[primaryPolls$candidate_name%in%c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"),]

# get data from dsired states
poll.data<-primaryPolls[primaryPolls$state %in% c("Alabama", "Arkansas", "California", "Colorado", "Maine", "Massachusetts", "Minnesota", "North Carolina", "Oklahoma", "Tennessee", "Texas", "Utah", "Vermont", "Virginia"),]


ggplot(data=poll.data, mapping=aes(x=start_date, y=pct))+
  geom_point(mapping=aes(color=state))+
  geom_smooth(se=F)+
  facet_wrap(~candidate_name, nrow=3)+
  theme_minimal()+
  labs(x="Date", y="Percentage of Vote", title="State of Primary Race", color="State")+
  theme(axis.text.x=element_text(angle=90, hjust=1)) # rotate x labels


### PART 2
library(tidyverse)
primaryPolls<-read_csv('https://jmontgomery.github.io/PDS/Datasets/president_primary_polls_feb2020.csv')
primaryPolls$start_date<-as.Date(primaryPolls$start_date, "%m/%d/%y")

primaryPolls %>% select(sample_size)

# get useful columns of data
basicPolls<-select(primaryPolls, state, candidate_name, start_date, pct)

# get relevant candidates
basicPolls <- basicPolls %>% filter(candidate_name %in% c("Amy Klobuchar", "Bernard Sanders", "Elizabeth Warren", "Joseph R. Biden Jr.", "Michael Bloomberg", "Pete Buttigieg"))
basicPolls <- basicPolls %>% arrange(state)