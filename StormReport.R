library(dplyr)

storm.data <- read.csv("repdata-data-StormData.csv")

storm.data.summary <- storm.data %>% 
  group_by(EVTYPE) %>% 
  summarise_each(funs(sum), FATALITIES, INJURIES) %>% 
  arrange(desc(FATALITIES))

storm.data.summary$INCIDENTS <- storm.data.summary$FATALITIES + storm.data.summary$INJURIES

head(storm.data.summary, n = 10)
head(storm.data.summary %>% arrange(desc(INCIDENTS)), n = 10)
