library(dplyr)

storm.data <- read.csv("repdata-data-StormData.csv")

storm.data.summary <- storm.data %>% 
  group_by(EVTYPE) %>% 
  summarise_each(funs(sum), FATALITIES, INJURIES) %>% 
  arrange(desc(FATALITIES))

storm.data.summary$INCIDENTS <- storm.data.summary$FATALITIES + storm.data.summary$INJURIES

head(storm.data.summary, n = 10)
head(storm.data.summary %>% arrange(desc(INCIDENTS)), n = 10)

storm.data$EVTYPE <- as.character(storm.data$EVTYPE)
storm.data$EVTYPE <- toupper(storm.data$EVTYPE)
storm.data$EVTYPE <- gsub("\\s+", " ", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("^\\s+", "", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("\\s+$", "", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("\\s+$", "", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("HIGH SURF.*", "HIGH SURF", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("TSTM", "THUNDERSTORM", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("^THUNDERSTORM.*", "THUNDERSTORM", storm.data$EVTYPE, perl = TRUE)
storm.data <- storm.data[!grepl("SUMMARY.*", storm.data$EVTYPE),]
storm.data$EVTYPE <- as.factor(storm.data$EVTYPE)

