library(dplyr)

storm.data <- read.csv("repdata-data-StormData.csv")
evtypes <- read.csv("evtypes.csv")
evtypes$evtype <- toupper(evtypes$evtype)

storm.data$EVTYPE <- as.character(storm.data$EVTYPE)
storm.data$EVTYPE <- toupper(storm.data$EVTYPE)
storm.data$EVTYPE <- gsub("\\s+", " ", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("^\\s+", "", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("\\s+$", "", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("\\s+$", "", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("(?:HEAVY|HIGH) SURF.*", "HIGH SURF", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("TSTM", "THUNDERSTORM", storm.data$EVTYPE, perl = TRUE)


#
# Handle some synonyms
#
storm.data$EVTYPE <- sub("SHOWERS|PRECIPITATION", "RAIN", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("EXCESSIVE|RECORD", "HEAVY", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("DRIEST MONTH|DRY PATTERN|HEAVY DRYNESS|HEAVYLY DRY", "DROUGHT", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("TORRENTIAL RAINFALL", "HEAVY RAIN", storm.data$EVTYPE, perl = TRUE)

#
# Special case occassions where "HEAVY" is used instead of "HIGH" and vice-versa.
#
storm.data$EVTYPE <- sub("HEAVY (SURF|WIND)", "HIGH \\1", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("HIGH (RAIN|SNOW)", "HEAVY \\1", storm.data$EVTYPE, perl = TRUE)

#
# Special case to handle evtypes that can have a "MARINE" prefix / qualifier
#
storm.data$EVTYPE <- sub("(?:(MARINE\\s+)|(.*))HAIL.*", "\\1HAIL", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("(?:(MARINE\\s+)|(.*))HIGH WIND.*", "\\1HIGH WIND", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("(?:(MARINE\\s+)|(.*))STRONG WIND.*", "\\1STRONG WIND", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("(?:(MARINE\\s+)|(.*))THUNDE.*[ST].*M.*", "\\1THUNDERSTORM WIND", storm.data$EVTYPE, perl = TRUE)

storm.data$EVTYPE <- sub(".*HURRICANE.*", "HURRICANE (TYPHOON)", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub(".*(HEAVY RAIN|RAIN \\(HEAVY\\)).*", "HEAVY RAIN", storm.data$EVTYPE, perl = TRUE)

storm.data <- storm.data[!grepl("SUMMARY.*", storm.data$EVTYPE),]

storm.data$EVTYPE <- as.factor(storm.data$EVTYPE)

evtypes.short.list <- evtypes$evtype[!evtypes$evtype %in% c("HAIL", "HIGH WIND", "STRONG WIND", "THUNDERSTORM WIND", "MARINE HAIL", "MARINE HIGH WIND", "MARINE STRONG WIND", "MARINE THUNDERSTORM WIND")]

for(evtype in evtypes.short.list) {
  storm.data$EVTYPE <- sub(sprintf(".*%s.*", evtype), evtype, storm.data$EVTYPE, perl = TRUE)
}

unique(storm.data$EVTYPE)

storm.data <- storm.data %>% filter(storm.data$EVTYPE %in% evtypes$evtype)

unique(storm.data$EVTYPE)

storm.data.summary <- storm.data %>% 
  group_by(EVTYPE) %>% 
  summarise_each(funs(sum), FATALITIES, INJURIES) %>% 
  arrange(desc(FATALITIES))

storm.data.summary$INCIDENTS <- storm.data.summary$FATALITIES + storm.data.summary$INJURIES

head(storm.data.summary, n = 10)
head(storm.data.summary %>% arrange(desc(INCIDENTS)), n = 10)

how.many.has.text <- function(text) {
  unique(storm.data[grepl(text, storm.data$EVTYPE), "EVTYPE"])
}
