library(dplyr)

storm.data <- read.csv("repdata-data-StormData.csv")

evtypes <- data.frame(evtype = c("ASTRONOMICAL LOW TIDE", "AVALANCHE",
  "BLIZZARD", "COASTAL FLOOD", "COLD/WIND CHILL", "DEBRIS FLOW",
  "DENSE FOG", "DENSE SMOKE", "DROUGHT", "DUST DEVIL", "DUST STORM",
  "EXCESSIVE HEAT", "EXTREME COLD/WIND CHILL", "FLASH FLOOD", "FLOOD",
  "FROST/FREEZE", "FUNNEL CLOUD", "FREEZING FOG", "HAIL", "HEAT",
  "HEAVY RAIN", "HEAVY SNOW", "HIGH SURF", "HIGH WIND", "HURRICANE (TYPHOON)",
  "ICE STORM", "LAKE-EFFECT SNOW", "LAKESHORE FLOOD", "LIGHTNING",
  "MARINE HAIL", "MARINE HIGH WIND", "MARINE STRONG WIND", "MARINE THUNDERSTORM WIND",
  "RIP CURRENT", "SEICHE", "SLEET", "STORM SURGE/TIDE", "STRONG WIND",
  "THUNDERSTORM WIND", "TORNADO", "TROPICAL DEPRESSION", "TROPICAL STORM",
  "TSUNAMI", "VOLCANIC ASH", "WATERSPOUT", "WILDFIRE", "WINTER STORM",
  "WINTER WEATHER"))

storm.data$EVTYPE <- as.character(storm.data$EVTYPE)
storm.data$EVTYPE <- toupper(storm.data$EVTYPE)
storm.data$EVTYPE <- gsub("\\s+", " ", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("^\\s+", "", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("\\s+$", "", storm.data$EVTYPE, perl = TRUE)
storm.data$EVTYPE <- sub("\\s+$", "", storm.data$EVTYPE, perl = TRUE)
#storm.data$EVTYPE <- sub("(?:HEAVY|HIGH) SURF.*", "HIGH SURF", storm.data$EVTYPE, perl = TRUE)
#storm.data$EVTYPE <- sub("TSTM", "THUNDERSTORM", storm.data$EVTYPE, perl = TRUE)


#
# Handle some synonyms
#
# storm.data$EVTYPE <- sub("SHOWERS|PRECIPITATION", "RAIN", storm.data$EVTYPE, perl = TRUE)
# storm.data$EVTYPE <- sub("EXCESSIVE|RECORD", "HEAVY", storm.data$EVTYPE, perl = TRUE)
# storm.data$EVTYPE <- sub("DRIEST MONTH|DRY PATTERN|HEAVY DRYNESS|HEAVYLY DRY", "DROUGHT", storm.data$EVTYPE, perl = TRUE)
# storm.data$EVTYPE <- sub("TORRENTIAL RAINFALL", "HEAVY RAIN", storm.data$EVTYPE, perl = TRUE)

#
# Special case occassions where "HEAVY" is used instead of "HIGH" and vice-versa.
#
# storm.data$EVTYPE <- sub("HEAVY (SURF|WIND)", "HIGH \\1", storm.data$EVTYPE, perl = TRUE)
# storm.data$EVTYPE <- sub("HIGH (RAIN|SNOW)", "HEAVY \\1", storm.data$EVTYPE, perl = TRUE)

#
# Special case to handle evtypes that can have a "MARINE" prefix / qualifier
#
# storm.data$EVTYPE <- sub("(?:(MARINE\\s+)|(.*))HAIL.*", "\\1HAIL", storm.data$EVTYPE, perl = TRUE)
# storm.data$EVTYPE <- sub("(?:(MARINE\\s+)|(.*))HIGH WIND.*", "\\1HIGH WIND", storm.data$EVTYPE, perl = TRUE)
# storm.data$EVTYPE <- sub("(?:(MARINE\\s+)|(.*))STRONG WIND.*", "\\1STRONG WIND", storm.data$EVTYPE, perl = TRUE)
# storm.data$EVTYPE <- sub("(?:(MARINE\\s+)|(.*))THUNDE.*[ST].*M.*", "\\1THUNDERSTORM WIND", storm.data$EVTYPE, perl = TRUE)
# 
# storm.data$EVTYPE <- sub(".*HURRICANE.*", "HURRICANE (TYPHOON)", storm.data$EVTYPE, perl = TRUE)
# storm.data$EVTYPE <- sub(".*(HEAVY RAIN|RAIN \\(HEAVY\\)).*", "HEAVY RAIN", storm.data$EVTYPE, perl = TRUE)
# 
# storm.data <- storm.data[!grepl("SUMMARY.*", storm.data$EVTYPE),]

storm.data$EVTYPE <- as.factor(storm.data$EVTYPE)

# evtypes.short.list <- evtypes$evtype[!evtypes$evtype %in% c("HAIL", "HIGH WIND", "STRONG WIND", "THUNDERSTORM WIND", "MARINE HAIL", "MARINE HIGH WIND", "MARINE STRONG WIND", "MARINE THUNDERSTORM WIND")]
# 
# for(evtype in evtypes.short.list) {
#   storm.data$EVTYPE <- sub(sprintf(".*%s.*", evtype), evtype, storm.data$EVTYPE, perl = TRUE)
# }

evtype.values.before <- length(unique(storm.data$EVTYPE))
storm.data.rows.before <- nrow(storm.data)

storm.data <- storm.data %>% filter(storm.data$EVTYPE %in% evtypes$evtype)

evtype.values.after <- length(unique(storm.data$EVTYPE))
storm.data.rows.after <- nrow(storm.data)

storm.data.summary <- storm.data %>%
  group_by(EVTYPE) %>%
  summarise_each(funs(sum), FATALITIES, INJURIES) %>%
  arrange(desc(FATALITIES))

storm.data.summary$INCIDENTS <- storm.data.summary$FATALITIES + storm.data.summary$INJURIES

fatalities <- head(storm.data.summary, n = 5)
injuries <- head(storm.data.summary %>% arrange(desc(INJURIES)), n = 5)
incidents <- head(storm.data.summary %>% arrange(desc(INCIDENTS)), n = 5)

fatalities <- fatalities %>% melt(id.vars = "EVTYPE", measure.vars = "FATALITIES", variable.name = "Category")
injuries <- injuries %>% melt(id.vars = "EVTYPE", measure.vars = "INJURIES", variable.name = "Category")
incidents <- incidents %>% melt(id.vars = "EVTYPE", measure.vars = "INCIDENTS", variable.name = "Category")

most.harmful <- rbind(fatalities, injuries)

ggplot(data=most.harmful, aes(x=EVTYPE, y=value, fill=Category)) +
  geom_bar(stat="identity") +
  labs(variable = "Foo") +
  xlab("Storm Event") + ylab("Count") + # Set axis labels
  ggtitle("Human Harm")

ggplot(data=most.harmful, aes(x=EVTYPE, y=value, fill=Category)) + 
  geom_bar(colour="black", stat="identity",
           position=position_dodge(),
           size=.3) +                        # Thinner lines
  scale_fill_hue(name="Metric") +      # Set legend title
  xlab("Storm Event") + ylab("Count") + # Set axis labels
  ggtitle("Human Harm") +     # Set title
  theme_bw()

ggplot(data=most.harmful, aes(x=EVTYPE, y=value, fill=EVTYPE)) + 
  geom_bar(colour="black", stat="identity",
           position=position_dodge(),
           size=.3) +                        # Thinner lines
  xlab("Storm Event") + ylab("Count") + # Set axis labels
  ggtitle("Human Harm") +     # Set title
  theme_bw() + facet_grid(variable ~ .)


how.many.has.text <- function(text) {
  unique(storm.data[grepl(text, storm.data$EVTYPE), "EVTYPE"])
}
