setwd("D:/Users/niprakas/Desktop/Cousera/Reproducable data/Week4")


# Download the data and unzip it.

if (!"StormData.csv.bz2" %in% dir("./")) {
    print("Downloading File.....")
    download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", destfile = "StormData.csv.bz2")
}
# Generate csv file.
if (!"storm" %in% ls()) {
    storm <- read.csv(bzfile("StormData.csv.bz2"), sep = ",", header = TRUE, stringsAsFactors = FALSE)
}
dim(storm)

# As per document extract data as per event, Create vector.
events <- c("Astronomical Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme cold/Wind Chill", "Flash Flood", "Flood", "Freezing", "Frost/Freeze", "Funnel Cloud", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane/Typhoon", "Ice Storm", "Lakeshore Flood", "Lake-Effect Snow", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind", "Rip Current", "Seiche", "Sleet", "Storm Tide", "Strong Wind", "Thunderstorm Wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")

# For combined event, extract using regular expression.
events_regex <- c("Astronomical Low Tide|Low Tide", "Avalanche", "Blizzard", "Coastal Flood", "Cold/Wind Chill", "Debris Flow", "Dense Fog", "Dense Smoke", "Drought", "Dust Devil", "Dust Storm", "Excessive Heat", "Extreme cold/Wind Chill|Extreme Cold|Wind Chill", "Flash Flood", "Flood", "Freezing", "Frost/Freeze|Frost|Freeze", "Funnel Cloud", "Hail", "Heat", "Heavy Rain", "Heavy Snow", "High Surf", "High Wind", "Hurricane/Typhoon|Hurricane|Typhoon", "Ice Storm", "Lakeshore Flood", "Lake-Effect Snow", "Lightning", "Marine Hail", "Marine High Wind", "Marine Strong Wind", "Marine Thunderstorm Wind|Marine tstm Wind", "Rip Current", "Seiche", "Sleet", "Storm Tide", "Strong Wind", "Thunderstorm Wind|tstm wind", "Tornado", "Tropical Depression", "Tropical Storm", "Tsunami", "Volcanic Ash", "Waterspout", "Wildfire", "Winter Storm", "Winter Weather")  

# Extract rows corresponding to the event.
# EVTYPE -> Type of event
# FATALITIES -> Number of fatalities
# INJURIES -> Number of injuries
# PROPDMG -> Amount of property damage in orders of magnitude
# PROPDMGEXP -> Order of magnitude for property damage (e.g. K for thousands)
# CROPDMG -> Amount of crop damage in orders of magnitude
# PROPDMGEXP -> Order of magnitude for crop damage (e.g. M for millions)

options(scipen = 999)  # force fixed notation of numbers instead of scientific
cleandata <- data.frame(EVTYPE = character(0), FATALITIES = numeric(0), INJURIES = numeric(0), PROPDMG = numeric(0), PROPDMGEXP = character(0), CROPDMG = numeric(0), CROPDMGEXP = character(0))  
for (i in 1:length(events)) {
rows <- storm[grep(events_regex[i], ignore.case = TRUE, storm$EVTYPE), ]
rows <- rows[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "PROPDMGEXP", "CROPDMG", "CROPDMGEXP")]
CLEANNAME <- c(rep(events[i], nrow(rows)))
rows <- cbind(rows, CLEANNAME)
cleandata <- rbind(cleandata, rows)
}

#order of magnitude of property and crop damage
#(H = hundreds, K = thousands, M = millions, B= billions)
# convert letter exponents to integers

cleandata[(cleandata$PROPDMGEXP == "K" | cleandata$PROPDMGEXP == "k"), ]$PROPDMGEXP <- 3
cleandata[(cleandata$PROPDMGEXP == "M" | cleandata$PROPDMGEXP == "m"), ]$PROPDMGEXP <- 6
cleandata[(cleandata$PROPDMGEXP == "B" | cleandata$PROPDMGEXP == "b"), ]$PROPDMGEXP <- 9
cleandata[(cleandata$CROPDMGEXP == "K" | cleandata$CROPDMGEXP == "k"), ]$CROPDMGEXP <- 3
cleandata[(cleandata$CROPDMGEXP == "M" | cleandata$CROPDMGEXP == "m"), ]$CROPDMGEXP <- 6
cleandata[(cleandata$CROPDMGEXP == "B" | cleandata$CROPDMGEXP == "b"), ]$CROPDMGEXP <- 9

#Compute combined economic damage (property damage + crops damage)
# multiply property and crops damage by 10 raised to the power of the exponent
suppressWarnings(cleandata$PROPDMG <- cleandata$PROPDMG * 10^as.numeric(cleandata$PROPDMGEXP))  
suppressWarnings(cleandata$CROPDMG <- cleandata$CROPDMG * 10^as.numeric(cleandata$CROPDMGEXP))
# compute combined economic damage (property damage + crops damage)
suppressWarnings(TOTECODMG <- cleandata$PROPDMG + cleandata$CROPDMG)
cleandata <- cbind(cleandata, TOTECODMG)
# delete 'PROPDMGEXP' and 'CROPDMGEXP'columns which have become unnecessary after conversion
cleandata <- cleandata[, c("EVTYPE", "FATALITIES", "INJURIES", "PROPDMG", "CROPDMG", "CLEANNAME", "TOTECODMG")]

#Now data ready for graph ploting
#Fatalities and Injuries

fatalities <- aggregate(FATALITIES ~ CLEANNAME, data = cleandata, FUN = sum)
fatalities <- fatalities[order(fatalities$FATALITIES, decreasing = TRUE), ]
# 10 most harmful causes of fatalities
MaxFatalities <- fatalities[1:10, ]
print(MaxFatalities)

injuries <- aggregate(INJURIES ~ CLEANNAME, data = cleandata, FUN = sum)
injuries <- injuries[order(injuries$INJURIES, decreasing = TRUE), ]
# 10 most harmful causes of injuries
MaxInjuries <- injuries[1:10, ]
print(MaxInjuries)

# Graph of Total Fatalities and Total Injuries

par(mfrow = c(1, 2), mar = c(15, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(MaxFatalities$FATALITIES, las = 3, names.arg = MaxFatalities$CLEANNAME, main = "Weather Events With\n The Top 10 Highest Fatalities", ylab = "Number of Fatalities", col = "grey")
barplot(MaxInjuries$INJURIES, las = 3, names.arg = MaxInjuries$CLEANNAME, main = "Weather Events With\n The Top 10 Highest Injuries", ylab = "Number of Injuries", col = "grey")

# Property damage
propdmg <- aggregate(PROPDMG ~ CLEANNAME, data = cleandata, FUN = sum)
propdmg <- propdmg[order(propdmg$PROPDMG, decreasing = TRUE), ]
# 5 most harmful causes of injuries
propdmgMax <- propdmg[1:10, ]
print(propdmgMax)

# Crop damage
cropdmg <- aggregate(CROPDMG ~ CLEANNAME, data = cleandata, FUN = sum)
cropdmg <- cropdmg[order(cropdmg$CROPDMG, decreasing = TRUE), ]
# 5 most harmful causes of injuries
cropdmgMax <- cropdmg[1:10, ]
print(cropdmgMax)

# Economic damage
ecodmg <- aggregate(TOTECODMG ~ CLEANNAME, data = cleandata, FUN = sum)
ecodmg <- ecodmg[order(ecodmg$TOTECODMG, decreasing = TRUE), ]
# 5 most harmful causes of property damage
ecodmgMax <- ecodmg[1:10, ]
print(ecodmgMax)


# Graph of Total Property Damages, Total Crop Damages and Total Economic Damages
par(mfrow = c(1, 3), mar = c(15, 4, 3, 2), mgp = c(3, 1, 0), cex = 0.8)
barplot(propdmgMax$PROPDMG/(10^9), las = 3, names.arg = propdmgMax$CLEANNAME, main = "Top 10 Events with\n Greatest Property Damages", ylab = "Cost of damages ($ billions)", col = "grey")
barplot(cropdmgMax$CROPDMG/(10^9), las = 3, names.arg = cropdmgMax$CLEANNAME, main = "Top 10 Events With\n Greatest Crop Damages", ylab = "Cost of damages ($ billions)", col = "grey")
barplot(ecodmgMax$TOTECODMG/(10^9), las = 3, names.arg = ecodmgMax$CLEANNAME, main = "Top 10 Events With\n Greatest Economic Damages", ylab = "Cost of damages ($ billions)", col = "grey")





