# NOAA Storm Data Population Health and Economic Impact Analysis
David Kochar  
`r Sys.Date()`  

#Analysis of the Population Health and Economic Impact of United States Storm Events

##Synopsis

This report uses the NOAA Storm Events database to measure the population health and economic impact of storm events in the United States. Storm Data is provided by the National Weather Service (NWS) and contain statistics on personal injuries and damage estimates. The data began as early as 1950 through to the present, updated monthly with up to a 120 day delay possible. All analysis is performed with the R programming language.

## Setup

We will first prepare the workspace environment by setting global options, and installing and loading required libraries

### Set Global Options

Establish global options for the report output


```r
list.of.packages <- c("knitr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
suppressWarnings ( suppressMessages ( library ( knitr ) ) )
knitr::opts_chunk$set(fig.width=8, fig.height=4, fig.path='figures/ReproducibleResearchCourseProject2_', echo=TRUE, warning=FALSE, message=FALSE)
```

### Prepare Workspace and Load Libraries

Clear any existing variables from the workspace, set the working directory, and install required libraries if neccessary


```r
#Clear variables
rm ( list = ls ( all = TRUE ) )
#Get and set working directory
setwd ( getwd ( ) )
#Check installed status of requried packages, and install if necessary
list.of.packages <- c("dplyr", "ggplot2", "scales", "kableExtra")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
suppressWarnings ( suppressMessages ( library ( dplyr ) ) )
suppressWarnings ( suppressMessages ( library ( ggplot2 ) ) )
suppressWarnings ( suppressMessages ( library ( scales ) ) )
suppressWarnings ( suppressMessages ( library ( kableExtra ) ) )
```

## Data Processing

Our data set will be obtained via downloading a csv from the URL in the code chunk below, and then loading the csv as a data frame.


```r
#Clear variables
rm ( list = ls ( all = TRUE ) )
#Download csv
download.file ( "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", "StormData.csv.bz2" )
#Create data frame from downloaded csv
StormData <- as.data.frame ( read.csv ( file = "StormData.csv.bz2", header = TRUE, sep = "," ) )
```

## Data Transformation

In order to measure a descriptive monetary economic impact of the storm events, dollar figures must be calculated. To calculate the dollar figures for the economic impact, the damage base figures, PROPDMG & CROPDMG, will be multiplied by a numeric derivation of their respective multipliers, PROPDMGEXP & CROPDMGEXP (H = a multiplier of 100, K = a multiplier of 1000, etc.)


```r
#Create derived dollar amounts from the Property and Crop EXP-based variables
StormData <- StormData %>%
  mutate (
  CashPropertyDamage = case_when (
  PROPDMGEXP == "K" ~ 1000,
  PROPDMGEXP == "M" ~ 1000000,
  PROPDMGEXP == ""  ~ 1,
  PROPDMGEXP == "B" ~ 1000000000,
  PROPDMGEXP == "m" ~ 1000000,
  PROPDMGEXP == "0" ~ 1,
  PROPDMGEXP == "5" ~ 100000,
  PROPDMGEXP == "6" ~ 1000000,
  PROPDMGEXP == "4" ~ 10000,
  PROPDMGEXP == "2" ~ 100,
  PROPDMGEXP == "3" ~ 1000,
  PROPDMGEXP == "h" ~ 100,
  PROPDMGEXP == "7" ~ 10000000,
  PROPDMGEXP == "H" ~ 100,
  PROPDMGEXP == "1" ~ 10,
  PROPDMGEXP == "8" ~ 100000000,
  PROPDMGEXP == "+" ~ 0,
  PROPDMGEXP == "-" ~ 0,
  PROPDMGEXP == "?" ~ 0
  ),
  CashCropDamage = case_when (
  CROPDMGEXP == "K" ~ 1000,
  CROPDMGEXP == "M" ~ 1000000,
  CROPDMGEXP == ""  ~ 1,
  CROPDMGEXP == "B" ~ 1000000000,
  CROPDMGEXP == "m" ~ 1000000,
  CROPDMGEXP == "0" ~ 1,
  CROPDMGEXP == "5" ~ 100000,
  CROPDMGEXP == "6" ~ 1000000,
  CROPDMGEXP == "4" ~ 10000,
  CROPDMGEXP == "2" ~ 100,
  CROPDMGEXP == "3" ~ 1000,
  CROPDMGEXP == "h" ~ 100,
  CROPDMGEXP == "7" ~ 10000000,
  CROPDMGEXP == "H" ~ 100,
  CROPDMGEXP == "1" ~ 10,
  CROPDMGEXP == "8" ~ 100000000,
  CROPDMGEXP == "+" ~ 0,
  CROPDMGEXP == "-" ~ 0,
  CROPDMGEXP == "?" ~ 0
  )
  )
```

## Results

###Population Health Impact of Storm Events

Let's determine the most impactful Storm events to Population Health. The most impactful events will be the top 10 in terms of total injuries and fatalities.

####Subset the data into respective injury and fatality aggregations

We will create two aggregated subsets:

* Top 10 Events by Total Injuries
* Top 10 Events by Total Fatalities


```r
#Sum injuries and fatalities for each event type
StormDataEventPopHealth <- StormData %>%
  select (EVTYPE, INJURIES, FATALITIES) %>%
  group_by (EVTYPE) %>%
  summarise(
  TotalInjuries = sum (INJURIES, na.rm = TRUE),
  TotalFatalities = sum (FATALITIES, na.rm = TRUE)
  )

#Create a set of Top 10 Events by Total Injuries
StormDataEventTotalInjuries <- StormDataEventPopHealth %>%
                               select (EVTYPE, TotalInjuries) %>%
                               top_n(10, TotalInjuries)

#Create a set of Top 10 Events by Total Fatalities
StormDataEventTotalFatalities <- StormDataEventPopHealth %>%
                                 select (EVTYPE, TotalFatalities) %>%
                                 top_n(10, TotalFatalities)
```

#### Plot the results

We will create two bar chart visualizations, one for Top 10 Injuries, and one for Top 10 Fatalities


```r
#Plot the Top 10 Injuries
ggplot(StormDataEventTotalInjuries,
aes(x = reorder(EVTYPE,-TotalInjuries), y = TotalInjuries)) +
geom_bar(stat = "identity",
fill = "blue",
colour = "blue") +
labs(title = "Total Injuries by Storm Event Type", x = "Storm Event", y = "Total Injuries") +
scale_y_continuous(limits = c(0, 100000)) +
theme(
axis.text.x = element_text(angle = 90, hjust = 1),
panel.grid.major =     element_blank(),
panel.grid.minor = element_blank(),
panel.background = element_blank(),
axis.line = element_line(colour = "black")
)
```

![](figures/ReproducibleResearchCourseProject2_plot-tophealthresults-1.png)<!-- -->

```r
#Plot the Top 10 Fatalities
ggplot(StormDataEventTotalFatalities,
aes(x = reorder(EVTYPE,-TotalFatalities), y = TotalFatalities)) +
geom_bar(stat = "identity",
fill = "blue",
colour = "blue") +
labs(title = "Total Fatalities by Storm Event Type", x = "Storm Event", y = "Total Fatalities") +
scale_y_continuous(limits = c(0, 6000)) +
theme(
axis.text.x = element_text(angle = 90, hjust = 1),
panel.grid.major =     element_blank(),
panel.grid.minor = element_blank(),
panel.background = element_blank(),
axis.line = element_line(colour = "black")
)
```

![](figures/ReproducibleResearchCourseProject2_plot-tophealthresults-2.png)<!-- -->


For both Total Injuries and Total Fatalities, Tornadoes are the most impactful. This is not very surprising as tornadoes are the most dangerous type of storm event.

###Economic Impact of Storm Events

Let's determine the most impactful Storm events to the economy. The most impactful events will be the top 10 in terms of total property plus total crop dollar damage.

####Subset the data into property plus crop aggregations

We will create an aggregated subset of total propety plust total crop dollar damage


```r
#Subset the data into Total Economic Dollar Impact by Event
StormDataEventEconomic <- StormData %>%
  select (EVTYPE, CashPropertyDamage, CashCropDamage) %>%
  group_by (EVTYPE) %>%
  summarise(TotalDamage = sum (CashPropertyDamage, na.rm = TRUE) + sum (CashCropDamage, na.rm = TRUE)) %>%
  top_n(10, TotalDamage)
```

#### Plot the results

We will create a bar chart visualizations for the Top 10 Events in terms of Total Economic Impact ( Property plus Crop Damage)


```r
#Plot the Top 10 Events for Property Damage
ggplot(StormDataEventEconomic,
aes(x = reorder(EVTYPE,-TotalDamage), y = TotalDamage)) +
geom_bar(stat = "identity",
fill = "blue",
colour = "blue") +
labs(title = "Total Property and Crop Damage in Dollars by Storm Event Type", x = "Storm Event", y = "Total Dollars") +
scale_y_continuous(labels = comma, limits = c(0, 15000000000)) +
theme(
axis.text.x = element_text(angle = 90, hjust = 1),
panel.grid.major =     element_blank(),
panel.grid.minor = element_blank(),
panel.background = element_blank(),
axis.line = element_line(colour = "black")
)
```

![](figures/ReproducibleResearchCourseProject2_plot-topeconomicresults-1.png)<!-- -->


Hurricanes/Typhoons were the most damaging in terms of economic impact. This is also not very surprising, as hurricanes are the most powerful storm types, and thus have the most damage potential.
