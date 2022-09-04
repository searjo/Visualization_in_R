---
title: "Data Visualization Final"
author: "John Searight"
date: '2022-04-06'
output:
  pdf_document: default
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## Intro: 
Below is UK housing data. I demonstrate methods for visualization using the libraries below

```{r, include=TRUE}
library("knitr")
library("plyr")
library("dplyr")
library("ggplot2")
library("plotly")
library("stringr")
library("geojsonio")
library('jsonify')
library('jsonlite')
library('jsonvalidate')
library("geojsonio")
library("geojson")
library("sf")
library("stringr")
library("leaflet")
library("rgdal")
library('raster')
library('maps')
library('Hmisc')
library('purrr')
```

```{r}
ukpostcodes <- read.csv("C:/Users/jcsea/Downloads/ukpostcodes.csv")

df <- read.csv('C:/Users/jcsea/OneDrive/Desktop/BGSE Documents/ppdata_liter2.csv')

#Inspect
head(df)

#Get dimensions
dim(df)

#Create log price
df$logprice <- log(df$price, 10)
```
Task A:
First look at counties to see what constitutes London

```{r}
unique(df$county)

#Create london df feat only london
london <-df[df$county=="GREATER LONDON",]

#Inspect districts (boroughs)
unique(london$district)

#Sort alphabetically
london <- london[order(london$district), ]
```
Now I use Plotly. Here are the simplest box plots featuring price and log prices

```{r}
fig <- plot_ly(london, y = ~price, color = ~district, type = "box")
fig

fig2 <- plot_ly(london, x = ~logprice, y = ~district, jitter = 0.5, pointpos = 0, type = "box", boxpoints='suspectedoutliers')
fig2
```
The above plots districts on the vertical access and gets rid of the legend for redundancy. I also removed the colors for minimilast feel, but it is still interactive and you can see the name of the district when hovering over. Notice the scale is now log price so that the outliers are minimized.Kensington and Chelsea continue to stand out.

It would be easier to see wider plots, so I will visualize subplots and violin plots
I split so there are 10 or 11 observations in each, though could have also
split it to have roughly equal number of total observations in each
```{r}
#Violin plots might be more useful here, so I split the data evenly into 3 groups

london_ae = {london %>% filter(str_detect(district, '^[A-E]'))} 
london_gk = {london %>% filter(str_detect(district, '^[G-K]'))}
london_lz = {london %>% filter(str_detect(district, '^[L-Z]'))}

fig_ae <- plot_ly(london_ae, x = ~logprice, jitter = 0.5, pointpos = 0, color = ~district, type = "violin", points='suspectedoutliers', showlegend = FALSE)
fig_gk <- plot_ly(london_gk, x = ~logprice, jitter = 0.5, pointpos = 0, color = ~district, type = "violin", points='suspectedoutliers', showlegend = FALSE)
fig_lz <- plot_ly(london_lz, x = ~logprice, jitter = 0.5, pointpos = 0, color = ~district, type = "violin", points='suspectedoutliers', showlegend = FALSE)

#I can inspect each one individually for a closer look
fig_ae
fig_gk
fig_lz
```
A few observations include that Kensington and Chelsea seem to have the highest home prices, while Greenwich, Hackney, and Fulham in particular have some outliers. There is one house in Hackney that is listed at zero and, which is peculiar, but perhaps it was given away.

#A2

Floor level is not obviously indicated in the dataframe, however SAON does indicate floor level in some features.

So the entire dataset could not be used to estimate the relationship between price of flats and floor level, but those that have floors indicated can be used.

```{r}
#Filter data to only include those with floors listed
floors_df <- filter(df, grepl("FLOOR", SAON, ignore.case = TRUE))

#Look at unique floor levels
#unique(floors_df$SAON)
```
There are a lot of ways I could categorize, I walk through the process below.
```{r}
#Create new column with Unknown as the bast
floors_df$floor <- "UNKNOWN"

#Use grep to auto fill in columns based on substrings
#I commented out floors 7-10 as they 
floors_df$floor[grep("BASEMENT", floors_df$SAON)] <- "BASEMENT"
floors_df$floor[grepl("GROUND", floors_df$SAON)]<- 0
floors_df$floor[grepl("GARDEN", floors_df$SAON)]<- 0
floors_df$floor[grepl("FIRST", floors_df$SAON)]<- 1
floors_df$floor[grepl("1ST", floors_df$SAON)]<- 1
floors_df$floor[grepl("MEZZANINE", floors_df$SAON)]<- 'MEZZANINE'
floors_df$floor[grepl("SECOND", floors_df$SAON)]<- 2
floors_df$floor[grepl("2ND", floors_df$SAON)]<- 2
floors_df$floor[grepl("THIRD", floors_df$SAON)]<- 3
floors_df$floor[grepl("3RD", floors_df$SAON)]<- 3
floors_df$floor[grepl("FOURTH", floors_df$SAON)]<- 4
floors_df$floor[grepl("4TH", floors_df$SAON)]<- 4
floors_df$floor[grepl("FIFTH", floors_df$SAON)]<- 5
floors_df$floor[grepl("5TH", floors_df$SAON)]<- 5
floors_df$floor[grepl("SIXTH", floors_df$SAON)]<- 6
floors_df$floor[grepl("6TH", floors_df$SAON)]<- 6
floors_df$floor[grepl("SEVENTH", floors_df$SAON)]<- 7
floors_df$floor[grepl("7TH", floors_df$SAON)]<- 7
floors_df$floor[grepl("EIGHTH", floors_df$SAON)]<- 8
floors_df$floor[grepl("8TH", floors_df$SAON)]<- 8
floors_df$floor[grepl("NINTH", floors_df$SAON)]<- 9
floors_df$floor[grepl("9TH", floors_df$SAON)]<- 9
floors_df$floor[grepl("TENTH", floors_df$SAON)]<- 'ABOVE 9'
floors_df$floor[grepl("10TH", floors_df$SAON)]<- 'ABOVE 9'
floors_df$floor[grepl("ELEVENTH", floors_df$SAON)]<- 'ABOVE 9'
floors_df$floor[grepl("11TH", floors_df$SAON)]<- 'ABOVE 9'
floors_df$floor[grepl("TOP", floors_df$SAON)]<- 'TOP'
floors_df$floor[grepl("PENTHOUSE", floors_df$SAON)]<- 'PENTHOUSE'
floors_df$floor[grepl("UPPER", floors_df$SAON)]<- 'UPPER'
floors_df$floor[grepl("HALL", floors_df$SAON)]<- 'HALL'
floors_df$floor[grepl("AND", floors_df$SAON)]<- 'MULTI_FLOORS'
```

```{r}
#First we inspect frequencies of each observation
ggplot(floors_df, aes(x=floor))+
geom_bar()+
theme_minimal() +
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle("Floor Counts")+
        geom_text(aes(label = ..count..), stat = "count", position = position_dodge(width = 0.9), vjust=-0.40)+
        xlab("Number of Floors") + ylab("Count of Homes")
```
Clearly I have an error in the above and need bin the data further.

```{r}
floors_df$floor[grep("BASEMENT", floors_df$SAON)] <- 0 #Given so few baseement observations I grouped with ground
floors_df$floor[grepl("GROUND", floors_df$SAON)]<- 0
floors_df$floor[grepl("GARDEN", floors_df$SAON)]<- 0
floors_df$floor[grepl("THIRD", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("3RD", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("FOURTH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("4TH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("FIFTH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("5TH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("SIXTH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("6TH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("SEVENTH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("7TH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("EIGHTH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("8TH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("NINTH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("9TH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("TENTH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("10TH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("ELEVENTH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("11TH", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("TOP", floors_df$SAON)]<- 'TOP'
floors_df$floor[grepl("PENTHOUSE", floors_df$SAON)]<- 'TOP'
floors_df$floor[grepl("UPPER", floors_df$SAON)]<- '3 OR ABOVE'
floors_df$floor[grepl("HALL", floors_df$SAON)]<- '0'
floors_df$floor[grepl("AND", floors_df$SAON)]<- 'MULTI_FLOORS'
floors_df$floor[grepl("MEZZANINE", floors_df$SAON)]<- 2


#Drop any that couldn't be characterized
floors_df <- floors_df[!grepl("UNKNOWN", floors_df$floor),]
```
Now inspect new frequencies
```{r}
#First we inspect frequencies of each observation
ggplot(floors_df, aes(x=floor))+
geom_bar()+
theme_minimal() +
 theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        ggtitle("Floor Counts")+
        geom_text(aes(label = ..count..), stat = "count", position = position_dodge(width = 0.9), vjust=-0.40)+
        xlab("Number of Floors") + ylab("Count of Homes")
```

```{r}
#Now I plot with median values to account for outliers
options(scipen=1000) 

ggplot(floors_df, aes(x=floor, y=price))+
  geom_bar(stat="summary", fun ="median")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Median Prices by Floor of Building in the UK")+
  xlab("Number of Floors") + ylab("Median Price of Home")
```
Now that I have fewer categories, I can inspect the distribution of plots using a custom function.

The below binning function I got from the tidyverse website, and is "particularly useful when faceting along variables with different ranges".
```{r}
ggplot(floors_df, aes(logprice, color=floor)) +
  #facet_grid(~floor, scales = 'fixed') +
  geom_histogram(binwidth = function(x) 2 * IQR(x) / (length(x)^(1/3)))+
  ggtitle("Log Prices by Floor of Building in the UK")+
  xlab("Log Price of Home") + ylab("Number of Observations")
```
I like the above plot because it's aesthetically pleasing and indicates that multi-floor units tend to have a higher price, but you can see the progression from 0, 1, 2, to 3 or above and multi floors.

I also tried a facet wrap but found the above to be a more efficient way to inspect the floor level and housing prices.

# Task B
```{r}
head(ukpostcodes)
df <- read.csv('C:/Users/jcsea/OneDrive/Desktop/BGSE Documents/ppdata_liter2.csv')

data <- merge(df, ukpostcodes, by = "postcode")
dim(data)

#Now mutate to create new columns
data <- data %>%                             
  group_by(postcode) %>% 
  #Mutate function creates new variables
  mutate(min = min(price), mean = mean(price), median = median(price), max = max(price))
```
Get high medium and low categories by splitting evenly into 3 groups.
Used Hmisc library for this

```{r}
data$Price_Category <- as.numeric(cut2(data$median, g=3))
```
### Method 1
I created a spatial dataframe and plotted all of the houses based on price.

I used several sources to guide me in tying to make the geojson, but this one that looks at UK geospatial data was the one I followed very closely.

https://bookdown.org/yann_ryan/r-for-newspaper-data/mapping-with-r-geocode-and-map-the-british-librarys-newspaper-collection.html

```{r}
#To get a Spatial Dataframe I can do the below. #18, 19 are locations of latitude and longitude
sdf <- SpatialPointsDataFrame(data[,18:19], data)

#plot(sdf, main="Map of House Locations")

#

#Get world map from maps library
worldmap = map_data('world')

#Tweak to just get map of UK
ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray90', color = 'black') + 
  coord_fixed(ratio = 1.3, xlim = c(-10,3), ylim = c(50, 59)) + 
  theme_void() + 
  geom_point(data = data, 
             aes(x = longitude, 
                 y = latitude, color = Price_Category), alpha = .7) + 
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme(legend.position = 'top') + 
  theme(title = element_text(size = 12))
```
Here I improve upon this
```{r}
agg <- data %>%
  group_by(district) %>%
  summarise_at(vars(Price_Category), list(Wealth_Level = mean))

agg2 <- merge(data, agg, by = "district")

ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray90', color = 'black') + 
  coord_fixed(ratio = 1.3, xlim = c(-10,3), ylim = c(50, 59)) + 
  theme_void() + 
  geom_point(data = agg2, 
             aes(x = longitude, 
                 y = latitude, color = Wealth_Level), alpha = .7) + 
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme(legend.position = 'top') + 
  theme(title = element_text(size = 12))
```



To get the geojson I can use the code from the starter notebook. I ended up not needing it.
```{r}
#From starter notebook
#coordinates(data) <- c("latitude", "longitude")
#writeOGR(data, "test_geojson.geojson", layer = "data", driver = "GeoJSON")
```


## B3
I don't think mean would have improved the plot, because as we saw some of the housing prices are enormous outliers, over 100 milllion pounds. This would heavily skew the mean housing prices.

We could remove the outliers and then plot housing prices, or plot log house prices as I have done elsewhere in this notebook, to plot it. Given splitting it into 3 equal categories will produce roughly the same map regardless of outliers, I could also use 10 categories here to see how the map looks different.
```{r}
data$Price_Category2 <- as.numeric(cut2(data$mean, g=10))

ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray90', color = 'black') + 
  coord_fixed(ratio = 1.3, xlim = c(-10,3), ylim = c(50, 59)) + 
  theme_void() + 
  geom_point(data = data, 
             aes(x = longitude, 
                 y = latitude, color = Price_Category2), alpha = .7) + 
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme(legend.position = 'top') + 
  theme(title = element_text(size = 12))
```
```{r}
agg3 <- data %>%
  group_by(district) %>%
  summarise_at(vars(Price_Category), list(Wealth_Level = median))

agg4 <- merge(data, agg3, by = "district")

ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray90', color = 'black') + 
  coord_fixed(ratio = 1.3, xlim = c(-10,3), ylim = c(50, 59)) + 
  theme_void() + 
  geom_point(data = agg2, 
             aes(x = longitude, 
                 y = latitude, color = Wealth_Level), alpha = .7) + 
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme(legend.position = 'top') + 
  theme(title = element_text(size = 12))
```



You can see the wealthiest areas are around London and the poorest in dark blue.


# Task C
#Create new df only consisting of 2015 (recognize this could also be done within ggplot using filter function, however I wanted to inspect the DF too)


```{r}
df <- read.csv('C:/Users/jcsea/OneDrive/Desktop/BGSE Documents/ppdata_liter2.csv')

df$logprice <- log(df$price, 10)
```
First give labels to the property types
```{r}
df$property_type[df$property_type == "D"] <- "Detached"
df$property_type[df$property_type == "S"] <- "Semi-Detached"
df$property_type[df$property_type == "T"] <- "Terraced"
df$property_type[df$property_type == "F"] <- "Flats/Maisonettes"
df$property_type[df$property_type == "O"] <- "Other"
```

Below I do some date transformations. 
```{r}
#Trim date of transfer column to remove minutes/hours
df$date_of_transfer <- strtrim(df$date_of_transfer,10)

#Turn into dates instead of character
df$date <- as.Date(df$date_of_transfer, "%d/%m/%Y")

#Drop leap day in 1996 that caused null values
#df <- na.omit(df)

#Extract month in case needed
df$month <- as.numeric(format(df$date, "%m"))

#Create season
df$Season[df$month == 12] <- "Winter"
df$Season[df$month == 1] <- "Winter"
df$Season[df$month == 2] <- "Winter"
df$Season[df$month == 3] <- "Spring"
df$Season[df$month == 4] <- "Spring"
df$Season[df$month == 5] <- "Spring"
df$Season[df$month == 6] <- "Summer"
df$Season[df$month == 7] <- "Summer"
df$Season[df$month == 8] <- "Summer"
df$Season[df$month == 9] <- "Autumn"
df$Season[df$month == 10] <- "Autumn"
df$Season[df$month == 11] <- "Autumn"

#Get only 2015 observations. Need to use dplyr:: to avoid a reoccurring error that the object was not found
dC <- dplyr::filter(df, grepl("2015", df$date_of_transfer))

#Define month labels for later
monthlabels <- list('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
```
Create plot with smooth line
```{r}
#Create plot with smooth line
ggplot(dC) +
  geom_smooth(aes(x=date, y=price, color=old_new))+
  theme(text=element_text(size=8))+
  theme_update(plot.title = element_text(hjust = 0.5))+
  ylim(0,1000000)  +
  labs(title="UK Housing Prices Over Time \n 2015",
        x ="Date", y = "Price")+
  geom_line(aes(date, price), alpha = 0.3, size = .1) #Used alpha/size to make thel ines very transparent and small
```



There is not much of a price change over time. It increases slightly. If I decrease the y upper limit we can see a moderate trend
```{r}
#Create plot with y limit
ggplot(dC) +
  geom_smooth(aes(x=date, y=price, color=old_new))+
  theme(text=element_text(size=8))+
  theme_update(plot.title = element_text(hjust = 0.5))+
  ylim(0,150000)  +
  labs(title="UK Housing Prices Over Time \n 2015",
        x ="Date", y = "Price")+
  geom_line(aes(date, price), alpha = 0.3, size = .1)
```


As you can see in the plot above for houses under 150,000, for new houses the prices have increased slightly more than for old houses, which have stayed relatively flat with a slight upward trend.

## C2
Is there a significant relationship between the price of a property and the time of year it is sold? Does this vary with type of property?
```{r}
options(scipen=10000) 
ggplot(df) +
  geom_smooth(aes(x=date, y=price, color=property_type))+
  theme(text=element_text(size=8))+
  theme_update(plot.title = element_text(hjust = 0.5))+
  ylim(0,1500000)  +
  labs(title="UK Housing Prices Over Time",
        x ="Date", y = "Price")+
  geom_line(aes(date, price), alpha = 0.05, size = .05)
```



It looks like the 'Other' category of home does not align with the categorized types of home. It also just emerged in 2006 - certainly worth looking into what this could be, as the price has skyrocketed. You can also see the impact of the housing crisis over time, where it flattens out and dips after 2007. To look at just the months (irrespective of the year) I can do the following:
```{r}
options(scipen=1000) 
ggplot(df, aes(x=month, y=price, fill=Season))+
  geom_bar(stat="summary", fun ="median")+
  theme_minimal() +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) +
  ggtitle("Median Prices by Month in the UK")+
  xlab("Month") + ylab("Median Price of Home") + 
  scale_x_continuous(breaks=min(df$month):max(df$month), expand=c(0,0.1), labels=monthlabels) 
```



It looks like over time median prices tend to peak in August and September.
```{r}
options(scipen=1000) 
ggplot(df, aes(x=Season, y=price, fill=Season))+
  geom_bar(stat="summary", fun ="median")+
  theme_minimal() +
  ggtitle("Median Price by Season in the UK")+
  xlab("Season") + ylab("Median Price of Home")
```


It does appear that there is a trend for higher home prices in the summer and autumn months. We can use a box plot to visualize more clearly and facet by property type.

```{r}
options(scipen=1000) 
ggplot(df, aes(x=Season, y=price, fill=Season))+
  geom_bar(stat="summary", fun ="mean")+
  theme_minimal() +
  ggtitle("Mean Price by Season in the UK")+
  xlab("Season") + ylab("Mean Price of Home")
```


The mean seems to show a similar trend, with Summer and Autumn being higher priced seasons.
```{r}
ggplot(df, aes(y=logprice, x=Season, fill=Season)) + 
    geom_violin() +
    theme_minimal() + 
    labs(x = "Month", y = "Price")+ 
    ggtitle("House Prices by Season and Property Type") +
    #scale_x_continuous(breaks=min(df$month):max(df$month) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    #scale_y_continuous(expand=c(0,0.2))+
    facet_wrap(~property_type) 
```


The above does not seem to indicate a clear trend over time.
Below I can plot it differently with the property_types on the y axis, looking at old_new and facet_wrapping by season. 
```{r}
ggplot(df, aes(fill= old_new, x=price, y=property_type)) + 
    geom_boxplot() +
    theme_minimal() + 
    labs(x = "Month", y = "Price")+ 
    ggtitle("House Prices by Month and Property Type") +
    #scale_x_continuous(breaks=min(df$month):max(df$month), expand=c(0,0.1)) +
    # scale_y_continuous(expand=c(0,0.2))+
    #theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
    facet_wrap(~Season) + xlim(c(0, 1000000))
```


It does not appear that there is a significant trend depending on the season for each property type.
