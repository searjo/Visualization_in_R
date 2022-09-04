#Import Data
df <- read.csv("C:/Users/jcsea/OneDrive/Desktop/BGSE Documents/Networks/ppdata_lite.csv")

#install.packages("plotly")
#install.packages("dplyr")
#install.packages("plyr")
#install.packages('geojsonlint')

#Get logprice for easier visualization 
df$logprice <- log(df$price, 10)


library("knitr")
library("plyr")
library("dplyr")
library("ggplot2")
library("plotly")
library("geojsonio")
library("geojson")
library("sf")
library("stringr")
library("leaflet")
library("rgdal")
library('raster')
library('maps')
library('Hmisc')

#Counties are not boroughs clearly
unique(df$county)
#Get those in Greater London
london <-df[df$county=="GREATER LONDON",]

#Double check to see if output is as intended
unique(london$district)

#Sort alphabetically
london <- london[order(london$district), ]


#Here is the simplest version of a boxplot for each district. It can clearly be improved.
fig <- plot_ly(london, y = ~price, color = ~district, type = "box")
fig


#One method is to make the boroughs horizontal so you can read them.
#I can also look at log price to get a more condensed plot
fig2 <- plot_ly(london, x = ~logprice, y = ~district, jitter = 0.5, pointpos = 0, type = "box", boxpoints='suspectedoutliers')
fig2 

#Violin plots might be more useful here, so I split the data evenly into 3 groups

london_ae = {london %>% filter(str_detect(district, '^[A-E]'))} 
london_gk = {london %>% filter(str_detect(district, '^[G-K]'))}
london_lz = {london %>% filter(str_detect(district, '^[L-Z]'))}

fig_ae <- plot_ly(london_ae, x = ~logprice, jitter = 0.5, pointpos = 0, color = ~district, type = "violin", boxpoints='suspectedoutliers', showlegend = FALSE)
fig_gk <- plot_ly(london_gk, x = ~logprice, jitter = 0.5, pointpos = 0, color = ~district, type = "violin", boxpoints='suspectedoutliers', showlegend = FALSE)
fig_lz <- plot_ly(london_lz, x = ~logprice, jitter = 0.5, pointpos = 0, color = ~district, type = "violin", boxpoints='suspectedoutliers', showlegend = FALSE)

#I can inspect each one individually for a closer look
fig_ae
fig_gk
fig_lz

#Or combine them into one plot - note when exporting I will increase the height
subplot(fig_ae, fig_gk, fig_lz) %>% 
  layout(title = list(text = "Districts by Log Price"),
         plot_bgcolor='#e5ecf6', 
         xaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff'), 
         yaxis = list( 
           zerolinecolor = '#ffff', 
           zerolinewidth = 2, 
           gridcolor = 'ffff')) 


pd <- ggplot(london) +
  geom_smooth(aes(x=date_of_transfer, y=logprice, color=district))+
  theme(text=element_text(size=8))+
  theme_update(plot.title = element_text(hjust = 0.5))+
  labs(title="UK Housing Prices Over Time",
       x ="Date", y = "Price")+
  geom_line(alpha = 0, size = .1)


ukpostcodes <- read.csv("C:/Users/jcsea/OneDrive/Desktop/BGSE Documents/Networks/ukpostcodes.csv")
data <- merge(pp_data_litest, `ukpostcodes.(1)`, by = "postcode")

data$logprice <- log(data$price, 10)

data <- data %>%                             
  group_by(postcode) %>% 
  mutate(min = min(price), mean = mean(price), median = median(price), max = max(price))

#Get high medium and low categories by splitting evenly into 3 groups.
#Used Hmisc library for this
data$zip_price_cat <- as.numeric(cut2(data$median, g=3))

#From starter notebook
coordinates(data) <- c("latitude", "longitude")
writeOGR(data, "test_geojson.gson", layer = "data", driver = "GeoJSON")

#Filter to only necessary columns
geo <- dplyr::select(data,c(postcode, zip_price_cat, price, logprice,latitude,longitude))

#Get a Spatial Dataframe
sdf <- SpatialPointsDataFrame(data[,19:20], data)

plot(sdf, main="Map of Plot Locations")

#

#Get world map
worldmap = map_data('world')

#Tweak to just get map of UK
#
ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, group = group), 
               fill = 'gray90', color = 'black') + 
  coord_fixed(ratio = 1.3, xlim = c(-10,3), ylim = c(50, 59)) + 
  theme_void() + 
  geom_point(data = data, 
             aes(x = longitude, 
                 y = latitude, color = zip_price_cat), alpha = .01) + 
  scale_size_area(max_size = 8) + 
  scale_color_viridis_c() + 
  theme(legend.position = 'top') + 
  theme(title = element_text(size = 12))

gj2<-geojson(
  sdf,
  lat = latitude,
  lon = longitude,
  group = postcode)
  #geometry = "point")#,
  #type = "FeatureCollection",
  #convert_wgs84 = FALSE)#,
  #crs = NULL,
  #precision = NULL,

uk <- geojsonio::geojson_read(gj, what = "sp")
df <- pp_data_litest
#Leaflet
data %>% 
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(label = data$postcode, 
             popup = ifelse(data$`zip_price_cat`==3,
                            "High Value", # Value if True
                            "Mid-Low Value")) # Val False

pal <- colorFactor(palette = 'YlOrBr', data$median)

leaflet(data) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(color = ~median, fillOpacity = 1, radius=0.001) %>%
  addLegend(pal = pal, values = ~median, opacity = 1)

df$propety_type[df$property_type == "D"] <- "Detached"
df$propety_type[df$property_type == "S"] <- "Semi-Detached"
df$propety_type[df$property_type == "T"] <- "Terraced"
df$propety_type[df$property_type == "F"] <- "Flats/Maisonettes"
df$propety_type[df$property_type == "O"] <- "Other"


#Trim date of transfer oclumn to remove minutes/hours
df$date_of_transfer <- strtrim(df$date_of_transfer,10)
#Turn into dates instead of character
df$date_of_transfer <- as.Date(df$date_of_transfer, "%Y-%m-%d")
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


df$Season[df$month == 3 || df$month == 4 || df$month == 5 ] <- "Spring"
df$Season[df$month == 6 || df$month == 7|| df$month == 8 ] <- "Summer"
df$Season[df$month == 9 || df$month == 10 || df$month == 11 ] <- "Autumn"


uk <- rgdal::readOGR("C:\Users\jcsea\OneDrive\Documents\myfile.geojson") 
