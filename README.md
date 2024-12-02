# 418_Final_Project

## Introduction

## Data

## Methods

### Data Cleaning
The first step is to clean up any data we have collected to be ready for our analysis. We will begin by installing the necessary packages (if needed), loading them in, and setting our working directory. Our working directory is where all of our files are being stored on our device and setting it will tell R where to look for them when we call on them. A package only needs to be installed once on your device, however, packages must be opened every time you open your R project.
```
#install packers if not already done so
install.packages(“spatstat”)
install.packages(“spdep”)
install.packages(“raster”)
install.packages(“sf”)
install.packages(“st”)
install.packages(“dplyr”)
install.packages(“gstat”)
install.packages(“ggplot2”)
install.packages("spgwr")
install.packages("lubridate")

#open packages
library(spatstat)
library(spdep)
library(raster)
library(sf)
library(st)
library(dplyr)
library(gstat)
library(ggplot2)
library(spgwr)
library(lubridate)

#Setting working directory
dir <- "C:/Users/blorr/OneDrive/Documents/GEOG418/Hicks_GEOG418_Final"

setwd(dir)
```
Now we can start working with our files in our working directory. We will start with our climate data collected from the Pacific Climate Impacts Consortium (PCIC). The data I collected was the daily maximum temperature from 2004-2024 in British Columbia. The data you use can be anything that makes sense for your study as long as the data has relatively good coverage across your study area. The data from PCIC gives us a folder (in this case called “BCH”) which contains many .csv files with data from each weather station point. We want look in these .csv files, extract the data we want from each one, combine it with our metadata (also obtained from PCIC), and compile it all into one excel file to use for our analysis. We will do this by creating a ‘for loop’ in R.
```
#Data cleaning for BCH Data from PCIC
# Create an empty data frame with specified columns. You will use this to write your weather station to when you want to combine your climate variables with the coordinates contained in the metadata.
empty_data <- data.frame(Native.ID = character(), TEMP = numeric(), 
                         Longitude = numeric(), Latitude = numeric(), stringsAsFactors = FALSE)
csv_file_name <- "BC_MAX_TEMP.csv" #We are giving the file this name as an example of calculating average temperatures in BC.

# Write the empty data frame to a CSV file
write.csv(empty_data, file = csv_file_name, row.names = FALSE) #We will call this file later. 

#First, list all CSV files in the directory to make sure you are in the right folder. 
csv_files <- list.files(path = "./pcds_data/BCH", pattern = "\\.csv$", full.names = TRUE)

#Next, loop through each CSV file and perform your calculations to calculate something about this variable. Here we calculate season average temperatures (June-August). 
for (file in csv_files) {
  
  #Read each file
  daily_data <- read.csv(file, skip = 1, header = TRUE)
 
  #Adjust the date/time column so that it is usable in calculations
  daily_data$time <- lubridate::ymd_hms(daily_data$time) 
  
  #Convert your variable column (I call it MAX_TEMP here) to numeric and remove NA's
  daily_data$MAX_TEMP <- as.numeric(daily_data$MAX_TEMP) 
  daily_data <- daily_data %>%
   filter(!is.na(MAX_TEMP))
 
  #Calculate the average daily max temp for June-August
  # Filter for the months from June to August
  average_temp_june_august <- daily_data %>%
    filter(month(time) >= 6 & month(time) <= 8) %>%
    summarize(MAX_TEMP = mean(MAX_TEMP, na.rm = TRUE))  
            
  #First, extract the filename as this is the name of your weather station.
  file_name <- basename(file)
  
  #Remove the file extension
  file_name_no_ext <- sub("\\.[^.]*$", "", file_name)
  
  # Display the result
  print(file_name_no_ext)
  
  #Read the existing CSV file
  file_path <- csv_file_name
  data <- read.csv(file_path)
  
  #Round the temperature values to two decimals
  Roundedtemp <- round(average_temp_june_august,2)
  
  #Convert the weather station ID column to character
  data$Native.ID <- as.character(data$Native.ID)
  
  #Now, add your weather station and temperature values to the file
  new_values <- data.frame(Native.ID = file_name_no_ext, 
                           TEMP = Roundedtemp, 
                           stringsAsFactors = FALSE)
  data <- bind_rows(data, new_values)
  
  #Check your data to make sure that the row has been added.
  print(head(data))
  
  #Save the updated data frame back to a new CSV file
  output_file_path <- csv_file_name
  write.csv(data, file = output_file_path, row.names = FALSE)
}

#Merge the climate data for each station with the location data found in the metadata file
metadata <- read.csv("./pcds_data/final-station-metadata-by-history.csv")
climatedata <- read.csv("BC_MAX_TEMP.csv")

merged_data <- merge(metadata, climatedata, by = "Native.ID")

#Remove unwanted columns leaving only “Native.ID, Longitude, Latitude, MAX_TEMP”
merged_data <- merged_data[, -c(2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 15, 16, 17)]

#Change column names for Latitude and Longitude to remove the x
colnames(merged_data)[colnames(merged_data) %in% c("Latitude.x", "Longitude.x")] <- c("Longitude", "Latitude")

#If there are erroneous temperature values filter data to remove these.
merged_data <- merged_data[merged_data$MAX_TEMP <= 70, ]

#Write the dataset so that it  is stored
write.csv(merged_data, file = "ClimateData.csv", row.names = FALSE)

#Create a shapefile of dataset
# Read the CSV file
climate_data <- read.csv("ClimateData.csv")

# Ensure Latitude and Longitude columns are correctly formatted
# Assuming the columns are named "Latitude" and "Longitude"
climate_data <- climate_data %>%
  mutate(Latitude = as.numeric(Latitude),
         Longitude = as.numeric(Longitude))

# Create a simple feature object (sf) using Latitude and Longitude
climate_sf <- st_as_sf(climate_data, coords = c("Longitude", "Latitude"), crs = 4326)


# If your data is in a different CRS, set the correct CRS first (WGS 84 in this case)
climate_sf <- st_set_crs(climate_sf, 4326)

# Transform the sf object to UTM Zone 10N (EPSG: 26910) for BC
climate_sf_utm <- st_transform(climate_sf, crs = 3005)

# Save the transformed sf object as a new shapefile
st_write(climate_sf_utm, "climate_data_utm.shp")

# Confirmation message
print("Shapefile has been created: climate_data_utm.shp")
```
This next step is only necessary if you obtained data from MORE THAN ONE network in PCIC. Doing this may be advantageous if it results in better coverage across your study site. In this case, you will want to run the last chunk of code again but using the excel files stored in a different folder and metadata obtained from PCIC. In my case, it was called EC. Once you do this, you will have two .shp called “climate_data_utm” (for BCH) and “climate_data_utm_2” (for EC). Our last step is to combine these to create our final .shp dataset.

*IMPORTANT: If you only used one network, skip this step and continue your analysis using your “climate_data_utm” file in the place of “final_climate_data”
```
# Merge the the BCH and EC shapefiles
merged_climate <- rbind(climate_sf_utm, climate_sf_utm_2)

# Save the merged shapefile to a new file
st_write(merged_climate, "final_climate_data.shp")
```
If you'd like to map your final climate data against your study area to visually see how the data cleaning performed, you can run the following code.
```
# Load the shapefiles
final_climate_data <- st_read("final_climate_data.shp")
BC <- st_read("BC.shp") #study area shapefile

# Transform the CRS from EPSG:3347 to EPSG:3005
prov_polygon <- st_transform(BC, crs = 3005)

# Create the map
ggplot() +
  geom_sf(data = prov_polygon, fill = "lightgrey", color = "black") +
  # Map the TEMP variable to color
  geom_sf(data = final_climate_data, aes(color = MAX_TEMP), size = 2) + 
  scale_color_gradient(low = "blue", high = "red") + # Adjust color gradient as needed
  theme_minimal() +
  labs(title = "Map of Max Average Temp Climate Data Points",
       subtitle = "in British Columbia 2004-2024 June-August",
       x = "Longitude",  # Use Longitude for x-axis
       y = "Latitude",   # Use Latitude for y-axis
       color = "Temperature (°C)") + # Label for color legend
  theme(legend.position = "bottom")
```
Here’s how mine turned out:

![Climate Data Points Map](https://github.com/user-attachments/assets/5b33a1ca-148d-416a-aec6-d089c11a2278)

Now we can move into cleaning up our fire data which should be a bit quicker. Here, we are adjusting the format of our data, clipping it to our dates and boundary, and removing any NA values that may cause errors in calculations.
```
# Load your point data (make sure to adjust the path). Here we use a wildfire dataset from the BC Data Catoluge called H_FIRE_PNT_point and our BC Boundary file.
H_FIRE_PNT_point <- st_read("H_FIRE_PNT_point.shp")

# Convert the 'FIRE_DATE' column to POSIXct
H_FIRE_PNT_point$IGN_DATE <- ymd_hms(as.character(H_FIRE_PNT_point$IGN_DATE)) 

# Remove rows with NA values in the FIRE_DATE column
H_FIRE_PNT_point <- H_FIRE_PNT_point %>%
  filter(!is.na(IGN_DATE))

# Subset the data for years 2004-2024 and months June-August
subset_fire_data <- H_FIRE_PNT_point %>%
  filter(year(IGN_DATE) >= 2004 & year(IGN_DATE) <= 2024,  # Filter for years 2004-2024
         month(IGN_DATE) %in% 6:8)  # Filter for June, July, August

# Remove rows where SIZE_HA is NA
subset_fire_data <- subset_fire_data %>%
  filter(!is.na(SIZE_HA))

# Clip fire data to the BC boundary
final_fire_data <- st_intersection(subset_fire_data, prov_polygon)

# Check for rows with NA values in any attribute column
na_rows <- final_fire_data[!complete.cases(final_fire_data), ]
```
### Examining Wildfire Descriptive Satistics
It’s important to be able to have some general statements that we can use to describe our climate driven event. We can gather these by performing descriptive and spatial descriptive statistics in our cleaned fire data. Here, we will be looking at the mean, median, and mean centre of our dataset.
```
#Descriptive stats on fire data: mean and median
mean_fire_size <- mean(final_fire_data$SIZE_HA)
median_fire_size <- median(final_fire_data$SIZE_HA) #most frequent fire size

#Spatial descriptive stats on fire data: mean center
# Calculate the mean center (centroid) of the final fire data
coords <- st_coordinates(final_fire_data)
mean_coords <- colMeans(coords)

# Create a point representing the mean center
mean_center <- st_sfc(st_point(c(mean_coords[1], mean_coords[2])), crs = st_crs(final_fire_data))

# Plot the fire data and add the mean center
ggplot() +
  # Plot the BC boundary
  geom_sf(data = prov_polygon, fill = "lightblue", color = "black") +
  
  # Plot the clipped fire data points
  geom_sf(data = final_fire_data, color = "red", size = 1, alpha = 0.5) +
  
  # Add the mean center (as a large blue point)
  geom_sf(data = mean_center, color = "blue", size = 3, shape = 21, fill = "yellow") +
  
  # Add title and labels
  labs(title = "Fire Data in BC 2004-2024 with Mean Center",
       subtitle = "(Mean center of fire points in BC)") +
  theme_minimal()
```



### Evaluating Spatial Distribution of Wildfires 


### Creating a Temperature Surface
Since our climate data is currently in point form across the province, we want to create a temperature surface in order to carry on with our analysis. We can do this by performing Inverse Distance Weighting (IDW). This local technique uses the data at each point to estimate values for the areas without point data (esri, n.d.). This is done by utilizing Tobler’s Law of geography which states that “Everything is related to everything else, but near things are more related than distant things” (Tobler, 1970). Therefore, IDW estimates unknown values by looking at nearby points in a ‘neighbourhood’ and calculating the most likely/appropriate value.

IDW uses the following formula:

$$
\hat{Z}(S_0) = \frac{\sum_{i=1}^{n} \frac{Z(S_i)}{d(S_i, S_0)}}{\sum_{i=1}^{n} \frac{1}{d(S_i, S_0)}}
$$


Where S0 represents the location you want to predict the value for, Si is your known value at location Si, d is the distance between S0 and Si, and n is the number of nearby points/neighbours (Metahni et al., 2019). Luckily, R will calculate all of these values for us using the following code chunk:

```
#Create a grid for the interpolation. Adjust the extent and resolution of the grid according to your needs
bbox <- st_bbox(prov_polygon)
grid <- st_make_grid(st_as_sfc(bbox), cellsize = c(50000, 50000))  # Adjust the cell size

#Interpolate using IDW
idw_result <- gstat::idw(MAX_TEMP ~ 1, 
                         locations = final_climate_data, 
                         newdata = st_as_sf(grid), 
                         idp = 2)

#Convert idw_result to an sf object
idw_sf <- st_as_sf(idw_result)

#Extract coordinates 
coordinates <- st_coordinates(idw_sf)

#Plot the results using geom_sf() for better handling of sf objects
ggplot(data = idw_sf) +
  geom_sf(aes(fill = var1.pred), color = NA) +
  scale_fill_viridis_c(name = "Temperature(°C)", option = "D", direction = -1) +  # Customizing the color palette
  labs(title = "IDW Interpolation of Temperature", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.position = "right")

#Save the result to a shapefile if needed
st_write(idw_sf, "IDW_Result.shp", driver = "ESRI Shapefile", delete_dsn = TRUE)

# Verify the structure of the polygon shapefile
print(head(prov_polygon))
# Check the CRS of both objects
crs_idw <- st_crs(idw_sf)  # CRS of IDW result
crs_polygon <- st_crs(prov_polygon)  # CRS of the polygon shapefile

# Now attempt the intersection again
idw_clipped <- st_intersection(idw_sf, prov_polygon)

# Check the results of clipping
print(st_geometry(idw_clipped))  # Check geometry to ensure it's clipped correctly

# Create the map of the clipped results with a white background
ggplot(data = idw_clipped) +
  geom_sf(aes(fill = var1.pred), color = NA) +  # Fill based on predicted temperature values
  scale_fill_viridis_c(option = "D") +  # Use viridis color scale for better readability
  labs(title = "Clipped IDW Interpolation of Temperature",
       fill = "Temperature (°C)",  # Change label as appropriate
       x = "Longitude", 
       y = "Latitude") +
  theme_minimal() +  # Use a minimal theme
  theme(
    legend.position = "right",  # Position the legend on the right
    panel.background = element_rect(fill = "white", color = NA),  # White background for the panel
    plot.background = element_rect(fill = "white", color = NA),   # White background for the whole plot
  )

#Save the map as an image file (optional)
ggsave("Clipped_IDW_Interpolation_Map.png", width = 10, height = 8, dpi = 300)
```

![Clipped_IDW_Interpolation_Map](https://github.com/user-attachments/assets/06d0451b-e8af-4af5-872e-53115e7048bb)

### Determinging if Temperature Explains Wildfires 
In order to make conclusions about whether temperature explains wildfires in BC, we can look at Ordinary Least Squares Regression (OLS), Moran’s I Statistic, and Geographically Weighted Regression (GWR). To begin, we will want to turn our fire points into density data to map and then combine our climate IDW and fire density.

First, let’s see at how we can visualize our fire density:
```
#layers should already be loaded in
# Ensure bbox2 is valid and formatted correctly
bbox2 <- st_bbox(prov_polygon)

raster_res <- 50000  # This resolution in 50000 meters 
raster_template <- raster(extent(bbox2), res = c(raster_res, raster_res))

# Estimate density using kernel density estimate
density_raster <- raster::rasterize(st_as_sf(final_fire_data), raster_template, fun = "count", field = 1)

# Ensure all NAs are turned to zeros in the raster
density_raster[is.na(density_raster)] <- 0

# Convert the raster to a data frame and replace any potential NAs with zeros
density_df <- as.data.frame(density_raster, xy = TRUE)
density_df[is.na(density_df)] <- 0  # Replace NAs in the data frame with zeros

# Step to rename the 'layer' column to 'fires' if applicable
colnames(density_df)[colnames(density_df) == "layer"] <- "fires"

# Convert to a spatial points data frame using sf (if needed later)
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(prov_polygon))

# Plotting the density map with the polygon boundary
ggplot() +
  geom_raster(data = density_df, aes(x = x, y = y, fill = fires)) +  # Use 'fires' from the data frame
  geom_sf(data = prov_polygon, fill = NA, color = "black") + # Boundary polygon
  scale_fill_viridis_c(option = "plasma") +  # Using a color scale
  theme_minimal() +
  labs(title = "Density Map of Fire Points",
       x = "Longitude",
       y = "Latitude",
       fill = "Density")

# Convert the raster to a data frame
density_df <- as.data.frame(density_raster, xy = TRUE)

# Rename the 'layer' column to 'fires'
colnames(density_df)[colnames(density_df) == "layer"] <- "fires"

# Replace NA values with zeros
density_df[is.na(density_df$fires), "fires"] <- 0

# Convert to a spatial points data frame using sf
density_sf <- st_as_sf(density_df, coords = c("x", "y"), crs = st_crs(prov_polygon))

# Write to a shapefile
st_write(density_sf, "density_points.shp", delete_dsn = TRUE)

# Create a simple map
ggplot() +
  geom_sf(data = prov_polygon, fill = NA, color = "black") +  # Plot the boundary polygon
  geom_sf(data = density_sf, aes(color = fires), size = 1) +  # Plot the density points with color mapping
  scale_color_viridis_c(option = "plasma", name = "Density of Fires") +  # Color scale for density values
  theme_minimal() +
  labs(title = "Density of Fires within Boundary",
       x = "Longitude",
       y = "Latitude")
```
Now we can combine our climate and events data to create our final data output.
```
# Perform the spatial join
joined_data <- st_join(idw_clipped, density_sf, join = st_intersects)

# Select needed columns
final_data <- joined_data[, c("var1.pred", "fires")]

# Rename column
final_data <- final_data %>%
  rename(temperature = var1.pred)

# Replace NA values in the fires column with 0
final_data <- final_data %>%
  mutate(fires = ifelse(is.na(fires), 0, fires))

# Create the map
ggplot(data = final_data) +
  geom_sf(aes(fill = fires)) +
  scale_fill_viridis_c(option = "C") +
  theme_minimal() +
  labs(title = "Temperature Map",
       fill = "Temperature (°C)") +
  theme(legend.position = "right")

# Save final_data as a shapefile
st_write(final_data, "final_data.shp", delete_dsn = TRUE)

# Convert final_data to a data frame
final_data_df <- st_drop_geometry(final_data)

# Write as CSV
write.csv(final_data_df, "final_data.csv", row.names = FALSE)
```


## Discussion


## References
esri. (n.d.). How inverse distance weighted interpolation works. How inverse distance weighted interpolation works-ArcGIS Pro | Documentation. https://pro.arcgis.com/en/pro-app/latest/help/analysis/geostatistical-analyst/how-inverse-distance-weighted-interpolation-works.htm 

Metahni, S., Coudert, L., Gloaguen, E., Guemiza, K., Mercier, G., & Blais, J.-F. (2019). Comparison of different interpolation methods and sequential gaussian simulation to estimate volumes of soil contaminated by as, CR, Cu, PCP and dioxins/furans. Environmental Pollution, 252, 409–419. https://doi.org/10.1016/j.envpol.2019.05.122 

Tobler, W. R. (1970). A computer movie simulating urban growth in the Detroit Region. Economic Geography, 46, 234. https://doi.org/10.2307/143141
