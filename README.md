# Spatial Analysis of Wildfires and Temperature in British Columbia

## 1.0 Introduction
The number of wildfires in British Columbia have been rapidly increasing with more area burned from 2017–2023 than the previous 58 years (1959–2016) (Daniels et al., 2024). Although there are many drivers for the different types of fires that burn across the province, one very important one to focus on is rising temperatures. This is because “increases in maximum temperatures and concurrent decreases in relative humidity cause vapour pressure deficits” (Daniels et al., 2024, pp. 7). This can cause the areas at high fire risk (forests) to dry out and be more susceptible to more intense and frequent wildfires (Flannigan et al., 2016). Starting this kind of positive feedback loop can be extremely difficult to halt and reverse. Therefore it is crucial to ask the question: Do daily maximum temperatures between June and August (fire season) in the last 20 years (2004–2024) significantly correlate with the number of wildfires in British Columbia? In this tutorial, we will perform various statistical tests in order to answer this question.

## 2.0 Study Site
For this study we will be examining wildfires in British Columbia (BC), Canada. This is because “British Columbia has one of the highest proportions of land covered with forests (57%) among all jurisdictions” (Gilani & Innes, 2020, pp. 1). Additionally, 41% of British Columbia’s forests are classified as old growth which makes them even more important to protect (Gilani & Innes, 2020). The mean fire size in BC from 2004-2024 was 0.01 ha and the median fire size was 190.13 ha (See section 4.2 of this tutorial). BC’s fire statistics are also important to examine because of the diversity of the province’s biogeoclimatic zones (Figure 1). Since BC has many different zones, conservation and mitigation may look different when examined on a more local level. 

![biogeo](https://github.com/user-attachments/assets/047fcd91-34cf-452e-bbaf-75c5ea88a5ed)
Figure 1. Biogeoclimactic zones of British Columbia

## 3.0 Data
The data used in this study was 1) Climate data collected from the Pacific Climate Impacts Consortium (PCIC) and 2) Wildfire data collected from the BC Data Catalogue. The data I collected was the daily maximum temperature from 2004-2024 in British Columbia. The data you use can be anything that makes sense for your study as long as the data has relatively good coverage across your study area. The data from PCIC gives us a folder (in this case called “BCH”) which contains many .csv files with data from each weather station point. We want to look in these .csv files, extract the data we want from each one, combine it with our metadata (also obtained from PCIC), and compile it all into one excel file to use for our analysis. We will do this by creating a ‘for loop’ in R.

We will begin by installing the necessary packages (if needed), loading them in, and setting our working directory. Our working directory is where all of our files are being stored on our device and setting it will tell R where to look for them when we call on them. A package only needs to be installed once on your device, however, packages must be opened every time you open your R project.
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
Now we can run our for loop:
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

Figure 2. Map of Maximum Avergave Temperature Climate Data Points in British Columbia 2004-2024 June-August

The wildfire data I collected from the BC Data Catalogue contained a shapefile of wildfire point data from 1950-2024. The following code will clean up this data for the purposes of this study. Here, we are adjusting the format of our data, clipping it to our dates and boundary, and removing any NA values that may cause errors in calculations.
```
# Open BC boundary for clipping
clippingbound <- st_read("CNCNSSDVS1_polygon.shp")

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
final_fire_data <- st_intersection(subset_fire_data, clippingbound)

# Save as shapefile
st_write(final_fire_data, "final_fire_data.shp")

```
### 4.0 Methods

### 4.1 Examining Wildfire Descriptive Satistics
It’s important to be able to have some general statements that we can use to describe our climate driven event. We can gather these by performing descriptive and spatial descriptive statistics in our cleaned fire data. Here, we will be looking at the mean, median, and mean centre of our dataset.
```
#Descriptive stats on fire data: mean and median
mean_fire_size <- mean(final_fire_data$SIZE_HA)
median_fire_size <- median(final_fire_data$SIZE_HA) #most frequent fire size

#Create dataframe for display in table
statstable <- data.frame(Variable = c("Fire Size (HA)"),
                   Mean = c(round(mean_fire_size,2)),
                   Median = c(round(median_fire_size,2)))

#Produce table
kable(statstable, caption = paste0("Descriptive statistics for Fire Size, 2004-2024, British Columbia"))

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
![image](https://github.com/user-attachments/assets/c927c5c1-799f-4dae-9634-3039d970d0f5)

Figure 3. Descriptive Statistics for Wildfire Size (ha), British Columbia, 2004-2024

![mean center](https://github.com/user-attachments/assets/2c9cb8e8-4dff-4450-8b08-893bac330c43)

Figure 4. Mean centre of Fire Points in British Columbia, 2004-2024

### 4.2 Creating a Temperature Surface
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

Figure 5. IDW Map of Average Maximum Temperature in British Columbia, 2004-2024

### 4.3 Determinging if Temperature Explains Wildfires 
In order to make conclusions about whether temperature explains wildfires in BC, we can look at Ordinary Least Squares Regression (OLS), Moran’s I Statistic, and Geographically Weighted Regression (GWR). To begin, we will want to turn our fire points into density data to map and then combine our climate IDW and fire density.

First, let’s see at how we can create and visualize our fire density by creating 2 maps:
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

![density_fires](https://github.com/user-attachments/assets/047362ca-d84d-4df0-b4d0-cd8b1e23d750)

Figure 6. Density Map of Wildfires in British Columbia, 2004-2024 (raster)

![density_fires_points](https://github.com/user-attachments/assets/e4c6ff11-dad3-4b6f-a424-a9237ddf5eed)

Figure 7. Density Map of Wildfires in British Columbia, 2004-2024 (points)

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

# Save final_data as a shapefile
st_write(final_data, "final_data.shp", delete_dsn = TRUE)

# Convert final_data to a data frame
final_data_df <- st_drop_geometry(final_data)

# Write as CSV
write.csv(final_data_df, "final_data.csv", row.names = FALSE)
```
Now we can look at our OLS to understand the global correlation of our two variables. Mapping out OLS will show us where temperature is explaining wildfires versus where it is not on the model. We will do this by mapping the residuals. A larger residual means that a given point is further away from our regression line and therefore, our independent variable (temperature) doesn't explain much about our dependent variable (wildfires). 

Running the following code will show us a map of our residuals:

```
# Read the shapefile
final_data_sf <- st_read("final_data.shp")

# Fit the OLS regression model on the entire spatial data
# Use "temprtr" instead of "temperature"
ols_model <- lm(fires ~ temprtr, data = final_data_sf)

# Add residuals to the original spatial data frame
final_data_sf$residuals <- resid(ols_model)

# Inspect the updated spatial object to verify residuals are added
print(head(final_data_sf))

# (Optional) Save the updated shapefile with residuals
st_write(final_data_sf, "final_data_with_residuals.shp", delete_dsn = TRUE)

# Create a map of residuals from the OLS regression
ggplot(data = final_data_sf) +
  geom_sf(aes(fill = residuals)) + # Map the residuals to fill color
  scale_fill_viridis_c(option = "C", name = "Residuals") + # Use a color scale
  theme_minimal() +
  labs(title = "Map of Residuals from OLS Regression",
       fill = "Residuals") +
  theme(legend.position = "right")

# Optional: Save the plot if desired
ggsave("residuals_map.png", width = 10, height = 8, dpi = 300)
```
As you can see, my clipping boundary contains district lines. This caused some discrepancies in the map but the general trend can still be observed.

![OLS](https://github.com/user-attachments/assets/3360d796-9d51-42e6-8ced-c5ca47930273)

Figure 8. Map of Residuals from OLS Regression

This map shows that our independent variable (temperature) is doing a worse job explaining our dependent variable (wildfires) in the southern parts of the province. This is most likely because the data in that region represents more frequent fires and higher temperature. This means that the points in that region would be placed further away from our regression line on a graph, therefore, having larger residuals.

Now we can look at our Global Moran's I Statistic using a queen's neighbourhood. This is a global statistic because it uses all the mean values of the whole study area to calculate a single value (I) for evaluation. We will use the following equation to achieve this:

$$
I = \frac{\sum_{i=1}^n\sum_{j=1}^nW_{i,j}(x_i - \bar{x})(x_j - \bar{x})}{(\sum_{i=1}^n\sum_{j=1}^nW_{i,j})\sum_{i=1}^n(x_i - \bar{x})^2}
$$

The numerator of this equation is comparing the point of interest $(i)$ with its neighbors $(j)$ depending on our chosen weighing matrix $(W_{i,j})$. Similar to many other statistical tests, the denominator in this equation serves to standardize the values. Once we produce our output statistic using this equation, it is important to understand how to interpret it. Values of I that are high (relatively) exhibit positive spatial autocorrelation and values of I that are low (relatively) exhibit negative spatial autocorrelation.

```
#Fire Neighbours - Queens weight
fire.nb <- poly2nb(final_data_sf)
# Use st_coordinates to get the coordinates
fire.net <- nb2lines(fire.nb, coords=st_coordinates(st_centroid(final_data_sf)))
crs(fire.net) <- crs(final_data_sf) 

#Create Fire weights matrix
fire.lw <- nb2listw(fire.nb, zero.policy = TRUE, style = "W")

head(fire.lw[["weights"]])[c(1:3)]

#Calculate Global Moran's I for fires
miFires <- moran.test(final_data_sf$`fires`, fire.lw, zero.policy = TRUE)
#Extract Global Moran's I results for fire
mIFires <- miFires$estimate[[1]]
eIFires <- miFires$estimate[[2]]
varFires <- miFires$estimate[[3]]
```

The results of this statistic was I = 0.29. This indicates a moderate positive spatial autocorrelation of fires.


Lastly, we can run our GWR. This model will also examine the correlation between our independent variable (Temperature) and our dependent variable (Wildfires), however, it will model it with more sensitivity to local variability. We will be mapping the R2 value which will show either temperature explaining a lot about wildfires (high R2) or temperature explaining little about wildfires (low R2).

```
# Read the shapefile (with residuals included)
final_data_sf <- st_read("final_data.shp")

# Preview the data to check variable names and content
print(head(final_data_sf))
print(colnames(final_data_sf))

# Convert the sf object to Spatial object
final_data_sp <- as_Spatial(final_data_sf)

# Create neighborhood structure
neighbors <- poly2nb(final_data_sp, queen = TRUE)

# Check neighbors for any issues
print(summary(neighbors))

# Check for any empty neighbors
if (any(sapply(neighbors, length) == 0)) {
  warning("Some polygons have no neighbors. This may cause issues for GWR.")
}

# Prepare the dependent and independent variables
dependent_var <- final_data_sp@data$fires
independent_vars <- final_data_sp@data$temprtr

# Check if both variables are numeric
if (!is.numeric(dependent_var) || !is.numeric(independent_vars)) {
  stop("Dependent and independent variables must be numeric.")
}

# Use gwr.sel to automatically select the optimal bandwidth
optimal_bandwidth <- gwr.sel(dependent_var ~ independent_vars, data = final_data_sp)
print(paste("Optimal Bandwidth selected: ", optimal_bandwidth))  # Output the selected bandwidth

# Run GWR with the optimal bandwidth selected by gwr.sel
gwr_model_optimal <- gwr(dependent_var ~ independent_vars,
                         data = final_data_sp,
                         bandwidth = optimal_bandwidth,
                         se.fit = TRUE)

# Validate that the model ran successfully
if (is.null(gwr_model_optimal)) {
  stop("The GWR model did not return any results.")
}

if (is.null(gwr_model_optimal$SDF)) {
  stop("The GWR model SDF is NULL, indicating it might not have calculated properly.")
}

# Print GWR summary
print(summary(gwr_model_optimal))

# Extract coefficients and create a dataframe for visualization
gwr_results_optimal <- as.data.frame(gwr_model_optimal$SDF)

# Extract coordinates from the original spatial data
coordinates_optimal <- st_coordinates(st_centroid(final_data_sf))  # Get coordinates from the original data

# Combine the GWR results with the coordinates
gwr_results_optimal <- cbind(gwr_results_optimal, coordinates_optimal)

# Convert to an sf object for visualization
gwr_output_sf_optimal <- st_as_sf(gwr_results_optimal, coords = c("X", "Y"), crs = st_crs(final_data_sf))

# Plotting GWR coefficients with the optimal bandwidth
ggplot(data = gwr_output_sf_optimal) +
  geom_sf(aes(colour = localR2)) +
  scale_fill_viridis_c(option = "C") +
  labs(title = "GWR Coefficients with Optimal Bandwidth",
       fill = "localR2") +
  theme_minimal()

# Optional: Save the plot
ggsave("gwr_coefficients_optimal_bandwidth.png", width = 10, height = 8, dpi = 300)
```

![GWR_Optimal_Bandwidth](https://github.com/user-attachments/assets/f42d61b7-1e01-41dc-9d39-a6210e4c0490)

Figure 9. GWR R2 Mapped with Optimal Bandwidth

This result indicates moderate to low model performance in most of the province. The model shows a greater performance in northern parts of BC.

## 5.0 Discussion
In this study we have examined various different methods to spatially analyze if daily maximum temperatures between June and August (fire season) in the last 20 years (2004–2024) significantly correlate with the number of wildfires in British Columbia. This study found that temperature has a moderate correlation to wildfire in BC, 2004-2024. This finding is based on the moderate positive spatial autocorrelation value of the Global Moran’s I statistic along with the moderate to low results of our GWR output. In all of our results we can see that temperature can somewhat explain wildfire in BC but not with confidence. One reason for this was because of the timeline chosen. Since this study used data over 20 years, the wildfire points were oversaturated across the province (Figure 4), therefore, making our results less informative. However, using such a long timeline makes the regions with no fires of great relevance. We can see in our models that low temperatures can explain low wildfires well in the northern parts of the province (Figure 9). That said, I recommend that future studies utilize a shorter timeline to be able to see how warmer temperatures may be influencing the distribution of wildfires in BC.

## 6.0 References
esri. (n.d.). How inverse distance weighted interpolation works. How inverse distance weighted interpolation works-ArcGIS Pro | Documentation. https://pro.arcgis.com/en/pro-app/latest/help/analysis/geostatistical-analyst/how-inverse-distance-weighted-interpolation-works.htm 

Flannigan, M. D., Wotton, B. M., Marshall, G. A., De Groot, W. J., Johnston, J., Jurko, N., Cantin, A. S. (2016). Fuel moisture sensitivity to temperature and precipitation: Climate change implications. Climatic Change, 134(1-2), 59-71. 10.1007/s10584-015-1521-0

Gilani, H. R., & Innes, J. L. (2020). The state of British Columbia’s Forests: A global comparison. Forests, 11(3), 316. https://doi.org/10.3390/f11030316 

Metahni, S., Coudert, L., Gloaguen, E., Guemiza, K., Mercier, G., & Blais, J.-F. (2019). Comparison of different interpolation methods and sequential gaussian simulation to estimate volumes of soil contaminated by as, CR, Cu, PCP and dioxins/furans. Environmental Pollution, 252, 409–419. https://doi.org/10.1016/j.envpol.2019.05.122 

Tobler, W. R. (1970). A computer movie simulating urban growth in the Detroit Region. Economic Geography, 46, 234. https://doi.org/10.2307/143141

