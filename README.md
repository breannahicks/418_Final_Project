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
BC <- st_read <- st_read("BC.shp") #study area shapefile

# Transform the CRS from EPSG:3347 to EPSG:3005
prov_polygon <- st_transform(BC, crs = 3005)

# Create the map
ggplot() +
  geom_sf(data = prov_polygon, fill = "lightgrey", color = "black") +
  # Map the TEMP variable to color
  geom_sf(data = final_climate_data, aes(color = MAX_TEMP), size = 2) + 
  scale_color_gradient(low = "blue", high = "red") + # Adjust color gradient as needed
  theme_minimal() +
  labs(title = "Map of Climate Data Points in British Columbia",
       subtitle = "Overlayed on BC Boundary",
       x = "Longitude",  # Use Longitude for x-axis
       y = "Latitude",   # Use Latitude for y-axis
       color = "Temperature (°C)") + # Label for color legend
  theme(legend.position = "bottom")
```
Here’s how mine turned out:


### Evaluating Spatial Distribution of Wildfires 


### Creating a Temperature Surface


### Determinging if Temperature Explains Wildfires 

## Results

## Discussion
