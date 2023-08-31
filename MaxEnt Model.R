# Install the packages with install.package():
#install.packages("dismo")
#install.packages("rJava")

options(java.parameters = "-Xmx4g") # Setting the maximum heap memory to 4GB

# MAXENT modelling
# Before using 'rJava' make sure the 'Java' application is installed on computer go to: https://www.java.com/en/
## load following packages
library("raster")
library("dismo")
library("tmap")
library("sf")
library("rJava")
library("sp")

setwd("E:\\RSEM\\AAA - dissertation\\MaxEnt-R\\Preprocessing Data")

# load survey points of wildfire occurrences in 2013
wildfire.data <- read.csv("fire_2021_cleaning_90.csv")

# load raster files
precipitation <- raster("Brazil Precipitation3.tif")
maxtemp <- raster("Brazil Maximum Temperature2222.tif")
ndvi <- raster("Brazil NDVI.tif")
elevation<- raster("Brazil Elevation.tif")
slope <- raster("Brazil Slope.tif")
population <- raster("Brazil Population Density.tif")
landcover <- raster("Brazil Land Cover.tif")
wind_U <- raster("Brazil Wind u-component.tif")
wind_V <- raster("Brazil Wind v-component.tif")

# load shapefile data for california
Brazil_country <- read_sf("gadm41_BRA_0.shp")
Brazil_state <- read_sf("gadm41_BRA_1.shp")
Brazil_biomes <- read_sf("Brazil_biomes.shp")
Brazil_city <- read_sf("gadm41_BRA_2.shp")

# step 1: We need to prepare for data for maxent analysis
## first, we must create a multi-band raster using stack() function on the environmental variables
env_covariates <- stack(precipitation, maxtemp, ndvi, elevation, slope, population, landcover, wind_U, wind_V)
names(env_covariates) <- c("Precipitation", "Temperature", "NDVI", "Elevation", "Slope", "Population", "Land Cover", "Wind Speed U", "Wind Speed V")

# step 2: we need to convert points of occurrence from a data frame object to that of a spatial points object
## here, declare the column longitude and latitude as coordinates
wildfire_points <- wildfire.data[, c(1,2)]
coordinates(wildfire_points) = ~longitude+latitude

# make sure the CRS is defined: WGS84 4326 | using crs() from sp
crs(wildfire_points) <- "+proj=longlat +datum=WGS84 +no_defs"
# show details of temp as example
#temp
#tm_shape(Brazil_state) + tm_polygons() + tm_shape(wildfire_points) + tm_dots(col = "red")

# step 3: create background points within the Brazilian region
## here, we will general a set of random pseudo-absence points to act as controls where there's no aedes occurrences
## here, we double the number of controls based on the presence points
set.seed(19990525)
Brazil_state_sp <- as(Brazil_state, Class = "Spatial")
background_points <- spsample(Brazil_state_sp, n=2*length(wildfire_points), "random")

# step 4: perform raster extraction from the environmental covariates on to all points
wildfire_points_env <- extract(env_covariates, wildfire_points)
background_points_env <- extract(env_covariates, background_points)

## convert the large matrix objects to separate data frame objects and then add binary outcome `present`
wildfire_points_env <-data.frame(wildfire_points_env, present=1)
background_points_env <-data.frame(background_points_env, present=0)

# step 5: 4-fold cross validation

# split plot panel into 4 segments for 4 AUC plots
par(mfrow=c(2,2))
# create a list() object to dump results inside `eMAX`
eMAX<-list()

# use wildfire_points_env
# use background_points_env
folds <- 4

kfold_pres <- kfold(wildfire_points_env, folds)
kfold_back <- kfold(background_points_env, folds)

set.seed(19990525)
# adapting loop code from https://rpubs.com/mlibxmda/GEOG70922_Week5
# takes a long time to run 4-fold
for (i in 1:folds) {
  train <- wildfire_points_env[kfold_pres!= i,]
  test <- wildfire_points_env[kfold_pres == i,]
  backTrain<-background_points_env[kfold_back!=i,]
  backTest<-background_points_env[kfold_back==i,]
  dataTrain<-rbind(train,backTrain)
  dataTest<-rbind(test,backTest)
  maxnet_eval <- maxent(x=dataTrain[,c(1:9)], p=dataTrain[,10], args=c("responsecurves"))
  eMAX[[i]] <- evaluate(p=dataTest[dataTest$present==1,],a=dataTest[dataTest$present==0,], maxnet_eval)
  plot(eMAX[[i]],'ROC')
}

aucMAX <- sapply( eMAX, function(x){slot(x, 'auc')} )
# report 4 of the AUC
aucMAX
# find the mean of AUC (and it must be > 0.50)
mean(aucMAX)

#Get maxTPR+TNR for the maxnet model
Opt_MAX<-sapply( eMAX, function(x){ x@t[which.max(x@TPR + x@TNR)] } )
Opt_MAX

Mean_OptMAX<-mean(Opt_MAX)
Mean_OptMAX
# use Mean_OptMAX as threshold for mapping suitability

# step 6: train the model
maxnet_eval@results

# step 7: obtain results
# 1.) check variable contribution
plot(maxnet_eval, pch=19, xlab = "Percentage [%]", cex=1.2)
response(maxnet_eval)

# 2.) validation with ROC curve
plot(eMAX[[i]], 'ROC', cex=1.2)

# 3.) mapping predicted probabilities of wildfire occurrence
prob_wildfire <- predict(maxnet_eval, env_covariates)

# generate a publication-worthy figure
# map of probability 
tm_shape(prob_wildfire) +
  tm_raster(title = "Predicted probability", palette = '-RdYlBu', style ='cont', breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0))+
  tm_shape(Brazil_state) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(main.title = "Predicted Probability of Wildfire in Brazil [%]", main.title.position = c(0.05, 0.7), title.size=3.0, legend.text.size = 3.0, 
            legend.position = c(0.85, 0.02), legend.height= -0.3, legend.title.size = 5.0, frame='white')+
  tm_scale_bar(position=c(0.02, 0.02), text.size = 5.0, breaks = c(0, 250, 500, 750, 1000))+
  tm_compass(north = 0,type = 'arrow', position = c(0.85, 0.80), text.size = 0.9)

# calculate thresholds of models
threshold_value <- threshold(eMAX[[i]], "spec_sens")
# report value
threshold_value

# prepare threshold total map 
create_classes_vector <- c(0, threshold_value, 0, threshold_value, 1, 1)
create_clasess_matrix <- matrix(create_classes_vector, ncol = 3, byrow = TRUE)
create_clasess_matrix

# create new reclassify raster based on prob_wildfires
suitability_wildfires <- reclassify(prob_wildfire, create_clasess_matrix)

tm_shape(suitability_wildfires) + tm_raster(style = "cat", title = "Threshold", palette= c("lightgrey", "red"), labels = c("Safe", "Trigger Points")) +
  tm_shape(Brazil_state) + tm_polygons(alpha = 0, border.col = "black") +
  tm_layout(frame = FALSE, legend.outside = TRUE)

### Population Suitability Analysis

# Multiply population density by trigger point
suitable_popdensity <- suitability_wildfires*population

# output the result raster
writeRaster(suitable_popdensity, filename = "suitability of population.tif", overwrite = TRUE)
writeRaster(suitability_wildfires, filename = "trigger points of wildfire.tif", overwrite = TRUE)
writeRaster(prob_wildfire, filename = "probability of wildfire.tif", overwrite = TRUE)

# Aggregate raster data to vector regions
population_exposed <- extract(suitable_popdensity, Brazil_state, fun=sum,na.rm=TRUE)

# Adding results to a vector region property sheet
Brazil_state$population_exposed <- population_exposed

# Save vector regions with population exposure data
st_write(Brazil_state, "Brazil_state_with_population_exposed.shp")

# Aggregate total population values to the state polygons
population_by_state <- extract(population, Brazil_state, fun=sum, na.rm=TRUE)

# Add the aggregated population values to the state polygons' attribute table
Brazil_state$population_total <- population_by_state

# Save vector regions with total population data
st_write(Brazil_state, "Brazil_state_with_population.shp")

# Calculate the percentage of exposed population in each state
Brazil_state$percentage_exposed <- (Brazil_state$population_exposed / Brazil_state$population_total) * 100

# Save vector regions with the percentage
st_write(Brazil_state, "Brazil_state_with_percentage.shp")

## at city scale
# Aggregate raster data to city
population_exposed2 <- extract(suitable_popdensity, Brazil_city, fun=sum,na.rm=TRUE)

# Adding results to a vector region property sheet
Brazil_city$population_exposed2 <- population_exposed2

# Aggregate total population values to the city polygons
population_by_city <- extract(population, Brazil_city, fun=sum, na.rm=TRUE)

# Add the aggregated population values to the state polygons' attribute table
Brazil_city$population_total2 <- population_by_city

# Calculate the percentage of exposed population in each city
Brazil_city$percentage_exposed2 <- (Brazil_city$population_exposed2 / Brazil_city$population_total2) * 100

# Save city vector regions with the percentage
st_write(Brazil_city, "Brazil_city_with_percentage.shp")
