# activate packages
library("raster")
library("tmap")
library("sf")

setwd("/Users/snowfay/Desktop/Studying/Dissertation/dissertation/Datasets")

precipitation <- "/Users/snowfay/Desktop/Studying/Dissertation/dissertation/Datasets/bra_prec.tif"
Brazil_Prec <- raster(precipitation)
print(Brazil_Prec)

# standardize all raster to dimension of approx. 4.5 km (area = 21 sq.km)
RasterTemplate <- raster(nrow=dim(Brazil_Prec)[1], ncol=dim(Brazil_Prec)[2], crs=crs(Brazil_Prec), extent(Brazil_Prec))

# load in the following
#:::NDVI
#:::elevation
#:::slope
#:::population density
#:::land cover
#:::maximum temperature
#:::bioclimate
#:::u wind
#:::v wind

setwd("/Users/snowfay/Desktop/Studying/Dissertation/dissertation/Datasets")
ndvi <- raster("Brazil_NDVI.tif")
elevation <- raster("Brazil_Elevation.tif")
slope <- raster("Brazil_Slope.tif")
population <- raster("population.tif")
landcover <- raster("Brazil_Cover.tif")
maxtemp <- raster("temp_Brazil.tif")
bioclimate <- raster("bra_bioc.tif")
u_wind <- raster("2020 u-wind.tif")
v_wind<- raster("2020 v-wind.tif")

ndvi_resampled <- resample(ndvi, RasterTemplate, method = "bilinear")
elevation_resampled <- resample(elevation, RasterTemplate, method = "bilinear")
slope_resampled <- resample(slope, RasterTemplate, method = "bilinear")
population_resampled <- resample(population, RasterTemplate, method = "bilinear")
landcover_resampled <- resample(landcover, RasterTemplate, method = "ngb")
maxtemp_resampled <- resample(maxtemp, RasterTemplate, method = "bilinear")
bioclimate_resampled <- resample(bioclimate, RasterTemplate, method = "bilinear")
u_wind_resampled <- resample(u_wind, RasterTemplate, method = "bilinear")
v_wind_resampled <- resample(v_wind, RasterTemplate, method = "bilinear")
prec2_resampled <- resample(prec2, RasterTemplate, method = "bilinear")

print(ndvi_resampled)
print(elevation_resampled)
print(slope_resampled)
print(population_resampled)
print(landcover_resampled)
print(maxtemp_resampled)
print(bioclimate_resampled)
print(u_wind_resampled)
print(v_wind_resampled)
print(prec2_resampled)

setwd("/Users/snowfay/Desktop/Studying/Dissertation/dissertation/Preprocessing Data")

writeRaster(Brazil_Prec, filename = "Brazil Precipitation.tif", overwrite = TRUE)
writeRaster(ndvi_resampled, filename = "Brazil NDVI.tif", overwrite = TRUE)
writeRaster(elevation_resampled, filename = "Brazil Elevation.tif", overwrite = TRUE)
writeRaster(slope_resampled, filename = "Brazil Slope.tif", overwrite = TRUE)
writeRaster(population_resampled, filename = "Brazil Population Density.tif", overwrite = TRUE)
writeRaster(landcover_resampled, filename = "Brazil Land Cover.tif", overwrite = TRUE)
writeRaster(maxtemp_resampled, filename = "Brazil Maximum Temperature2222.tif", overwrite = TRUE)
writeRaster(bioclimate_resampled, filename = "Brazil Bioclimate.tif", overwrite = TRUE)
writeRaster(u_wind_resampled, filename = "Brazil Wind u-component.tif", overwrite = TRUE)
writeRaster(v_wind_resampled, filename = "Brazil Wind v-component.tif", overwrite = TRUE)
writeRaster(prec2_resampled, filename = "Brazil Precipitation3.tif", overwrite = TRUE)


