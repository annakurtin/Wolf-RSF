######## Preparing Raster Data####

wolfkde3 <- wolfkde
wolfkde3$usedFactor <-as.factor(wolfkde3$usedFactor)
kernelHR <- st_read(".\\Data\\shapefiles\\homerangeALL.shp")
deer_w<-rast(".\\Data\\tif_files\\deer_w2.tiff")
moose_w<-rast(".\\Data\\tif_files\\moose_w2.tif")
elk_w<-rast(".\\Data\\tif_files\\elk_w2.tif") # already brought in above
sheep_w<-rast(".\\Data\\tif_files\\sheep_w2.tif")
goat_w<-rast(".\\Data\\tif_files\\goat_w2.tif")
wolf_w<-rast(".\\Data\\tif_files\\wolf_w2.tif")
elevation2<-rast(".\\Data\\tif_files\\Elevation2.tif") #resampled
disthumanaccess2<-rast(".\\Data\\tif_files\\DistFromHumanAccess2.tif") #resampled in lab 4
disthhu2<-rast(".\\Data\\tif_files\\DistFromHighHumanAccess2.tif") #resampled in lab 4
landcover2 <- rast(".\\Data\\tif_files\\landcover16.tif")

# resampl landcover and human access layers to match extents
elevation2 <- resample(elevation2, elk_w)
disthhu2 <- resample(disthhu2, elk_w)
disthumanaccess2 <- resample(disthumanaccess2, elk_w)
landcover2 <- resample(landcover2, elk_w)

# create values for landcover rasters 
alpine <- ifel(landcover2 == 15 | landcover2 == 16, 1,ifel(is.na(landcover2),NA,0))
burn <- ifel(landcover2 == 12 | landcover2 == 13 |landcover2 == 14, 1, ifel(is.na(landcover2),NA,0))
closedConif <- ifel(landcover2 == 3, 1, ifel(is.na(landcover2),NA,0))
herb <- ifel(landcover2 == 7, 1, ifel(is.na(landcover2),NA,0))
mixed <- ifel(landcover2 == 5, 1, ifel(is.na(landcover2),NA,0))
rockIce <- ifel(landcover2 == 10, 1, ifel(is.na(landcover2),NA,0))
water <- ifel(landcover2 == 9, 1, ifel(is.na(landcover2),NA,0))
modConif <- ifel(landcover2 == 2, 1, ifel(is.na(landcover2),NA,0))
decid <- ifel(landcover2 == 10, 1, ifel(is.na(landcover2),NA,0))

all_rasters<-c(deer_w, moose_w, elk_w, sheep_w, goat_w,elevation2, disthumanaccess2, disthhu2, landcover2, alpine, burn, closedConif, modConif, herb, mixed, rockIce, water, decid)

plot(all_rasters)
