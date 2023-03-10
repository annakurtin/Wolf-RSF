```{r Read in tifs}
deer_w2 <- st_read(".\\Data\\tif_files\\deer_w2.tif")
# error with reading in this data
```

```{r Read in Shapefiles}
elc_habitat<-st_read(here::here("Data","elc_habitat.shp"))
humanaccess<-st_read(here::here("Data","humanacess.shp"))
wolfyht<-st_read(here::here("Data","wolfyht.shp"))
```


```{r Raster Stack}
# Create a mask raster to use as a template for converting shapefile data to rasters
#create an empty raster
mask.raster <- rast()

#set extent (note that I customized this extent so it covered both elc_habitat and humanacess) (this usually requires some research to set your bounding box)
ext(mask.raster) <- c(xmin=443680.6, xmax=650430.4, ymin=5618416, ymax=5789236) 	

#set the resolution to 300 m 
res(mask.raster)<-300

#match projection to elc_habitat shapefile
## proj4str() pullls out the projection of a file
crs(mask.raster)<- "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

#set all values of mask.raster to zero
mask.raster[]<-0


deer_w<-terra::rasterize(elc_habitat, mask.raster, field="DEER_W")
moose_w<-terra::rasterize(elc_habitat, mask.raster, field="MOOSE_W")
elk_w<-terra::rasterize(elc_habitat, mask.raster, field="ELK_W")
sheep_w<-terra::rasterize(elc_habitat, mask.raster, field="SHEEP_W")
goat_w<-terra::rasterize(elc_habitat, mask.raster, field="GOAT_W")
wolf_w<-terra::rasterize(elc_habitat, mask.raster, field="WOLF_W")

# Read in elevation raster
elevation2<-rast(here::here("Data","Elevation2.tif"))
# Read in human access rasters
disthumanaccess<-rast(here::here("Data\\tif_files","DistFromHumanAccess2.tif")) 
disthighhumanaccess<-rast(here::here("Data\\tif_files","DistFromHighHumanAccess2.tif"))

#resample elevation and humanaccess to match mask.raster 
elevation2<-resample(elevation2, mask.raster, method="bilinear")
disthumanaccess2<-resample(disthumanaccess, mask.raster, method="bilinear")
disthighhumanaccess2<-resample(disthighhumanaccess, mask.raster, method="bilinear")

#any more cleaning to do?
```


# Home range analysis by pack

```{r Red Deer Cleaning}
# convert to a spatial points dataframe
rd.data<-wolfyht[wolfyht$Pack=="Red Deer",]
# extract eastings and northings
x<-rd.data$EASTING
y<-rd.data$NORTHING
# put it into a new dataframe
xy<-cbind(x,y)
class(xy)
rd <- data.frame(as.character(rd.data$NAME))
coordinates(rd) <- xy
crs(rd) <-  "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

table(rd.data$NAME)
#42 60 69 70 81 82 84 
#43 25  4 15  3  2  1  
#looks like 4 of the wolves do not have enough locations

#remove these individuals with too few of locations
names(rd)<-"NAME"
rd<-rd[rd@data$NAME!="69" & rd@data$NAME!="81" & rd@data$NAME!="82" & rd@data$NAME!="84",]
#remove unused NAME levels
rd@data$NAME<-factor(rd@data$NAME)

```

```{r Red Deer MCP}
# Fit 99% mcp
cp.rd <- mcp(rd, percent=99)
plot(rd, col="black")
plot(cp.rd[cp.rd@data$id=="42",], col="blue", add=TRUE)
plot(cp.rd[cp.rd@data$id=="70",], col="green", add=TRUE)
plot(cp.rd[cp.rd@data$id=="60",], col="red", add=TRUE)
plot(rd, col="black", add=TRUE)
# gives you wolves with 99% mcp home range estimation

```


```{r Red Deer KDE}
#calculate 99% KDE for Red Deer wolf pack
red.deerUD <- kernelUD(rd, grid=30, extent=0.5, same4all=TRUE) # reference grid

#get polygons for home ranges instead of the raster output from the original 
homerangeRD <- getverticeshr(red.deerUD)
as.data.frame(homerangeRD)
plot(homerangeRD, col=2:4)
# 95% estimation of home range size 

#Estimate UD in raster mode
red.deerud <- getvolumeUD(red.deerUD) 

## Set up graphical parameters for the output of getvolumeUD 
par(mar=c(0,0,2,0)) #set margin
image(red.deerud[[1]]) #for first wolf only
title("Red Deer Wolf UD") 
xyzv <- as.image.SpatialGridDataFrame(red.deerud[[1]]) 
contour(xyzv, add=TRUE)

```


```{r Bow Valley Cleaning}
# convert to a spatial points dataframe
bv.data<-wolfyht[wolfyht$Pack=="Bow valley",]
# extract eastings and northings
x<-bv.data$EASTING
y<-bv.data$NORTHING
# put it into a new dataframe
xy<-cbind(x,y)
bv <- data.frame(as.character(bv.data$NAME))
coordinates(bv) <- xy
crs(bv) <-  "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

table(bv.data$NAME)
#These are all good - don't need to remove any
```

```{r Bow Valley MCP}
# Fit 99% mpc
cp.bow <- mcp(bv, percent=99)
plot(bv, col="black")
plot(cp.bow[cp.bow@data$id=="63",], col="blue",add=TRUE)
plot(cp.bow[cp.bow@data$id=="87",], col="red",add=TRUE,)
plot(cp.bow[cp.bow@data$id=="44",], col="green",add=TRUE)
plot(bv, col="black", add=TRUE)

```


```{r Bow Valley KDE}
#calculate 99% KDE for Red Deer wolf pack
bow.valleyUD <- kernelUD(bv, grid=30, extent=0.1, same4all=TRUE) # reference grid

#get polygons for home ranges
homerangeBV <- getverticeshr(bow.valleyUD)
as.data.frame(homerangeBV)
class(homerangeBV)
plot(homerangeBV, col=2:4)

#Estimate UD in raster mode
bow.valleyud <- getvolumeUD(bow.valleyUD) 

## Set up graphical parameters for the output of getvolumeUD 
par(mar=c(0,0,2,0)) #set margin
image(bow.valleyud[[1]])
title("Bow Valley Pack UD") 
xyzv <- as.image.SpatialGridDataFrame(bow.valleyud[[1]]) 
contour(xyzv, add=TRUE)
```



## Both packs
```{r both packs}
#first convert the spatialpointsdataframe to spatial points object
x<-wolfyht$EASTING
y<-wolfyht$NORTHING
xy<-cbind(x,y)

all <- data.frame(as.character(wolfyht$Pack))
coordinates(all) <- xy
crs(all) <-  "+proj=utm +zone=11 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"

# Fit 99% mpc
cp.all <- mcp(all, percent=99)

plot(all, col="black")
plot(cp.all[cp.all@data$id=="Bow valley",], col="blue",add=TRUE)
plot(cp.all[cp.all@data$id=="Red Deer",], col="green",add=TRUE)
plot(wolfyht, col="black", add=TRUE)

#calculate area for different percents of MPC
mcp.area(all, percent=seq(50, 100, by=5))

#calculate 99% KDE for both wolf packs
allUD <- kernelUD(all, grid=30, extent=0.5, same4all=TRUE) # reference grid
image(allUD)

# get polygons for home ranges
homerangeALL <- getverticeshr(allUD)
as.data.frame(homerangeALL)
class(homerangeALL)
plot(homerangeALL, col=2:3)

# Estimate use density in raster mode
allud <- getvolumeUD(allUD) 

## Set up graphical parameters for the output of getvolumeUD 
par(mar=c(0,0,2,0)) #set margin
image(allud[[1]]) #for first wolf only
title("Output of getvolumeUD") 
xyzv <- as.image.SpatialGridDataFrame(allud[[1]]) 
contour(xyzv, add=TRUE)
```



# Creating used - available data for each pack


```{r Used Available Data}
#subset polygons by wolf pack
red.deerPOLY<-homerangeALL[homerangeALL@data$id=="Red Deer",]
bow.valleyPOLY<-homerangeALL[homerangeALL@data$id=="Bow valley",]

#generate 1000 points from Red Deer wolf pack KDE polygon
rd.avail<-spsample(red.deerPOLY, 1000, "random")

#generate 1000 points from Bow valley wolf pack KDE polygon
bv.avail<-spsample(bow.valleyPOLY, 1000, "random")

plot(wolfyht$EASTING,wolfyht$NORTHING, col=c("red","blue")[wolfyht$PackID],ylab="Northing",xlab="Easting")
legend(555000,5742500,unique(wolfyht$Pack),col=c("blue","red"),pch=1)
plot(bv.avail, add=TRUE)
plot(rd.avail, add=TRUE)
```

# Associate the points with underlying ecological data - create a raster stack

```{r}
all_rasters<-c(deer_w, moose_w, elk_w, sheep_w, goat_w, wolf_w,elevation2, disthumanaccess2, disthighhumanaccess2)
# should be 8607
crs(deer_w)
crs(moose_w)
crs(elk_w)
crs(sheep_w)
crs(goat_w)

```