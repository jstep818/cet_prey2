##Mapping for MAPP paper
library(rgdal)
library(sp)
library(graticule)
library(rgeos)
library(raster)
library(ggplot2)
library(reshape2)
library(lubridate)
library(RColorBrewer)
library(sf)
library(dplyr)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(rnaturalearthhires)
library(lwgeom)
library(mapview)




setwd("C:/Users/jstepanuk/Dropbox/DMR_zones")
NOAAstat <- readOGR(".", "MaineDMR_-_Lobster_Zones")
NOAAstat <- spTransform(NOAAstat, "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
NOAAstat <- NOAAstat[order(NOAAstat$ZONEID),]
dmrzones <- st_as_sf(NOAAstat)

#download world polygon, coast line for clipping later
worldd <- st_as_sf(map_units10)
#oceann <- st_as_sf(ne_download(type="ocean", scale=50, category="physical"))
usa_coast <- ne_countries(scale = 10, returnclass = "sf", country=c("united states of america", "canada"))
neus_coast <- st_crop(usa_coast, c(xmin =-85, xmax =-55,ymin= 15, ymax=60))

neus_coast_prj <- neus_coast %>%
  st_transform(crs = "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
usa_coast_prj <- usa_coast %>%
  st_transform(crs = "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

natl_coastline <- ne_countries(scale=10, returnclass = "sf")
natl_coastline <- st_crop(natl_coastline, c(xmin=-150, xmax=100, ymin=-30, ymax=90))

natl_coastline <- natl_coastline %>%
  st_transform(crs = "+proj=laea +lat_0=53 +lon_0=-30 +x_0=431000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs") %>%
  group_by( featurecla) %>%  
  summarise(Project = unique(featurecla), do_union = TRUE)

#foraging and breeding grounds
setwd("C:/Users/jstepanuk/Dropbox/NOAA_climate_project/Figures/shps/")
grounds_breed <- st_read(".", "breed_grounds_polygon") %>%
  st_transform(crs = "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
grounds_forage <- st_read(".", "forage_grounds_polygon") %>%
  st_transform(crs = "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")

ggplot(natl_coastline) +
  geom_sf(data=grounds_breed, size=0, color=NA, fill="orange", alpha=1) + 
  geom_sf(data=grounds_forage, size=0, fill="darkblue", alpha=1) +
  geom_sf(data=natl_coastline, fill="darkgrey") +

  theme_minimal() +
  #annotation_scale(location = "br", width_hint = 0.5, style="ticks", text_cex = 1) +
  #annotation_north_arrow(location = "br", which_north = "true",  
                         #pad_x = unit(0.75, "in"), 
                        # pad_y = unit(0.2, "in"),
                        # style = north_arrow_orienteering) +
  theme(text=(element_text(size=14))) +
  coord_sf(ylim=c(-2068835, 5903591), 
           xlim=c(-4268835, 3046144))

### New NEUS Foraging Area
setwd("C:/Users/jstepanuk/Dropbox/NOAA_climate_project/Figures/shps/")
neus_forage <- readOGR(".", "neus_forage_bigger")
neus_forage <- spTransform(neus_forage, "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
neus_forage <- buffer(neus_forage, 10000)
neus_forage <- st_as_sf(neus_forage)

#shipping lanes
setwd("C:/Users/jstepanuk/Dropbox/shippinglanes/")
shipln <- readOGR(".", "shippinglanes")
shipln <- crop(shipln, extent(-79, -59, 32, 45))
shipln <- spTransform(shipln, "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
shipln <- st_as_sf(shipln)

narwzone <- shipln[shipln$OBJL == 112,]
narwzone <- narwzone[narwzone$THEMELAYER == "Speed Restrictions/Right Whales",]
narwzone$name <- c("Georgia/North Carolina", "Morehead City", NA, "Chesapeake","Massachusetts", "Block Island Sound","New York Harbor","Delaware Bay")
narwzone <- narwzone[narwzone$name == "Morehead City" |narwzone$name == "Chesapeake" | narwzone$name == "Massachusetts" | narwzone$name == "Block Island Sound" | narwzone$name == "New York Harbor" | narwzone$name == "Delaware Bay",]
narwzone$SMA <- factor(narwzone$name, levels= c("Massachusetts", "Block Island Sound", "New York Harbor", "Delaware Bay", "Chesapeake"))

narwzoneMA <- narwzone[12,]
narwzone_sf <- st_as_sf(narwzone)
shiplnn <- shipln[shipln$THEMELAYER == "Traffic Separation Schemes/Traffic Lanes",]

#SMAs with breakdown in the MA region
#setwd("C:/Users/jstepanuk/Downloads/shapefile-right-whale-sma-all")
#narwzone_fine <- readOGR(".", "right_whale_SMA_all_po")
#narwzone_fine <- spTransform(narwzone_fine, "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 #+lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
#narwzone_fine <- st_as_sf(narwzone_fine)
#narwzone_fine_sub <- narwzone_fine[narwzone_fine$Restr_Area == "NE U.S. Cape Cod Bay" | narwzone_fine$Restr_Area == "NE U.S. Off Race Point" | narwzone_fine$Restr_Area == "NE U.S. Great South Channel",]

#wind energy areas
setwd("C:/Users/jstepanuk/Dropbox/BOEM_Renewable_Energy_Areas_Shapefile_4_13_2020/")
boem_lease <- readOGR(".", "BOEM_Lease_Areas_4_13_2020")
boem_lease <- spTransform(boem_lease, "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
boem_lease <- st_as_sf(boem_lease)

boem_plan <- readOGR(".", "BOEM_Wind_Planning_Areas_4_13_2020")
boem_plan <- crop(boem_plan, extent(-79, -59, 35, 45))
boem_plan <- spTransform(boem_plan, "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
boem_plan <- st_as_sf(boem_plan)

boem_plan2 <- group_by(boem_plan, PROT_NUMBE) %>%  
  summarise(Project = unique(PROT_NUMBE), do_union = TRUE)

boem_lease2 <- group_by(boem_lease, PROT_NUMBE) %>%
  summarise(Project = unique(PROT_NUMBE), do_union=TRUE)

ggplot(neus_coast_prj) +
  geom_sf(data=dmrzones, color="blue", size=1) +
  geom_sf(data=shiplnn, size=0.5, fill="orange", alpha=0.8) + 
  geom_sf(data=boem_lease2, size=0.5, fill="yellow", alpha=0.8) + 
  geom_sf(data=boem_plan2, size=0.5, fill="yellow", alpha = 0.8) +
  geom_sf(data=neus_coast_prj, color=NA, fill="darkgrey") +
  
  theme_minimal() +
  annotation_scale(location = "br", width_hint = 0.5, style="ticks", text_cex = 1) +
  annotation_north_arrow(location = "br", which_north = "true",  
                         #pad_x = unit(0.75, "in"), 
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_orienteering) +
  theme(text=(element_text(size=14))) +
  coord_sf(ylim=c(242977.8  , 1285584))


sma_map <- ggplot(neus_coast_prj) +
  layer_spatial(whalepred_stack_ts_diff[[14]]) +
  scale_fill_gradientn(colors=cols(10), limits=c(0,2), name="", na.value=NA) +
  #geom_sf(data=neus_forage, fill="gray20", color=NA) +
  geom_sf(data=st_union(neus_coast_prj), size=0, fill="gray20") +
  geom_sf(data=narwzone[!is.na(narwzone$SMA),], aes(color=SMA), fill=NA, size=1, alpha=1) +
  #scale_color_manual(values=c("#8f0c40", "#7054d1", "#8ac3d7", "#21a38f", "#28597e", "#fa8a88"))+
  scale_color_manual(values=c("blueviolet","#382933","#0e49b5" ,  "#CC0000", "#fed049"))+
  theme_minimal() +
  annotation_scale(location = "br", width_hint = 0.5, style="ticks", text_cex = 1) +
  annotation_north_arrow(location = "br", which_north = "true",  
                         pad_x = unit(1.5, "in"), 
                         pad_y = unit(0.3, "in"),
                         style = north_arrow_orienteering) +
  theme(text=(element_text(size=12)),
        legend.position = "right") +
  guides(color = guide_legend(override.aes = list(fill = c("blueviolet", "#382933", "#0e49b5" , "#CC0000", "#fed049")))) +
  labs(color="Seasonal Management Areas", x="", y="") +
  
  #coord_sf(ylim=c(242977.8  , 1285584))
  coord_sf(crs="+proj=aea +lat_0=34 +lon_0=-78 +lat_1=27.3333333333333 +lat_2=40.6666666666667 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", xlim=c(-47437.4,1300563), ylim=c(150000,1302570)) 
  #annotate(geom = "curve", x = 566540.5, y = 565845, xend = 759386, yend = 110977, 
  #  curvature = -0.2, arrow = arrow(length = unit(4, "mm")), size=2) +
  #annotate(geom = "text", x = 562386, y = 235845, label = "Breeding \nGrounds", hjust = "center", size=8)+
  #annotate(geom = "text", x = 905386, y = 782845, label = "Summer Foraging \nGrounds", hjust = "center", angle=41, size=8)
sma_map 
  

#sma_map_leg <- ggpubr::get_legend(sma_map)
worldd_crop <- st_crop(worldd, c(xmin =-166, xmax =50,ymin= -70, ymax=90))

extentt <- data.frame(
  lon = c(-10079.2, -10079.2, 1129002, 1129002),
  lat = c(10977.8, 1315584, 1315584, 10977.8)
)
extenttp <- extentt %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
extentt_orth <- st_transform(extenttp, crs=ortho)

###do again for whole east coast
extentt <- data.frame(
  lon = c(-400437.4, -400437.4, 2100563, 2100563),
  lat = c(-897430, 1602570, 1602570, -897430)
)
extenttp <- extentt %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
extentt_orth <- st_transform(extenttp, crs=ortho)


###all this below from https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1
lat <- 20
lon <- -60
ortho <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon, ' +x_0=0 +y_0=0 +a=6371000 +b=6371000 +units=m +no_defs')

circle <- st_point(x = c(0,0)) %>% st_buffer(dist = 6371000) %>% st_sfc(crs = ortho)
circle_longlat <- circle %>% st_transform(crs = 4326)
if(lat != 0) {
  circle_longlat <- st_boundary(circle_longlat)
  
  circle_coords <- st_coordinates(circle_longlat)[, c(1,2)]
  circle_coords <- circle_coords[order(circle_coords[, 1]),]
  circle_coords <- circle_coords[!duplicated(circle_coords),]
  
  # Rebuild line
  circle_longlat <- st_linestring(circle_coords) %>% st_sfc(crs = 4326)
  
  if(lat > 0) {
    rectangle <- list(rbind(circle_coords,
                            c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                            c(X = 180, Y = 90),
                            c(X = -180, Y = 90),
                            c(X = -180, circle_coords[1, 'Y']),
                            circle_coords[1, c('X','Y')])) %>% 
      st_polygon() %>% st_sfc(crs = 4326)
  } else {
    rectangle <- list(rbind(circle_coords,
                            c(X = 180, circle_coords[nrow(circle_coords), 'Y']),
                            c(X = 180, Y = -90),
                            c(X = -180, Y = -90),
                            c(X = -180, circle_coords[1, 'Y']),
                            circle_coords[1, c('X','Y')])) %>% 
      st_polygon() %>% st_sfc(crs = 4326)
  }
  
  circle_longlat <- st_union(st_make_valid(circle_longlat), st_make_valid(rectangle))
}
visible <- st_intersection(st_make_valid(worldd), st_buffer(circle_longlat, -0.09)) %>%
  st_transform(crs = ortho)
broken_reason <- st_is_valid(visible, reason = TRUE)
na_visible <- visible[is.na(broken_reason),]
visible <- visible[!is.na(broken_reason),]

# Open and close polygons
na_visible <- st_cast(na_visible, 'MULTILINESTRING') %>% 
  st_cast('LINESTRING', do_split=TRUE)
na_visible <- na_visible %>% mutate(npts = npts(geometry, by_feature = TRUE))
# Exclude polygons with less than 4 points
na_visible <- na_visible %>%
  filter(npts >=4) %>%
  dplyr::select(-npts) %>%
  st_cast('POLYGON')
visible <- rbind(visible, na_visible)

ortho2 <- paste0('+proj=ortho +lat_0=', lat, ' +lon_0=', lon, ' +x_0=-85 +y_40 +a=6371000 +b=6371000 +units=m +no_defs')

sma_map_broad <- ggplot(worldd_crop) +
  #geom_sf(data=circle, color=NA, fill="aliceblue")+
  geom_sf(data = points_sf, aes(color=data, fill=data), shape=22 ) +
  scale_color_gradientn(colors=rev(c("#fce07e", "#78b5cc", "#5d44c4")), limits=c(-15,35)) +
  scale_fill_gradientn(colors=rev(c("#fce07e", "#78b5cc", "#5d44c4")), limits=c(-15,35)) +
  geom_sf(data=st_collection_extract(visible), color=NA, fill="gray20") +
  geom_sf(data=extentt_orth, fill=NA, color="black", size=1.5)+
  #annotate(geom="rect", xmin=-78, xmax=-63, ymin=34, ymax=46, color="black", fill="green", alpha=0.2) +
  theme_void() +
  #annotation_scale(location = "br", width_hint = 0.5, style="ticks", text_cex = 1) +
  #annotation_north_arrow(location = "br", which_north = "true",  
                         #pad_x = unit(0.75, "in"), 
                         #pad_y = unit(0.2, "in"),
                         #style = north_arrow_orienteering) +
  #theme(text=(element_text(size=14)),
        #legend.position = "") +
  coord_sf(crs=ortho, expand=FALSE)
  #coord_sf(ylim=c(242977.8  , 1285584))
  #coord_sf(crs=ortho, ylim=c(-6070835, 6070992), xlim=c(-6026747,6070992)) 

sma_map_broad


#Do as an inset from https://geocompr.github.io/post/2019/ggplot2-inset-maps/
library(cowplot)
sma_inset_map = ggdraw() +
  draw_plot(sma_map) +
  draw_plot(sma_map_broad, x = 0.09, y = 0.63, width = 0.25, height = 0.25)
#sma_inset_map

setwd("D:/PhD/Figures/20210122/")
png( filename="map_with_inset_20210201.png",width=12,height=10, units = "in", res=300, bg="white")
#png(paste("mncompare_spring2005_2012_mna10.png", sep=""),width=16,height=10, units = "in", res=300)
print(sma_inset_map)
dev.off()

##EFFORT PLOT
setwd("D:/PhD/roberts_data/v2/")
whaleeffort <- st_read(".", "segments")
whaleeffort <- whaleeffort %>%
  as("Spatial") %>%
  spTransform("+proj=aea +lat_1=27.33333333333333 +lat_2=40.66666666666666 +lat_0=34 +lon_0=-78 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0") %>%
  st_as_sf()

library(RColorBrewer)
n <- 20
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
pie(rep(1,n), col=sample(col_vector, n))

scalee <- c("#6486bd","#7adc52","#6436bc","#cfd24a","#c14fd6","#5d9940","#ce48a2","#66d7a3","#cd466b","#63c1ce","#d44c31","#6663c3","#d3933e","#693567","#c3d294","#cb90c4","#406b46","#d28d7a","#7d7230","#7d3a2a")

whaleeffort$Year <- as.factor(as.character(whaleeffort$Year_))
ggplot(whaleeffort) +
  geom_sf(data=whaleeffort, aes(color=Year), size=0.03) +
  geom_sf(data=st_collection_extract(usa_coast_prj), color="black", size=0.5, fill="darkgrey") +
  theme_minimal() +
  #scale_color_manual(values=scalee)+
  annotation_scale(location = "br", width_hint = 0.5, style="ticks", text_cex = 1) +
  annotation_north_arrow(location = "br", which_north = "true",  
                         #pad_x = unit(0.75, "in"), 
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_orienteering) +
  theme(text=(element_text(size=14))) +
  labs(x="", y="") +
  coord_sf(ylim=extent(whaleeffort)[1:2], xlim=extent(whaleeffort)[3:4])


##buffer plot
sf_100 <- st_as_sf(NEUS_buf100)
ggplot(sf_100) +
  geom_sf(data=sf_100, fill="darkblue", size=0.05) +
  geom_sf(data=usa_coast_prj, color="black", size=0.5, fill="darkgrey") +
  theme_minimal() +
  annotation_scale(location = "br", width_hint = 0.5, style="ticks", text_cex = 1) +
  annotation_north_arrow(location = "br", which_north = "true",  
                         #pad_x = unit(0.75, "in"), 
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_orienteering) +
  theme(text=(element_text(size=18))) +
  labs(x="", y="") +
  coord_sf(ylim=extent(sf_100)[1:2] + c(80000, -80000), xlim=extent(sf_100)[3:4])


##
#plot dmr zones on sst raster
library(viridis)
cols1 <- viridis(11, option ="viridis")
cols2 <- viridis(8, option="plasma")
cols2 <- cols2[1:7]

cols3 <- viridis(11, option="cividis")


ggplot(dmrzones) + 
  layer_spatial(temp2[[13]] - temp2[[12]]) +
  scale_fill_gradientn(colors=cols1, limits=c(0,3.5), name="SST - degrees C") +
  geom_sf(data=dmrzones, aes(color=ZONEID), fill=NA, size=1.5) +
  scale_color_manual(values=cols2, name="Fishing Area") +
  geom_sf(data=neus_coast_prj, color="black", size=0.5, fill="darkgrey") +
  guides(color=guide_legend(override.aes=list(fill=NA))) +
  theme_minimal() +
  annotation_scale(location = "br", width_hint = 0.5, style="ticks", text_cex = 1, bar_cols="white") +
  annotation_north_arrow(location = "br", which_north = "true",  
                         #pad_x = unit(0.75, "in"), 
                         pad_y = unit(0.2, "in"),
                         style = north_arrow_orienteering) +
  theme(text=(element_text(size=18))) +
  coord_sf(ylim=c(953997.5, 1285584), xlim=c(583996, 890947.5 )) +
  geom_sf_label(aes(label = ZONEID, color=ZONEID), nudge_x = c(-6000, 7000, 7000, -10000, 1000, 0, 0) + rep(40000, 7), nudge_y = c(0, -7000, -7000, -40000, -1000, 15000, 0) - rep(40000, 7), size=8, show.legend = F) +
  labs(x="", y="", title="SST change: 6/11/2005 to 6/18/2005")


####read global sst and try to plot
###download from nc
URL <- paste("http://iridl.ldeo.columbia.edu/SOURCES/.Models/.SubX/.EMC/.GEFS/.hindcast/.ts/S/%2818%20Aug%202016%29VALUES/L/%280.5%29VALUES/dods")

nc <- open.nc(URL)
nc.ts <- var.get.nc(nc, "ts")
nc.ts <- nc.ts - 273.15

sst_global <- t((brick(nc.ts[,,,1], xmn=-90, xmx=90, ymn=0, ymx=360)))
#ens_1_d1 <- brick(nc.ts[,,i,,j])
proj4string(sst_global) <- CRS("+init=epsg:4326")
sst_global <- mean(sst_global)
plot(sst_global)

sst_global_prj <- projectRaster(m, crs="+proj=aea +lat_0=34 +lon_0=-78 +lat_1=27.3333333333333 +lat_2=40.6666666666667 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs")
sst_global_prj <- data.frame(rasterToPoints(sst_global_prj))

x1 <- crop(sst_global, extent(180,360, -90, 90))
x2 <- crop(sst_global, extent(0, 180, -90, 90))   
extent(x1) <- c(-180, 0, -90, 90)
m <- merge(x1, x2)

m <- crop(m, extent(-178, 358, -88, 88))

#sst_global1 <- stars::st_as_stars(sst_global, proxy=FALSE)
#sst_global1 <- st_as_sf(as(sst_global, "SpatialPixelsDataFrame"), axes=T)

sst_global1 <- data.frame(rasterToPoints(m))
coords = data.frame(lat = sst_global1$x, lon = sst_global1$y)
spPoints <- SpatialPointsDataFrame(coords, 
                                   data = data.frame(data = sst_global1$layer), 
                                   proj4string = CRS("+init=epsg:4326"))
points_sf <- as(spPoints, "sf")
#points_sf <- st_crop(points_sf, st_collection_extract(visible))
points_sf <- st_intersection(st_make_valid(points_sf), st_buffer(circle_longlat, -1))
points_sf <- st_transform(points_sf, st_crs(extentt_orth))

#polys = as(SpatialPixelsDataFrame(spPoints, spPoints@data, tolerance = 0.149842),"SpatialPolygonsDataFrame")
#polys_sf = as(polys, "sf")
#polys_sf <- st_intersection(st_make_valid(polys_sf), st_buffer(circle_longlat, -0.15))
#polys_sf <- st_transform(polys_sf, st_crs(extentt_orth))


sma_map_zoom <- ggplot(usa_coast_prj) +
  #geom_sf(data=circle, color=NA, fill="aliceblue")+
  geom_tile(data = sst_global_prj, aes(x=x, y=y, color=layer, fill=layer)) +
  scale_color_gradientn(colors=rev(c("#fce07e", "#78b5cc", "#5d44c4")), limits=c(10,35)) +
  scale_fill_gradientn(colors=rev(c("#fce07e", "#78b5cc", "#5d44c4")), limits=c(10,35)) +
  geom_sf(data=usa_coast_prj, color=NA, fill="gray20") +
  #geom_sf(data=extentt_orth, fill=NA, color="black", size=1.5)+
  #annotate(geom="rect", xmin=-78, xmax=-63, ymin=34, ymax=46, color="black", fill="green", alpha=0.2) +
  theme_void() +
  #annotation_scale(location = "br", width_hint = 0.5, style="ticks", text_cex = 1) +
  #annotation_north_arrow(location = "br", which_north = "true",  
  #pad_x = unit(0.75, "in"), 
  #pad_y = unit(0.2, "in"),
  #style = north_arrow_orienteering) +
  #theme(text=(element_text(size=14)),
  #legend.position = "") +
  coord_sf(crs="+proj=aea +lat_0=34 +lon_0=-78 +lat_1=27.3333333333333 +lat_2=40.6666666666667 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs", xlim=c(-400437.4,2100563), ylim=c(-897430,1602570))
#coord_sf(ylim=c(242977.8  , 1285584))
#coord_sf(crs=ortho, ylim=c(-6070835, 6070992), xlim=c(-6026747,6070992)) 
sma_map_zoom

setwd("C:/Users/jstepanuk/Dropbox/my_papers/InPrep_Stepanuketal_MAPPdemo/")
pdf("forecast_zoom.pdf", width=2, height=2)
sma_map_zoom
dev.off()


###### gam outputs
###gam summary plots
library(voxel)
library(ggplot2)
library(ggpubr)
library(ggplotify)
library(mgcViz)

agammn_reg_sptr


viz <- getViz(agammn_reg_sptr)
print(plot(viz, allTerms = T), pages = 1)

trt <- plot(viz, allTerms = T) +
  l_points(shape=1, alpha=0.1) +
  l_fitLine(linetype = 1, colour="red")  +
  l_ciPoly() +
  l_ciBar() +
  l_rug(alpha=0.8) +
  theme_minimal() 

print(trt, pages = 1)


layout(matrix(1:4, ncol = 2, byrow = TRUE))
plot.gam(agammn_reg_sptr, select = 1, scale=0, rug=T, 
         cex=1.2, scheme=1, shade=T,shade.col='gray70', 
         ylab = "s(SST)", xlab = "SST")
plot.gam(agammn_reg_sptr, select = 2, scale=0, rug=T, 
         cex=1.2, scheme=1, shade=T,shade.col='gray70',
         ylab = "s(Log(Bathymetry))", xlab="Log(Bathymetry)")
plot.gam(agammn_reg_sptr, select = 3, scale=0, rug=T, 
         cex=1.2, scheme=1, shade=T,shade.col='gray70',
         ylab = "s(Dist. From Shore)", xlab="Dist. From Shore")
layout(1)

