# Southwest Social Networks
Nick Gauthier  
# Data import

First import the SWSN attribute file. Use tidyverse packages for data munging.

Site coordinates are in UTM, so first use rgdal to reproject to LatLon.

```r
library(tidyverse)
library(rgdal)

swsn.pts <- read_csv('Data/attributes_orig.csv') %>% 
  select(easting = EASTING, northing = NORTHING) %>%
  SpatialPoints(proj4string=CRS("+proj=utm +zone=12 +datum=WGS84")) %>%
  spTransform(CRS("+proj=longlat +datum=WGS84")) %>% 
  coordinates %>%
  data.frame
```

Now reimport the attribute file, select the relevant data, and combine with the reprojected site coordinates.

```r
swsn.attr <- read_csv('Data/attributes_orig.csv') %>%
  select(ID = SWSN_ID, site = SWSN_Site, macro = Macro, micro = Micro) %>%
  cbind(swsn.pts)
```

Now define a function to import the SWSN adjacency matrix for a given time step. This function imports the adjacency matrix, keeps only those connections with >= 75% similarity, and creates an igraph object. Then it adds attribute data from above to the graph object.

```r
library(igraph)

readSWSN <- function(net){
  net.in <- read.csv(net, row.names = 1, check.names = F) 
  net.in[net.in < .75] <- 0
  net.in <- net.in %>% 
    as.matrix %>%
    graph_from_adjacency_matrix(mode = 'undirected', weighted = T, diag = F)
  
  ord <- match(V(net.in)$name, swsn.attr$site)

  V(net.in)$lon <- swsn.attr[ord, 5]
  V(net.in)$lat <- swsn.attr[ord, 6]
  V(net.in)$region <- swsn.attr[ord, 3] %>% as.character
  
  return(net.in)
}
```

Use the function to import the network datasets.

```r
ad1200 <- readSWSN('Data/AD1200sim.csv')
ad1250 <- readSWSN('Data/AD1250sim.csv')
ad1300 <- readSWSN('Data/AD1300sim.csv')
ad1350 <- readSWSN('Data/AD1350sim.csv')
ad1400 <- readSWSN('Data/AD1400sim.csv')
```

# Plotting

First get a terrain basemap to plot the networks over. The terrain-background basemap from Stamen is a nice choice. Download this map and store a ggmap plot of it.

```r
library(ggmap)
terrain.background <- get_map(location = c(left = -113.5, right = -106.5, bottom = 31, top = 37.5),
  zoom = 8,
  color = "color",
  source = "stamen",
  maptype = "terrain-background")

map <- ggmap(terrain.background)  +
  labs(x = "Longitude", y = "Latitude")
```

Now plot the networks.



## More Minimal network maps


```r
library(GGally)
```

```
## 
## Attaching package: 'GGally'
```

```
## The following object is masked from 'package:dplyr':
## 
##     nasa
```

```r
library(ggmap)
```

```
## Google Maps API Terms of Service: http://developers.google.com/maps/terms.
```

```
## Please cite ggmap if you use it: see citation("ggmap") for details.
```

```r
library(maps)
```

```
## 
## Attaching package: 'maps'
```

```
## The following object is masked from 'package:purrr':
## 
##     map
```

```r
states <- map_data('state', region = c('arizona', 'new mexico'))
  
base <- ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, group = region), color = 'black', fill = 'white') +
  coord_quickmap() +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude")

n1 <- ggnetworkmap(base, ad1200, great.circles = T, size = .5, segment.alpha = I(.5)) +
  geom_label(x = -106, y = 35, label = 'AD 1200')
```

```
## Loading required package: network
```

```
## network: Classes for Relational Data
## Version 1.13.0 created on 2015-08-31.
## copyright (c) 2005, Carter T. Butts, University of California-Irvine
##                     Mark S. Handcock, University of California -- Los Angeles
##                     David R. Hunter, Penn State University
##                     Martina Morris, University of Washington
##                     Skye Bender-deMoll, University of Washington
##  For citation information, type citation("network").
##  Type help("network-package") to get started.
```

```
## 
## Attaching package: 'network'
```

```
## The following objects are masked from 'package:igraph':
## 
##     %c%, %s%, add.edges, add.vertices, delete.edges,
##     delete.vertices, get.edge.attribute, get.edges,
##     get.vertex.attribute, is.bipartite, is.directed,
##     list.edge.attributes, list.vertex.attributes,
##     set.edge.attribute, set.vertex.attribute
```

```
## Loading required package: sna
```

```
## Loading required package: statnet.common
```

```
## sna: Tools for Social Network Analysis
## Version 2.4 created on 2016-07-23.
## copyright (c) 2005, Carter T. Butts, University of California-Irvine
##  For citation information, type citation("sna").
##  Type help(package="sna") to get started.
```

```
## 
## Attaching package: 'sna'
```

```
## The following objects are masked from 'package:igraph':
## 
##     betweenness, bonpow, closeness, components, degree,
##     dyad.census, evcent, hierarchy, is.connected, neighborhood,
##     triad.census
```

```
## Loading required package: geosphere
```

```r
n2 <- ggnetworkmap(base, ad1250, great.circles = T, size = .5, segment.alpha = I(.5)) +
  geom_label(x = -106, y = 35, label = 'AD 1250')

n3 <- ggnetworkmap(base, ad1300, great.circles = T, size = .5, segment.alpha = I(.5)) +
  geom_label(x = -106, y = 35, label = 'AD 1300')

n4 <- ggnetworkmap(base, ad1350, great.circles = T, size = .5, segment.alpha = I(.5)) +
  geom_label(x = -106, y = 35, label = 'AD 1350')

n5 <- ggnetworkmap(base, ad1400, great.circles = T, size = .5, segment.alpha = I(.5)) +
  geom_label(x = -106, y = 35, label = 'AD 1400')
```
Get basemap for elevation.

```r
# courtesy R Lovelace
library(raster)
```

```
## 
## Attaching package: 'raster'
```

```
## The following object is masked from 'package:dplyr':
## 
##     select
```

```
## The following object is masked from 'package:tidyr':
## 
##     extract
```

```r
library(maptools)
```

```
## Checking rgeos availability: TRUE
```

```r
states.ply <- map('state', region = c('arizona', 'new mexico'), fill = T, plot = F)
IDs <- sapply(strsplit(states.ply$names, ":"), function(x) x[1])
states.ply <- map2SpatialPolygons(states.ply, IDs=IDs)

ggmap_rast <- function(map){
  map_bbox <- attr(map, 'bb') 
  .extent <- extent(as.numeric(map_bbox[c(2,4,1,3)]))
  my_map <- raster(.extent, nrow= nrow(map), ncol = ncol(map))
  rgb_cols <- setNames(as.data.frame(t(col2rgb(map))), c('red','green','blue'))
  red <- my_map
  values(red) <- rgb_cols[['red']]
  green <- my_map
  values(green) <- rgb_cols[['green']]
  blue <- my_map
  values(blue) <- rgb_cols[['blue']]
  stack(red,green,blue)
}


ggplot_build(n1)$layout$panel_ranges[[1]]$x.range 
```

```
## [1] -115.3997 -102.4102
```

```r
ggplot_build(n2)$layout$panel_ranges[[1]]$y.range
```

```
## [1] 31.06377 37.28437
```

```r
terrain.background <- get_map(location = c(left = -115.3997, right = -102.4102, bottom = 31.06377, top = 37.28437),
  zoom = 8,
  color = "bw",
  source = "stamen",
  maptype = "terrain-background")
```

```
## 66 tiles needed, this may take a while (try a smaller zoom).
```

```
## Source : http://tile.stamen.com/terrain-background/8/45/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/46/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/47/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/48/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/49/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/50/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/51/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/52/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/53/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/54/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/55/99.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/45/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/46/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/47/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/48/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/49/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/50/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/51/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/52/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/53/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/54/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/55/100.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/45/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/46/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/47/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/48/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/49/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/50/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/51/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/52/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/53/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/54/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/55/101.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/45/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/46/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/47/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/48/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/49/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/50/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/51/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/52/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/53/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/54/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/55/102.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/45/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/46/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/47/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/48/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/49/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/50/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/51/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/52/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/53/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/54/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/55/103.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/45/104.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/46/104.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/47/104.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/48/104.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/49/104.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/50/104.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/51/104.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/52/104.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/53/104.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/54/104.png
```

```
## Source : http://tile.stamen.com/terrain-background/8/55/104.png
```

```r
terrain.rast <- ggmap_rast(map = terrain.background) # convert stamen map to raster object
state.only <- mask(terrain.rast, states.ply) # clip to bounds of census tracts


# prep raster as a data frame for printing with ggplot
sw.df <- data.frame(rasterToPoints(state.only))
m1 <- ggplot(sw.df) + 
  geom_point(aes(x=x, y=y, col=rgb(layer.1/255, layer.2/255, layer.3/255))) + 
  scale_color_identity() +
  geom_polygon(data = states, aes(x = long, y = lat, group = region), color = 'black', fill = NA) +
  coord_quickmap() +
  geom_point(aes(x = easting, y = northing), size = 1, data = swsn.pts) +
  theme_nothing()
```






```r
multiplot(n1, n2, n3, n4, n5, m1, layout = matrix(c(1,2,3,4,5,6), byrow = T, nrow = 3))
```

![](network_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

