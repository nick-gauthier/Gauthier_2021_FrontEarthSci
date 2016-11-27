# Southwest Climate Variability
Nicolas Gauthier  





```r
library(magrittr)
library(raster)
library(ncdf4)
library(spacetime)
library(rasterVis)
```


```r
#elev <- getData('alt', country = 'USA') %>% crop(extent(c(-113.5, -106.5, 31, 37.5))) # if you need to download elevation data
elev <- raster('USA1_msk_alt.grd') %>% crop(extent(c(-113.5, -106.5, 31, 37.5)))
levelplot(elev, margin = F)
```

![](present_day_climate_files/figure-html/unnamed-chunk-2-1.png)<!-- -->




```r
box <- extent(c(-113.5, -106.5, 31, 37.5))

brick('~/Downloads/spei03.nc') %>%
  crop(box, snap = 'out') %>% 
  as('STFDF') %>%
  eof('spatial') %>%
  brick %>%
  extract2(1:6) %>% 
  resample(elev) %>%
  levelplot(par.settings = RdBuTheme(), contour = T)
```

![](present_day_climate_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
brick('~/Downloads/spei06.nc') %>%
  crop(box, snap = 'out') %>% 
  as('STFDF') %>%
  eof('spatial') %>%
  brick %>%
  extract2(1:6) %>% 
  resample(elev) %>%
  levelplot(par.settings = RdBuTheme(), contour = T)
```

![](present_day_climate_files/figure-html/unnamed-chunk-3-2.png)<!-- -->

```r
brick('~/Downloads/spei12.nc') %>%
  crop(box, snap = 'out') %>% 
  as('STFDF') %>%
  eof('spatial') %>%
  brick %>%
  extract2(1:6) %>% 
  resample(elev) %>%
  levelplot(par.settings = RdBuTheme(), contour = T)
```

![](present_day_climate_files/figure-html/unnamed-chunk-3-3.png)<!-- -->

```r
brick('~/Downloads/spei48.nc') %>%
  crop(box, snap = 'out') %>% 
  as('STFDF') %>%
  eof('spatial') %>%
  brick %>%
  extract2(1:6) %>% 
  resample(elev) %>%
  levelplot(par.settings = RdBuTheme(), contour = T)
```

![](present_day_climate_files/figure-html/unnamed-chunk-3-4.png)<!-- -->


```r
#stfdf <- prec 
```



```r
## attempt to compute EOFs
#eof_time <- eof(stfdf, 'temporal')
#eof_space <- eof(stfdf, 'spatial') %>% brick %>% extract2(1:6) %>% resample(elev)
#eof(stfdf, 'spatial', returnEOFs = F) %>% screeplot(type = 'lines')
```

```r
#levelplot(par.settings = RdBuTheme(), contour = T)
```



```r
#levelplot(eof_space, par.settings = RdBuTheme(), contour = F)
#levelplot(eof_space, par.settings = RdBuTheme(), contour = T)
```


```r
#
#eof_space %>% abs %>% sum %>% levelplot(margin = F)
```

