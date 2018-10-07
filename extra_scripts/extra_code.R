#extra stuff

# run a script that's just an animation of the nn network with different time cutoffs
```{r time_mat, fig.margin = TRUE, echo = FALSE}
time_mat <- dist_mat %>%
  as_tbl_graph(directed = FALSE) %E>%
  filter(!edge_is_loop()) %>%
  rename(distance = weight) %>%
  filter(distance <= 3600 * 6) %>%
  #mutate(max_dist = case_when(
  #    distance <= 3600 * 1 ~'1 Hours',
  #    distance <= 3600 * 2 ~'2 Hours',
  #    distance <= 3600 * 3 ~'3 Hours',
  #    distance <= 3600 * 4 ~'4 Hours',
  #    distance <= 3600 * 5 ~'5 Hours',
  #    distance <= 3600 * 6 ~'6 Hours',
  #    distance <= 3600 * 7 ~'7 Hours',
  #    distance <= 3600 * 8 ~'8 Hours')) %>%
  as_tibble %>%
  inner_join(activate(swsn, edges), ., by = c('from', 'to'))

ggraph(time_mat, 'manual', node.positions = pts) +
  geom_edge_link(alpha = .1) +
  theme_void() +
  facet_edges(~time) +
  coord_equal()
```

Something with sst anomalies
```{r eval = F}
library(remote)
sst_jja <- brick('~/Downloads/sst.mnmean.v4.nc') %>%
  .[[505:1752]] %>%
  .[[sort.int(c(seq(6, 1248, 12),seq(7, 1248, 12),seq(8, 1248, 12)))]] %>%
  stackApply(rep(1:104, each = 3), mean) %>%
  anomalize

test <- calc(sst_jja, fun=function(x) cor(x,obs_reof$amplitude[,1]))
plot(test)
cor(sst_jja[1,1], t1$EOF1)


sst_ann <- brick('~/Downloads/sst.mnmean.v4.nc') %>%
  .[[508:1755]] %>%
  stackApply(rep(1:104, each = 12), mean) %>%
  anomalize
```


## Environmental Data
Import higher resolution environmental layers and crop the reof files. Define a function that will import all the prism normals for a given variable name. outputs a brick.
```{r import_prism}
get_normals <- function(var){
  paste0('data/PRISM/PRISM_', var, '_30yr_normal_800mM2_all_asc/') %>%
    list.files(pattern = '\\.asc$', full.names = TRUE) %>%
    .[1:12] %>% # the last file in the list is annual values, drop it
    purrr::map(raster) %>% # load the rasters from file list
    purrr::map(crop, bbox_wus) %>% # crop to study area
    brick # combine list of rasters into a brick
}
```

Use the above function to import precipitation and temperature range normals. Use the biovars function from dismo to calculate 19 bioclimatic predictor variables from monthly precipitation and temperature data.
```{r bioclim_pca, cache = TRUE}
prec <- get_normals('ppt')
tmin <- get_normals('tmin')
tmax <- get_normals('tmax')

# calculate bioclim variables from the monthly normals
bio_clim <- biovars(prec, tmin, tmax)
```

Reduce the dimensionality of the bioclimatic data using an R-mode PCA.
```{r cache = TRUE}
clim_pca <- RStoolbox::rasterPCA(bio_clim, spca = TRUE, nComp = 6)
```
PC1 is general heat and dryness of the summer, pc2 is the seasonality index, pc3 is an index of diurnal variability. So its basically 3 different time scales.
```{r}
clim_pca$model$loadings
```

Retain the leading 4 PCs.
```{r}
screeplot(clim_pca$model, npcs = 10, type = 'lines')
```
