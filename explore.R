#install.packages("ismev")               # if not installed!
#library(ismev)
library(magrittr)
library(mgcv)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)

# background map for later
world <- ne_countries(scale = "medium", returnclass = "sf")

# Read in the data
anoph <- read_csv('Anopheles clean.csv')[,-c(1:2)]

# eliminate recent years which are data deficient (relatively)
anoph %<>% filter(Year < 2015)

# species factor
anoph %<>% transform(Species = as.factor(Species))


## plot all the unique locations
# where are these lads?
geo_unique <- as.matrix(unique(anoph[,c("Long", "Lat")]))
geo_unique <- st_sfc(st_multipoint(geo_unique))
st_crs(geo_unique) <- 4326

# make a map
ggplot(world) +
  geom_sf() +
  geom_sf(data=geo_unique, size=0.3) +
  coord_sf(xlim=st_bbox(geo_unique)[c(1,3)],
           ylim=st_bbox(geo_unique)[c(2,4)], expand=FALSE)



## now fiddle with per species plots, this is BAD CODE
# lifted from https://r-spatial.github.io/sf/reference/tidyverse.html
#  (storm example)
# data, but make it spatial
geo_anoph_all <- anoph %>%
  st_as_sf(coords=c("Long", "Lat"), crs=4326)

# this is a huge faff, first find per species/year maxes
x <- geo_anoph_all %>% group_by(Species, Year) %>% nest
ff <- function(tr){
  co <- st_coordinates(tr)
  bind_cols(tr[which.max(co[, 2]), ],
        Lat = max(co[, 2]))
}
trs <- lapply(x$data, ff)
trs <- do.call(bind_rows, trs) %>% st_sf(crs=4326)
trs.sf = bind_cols(x[,1:2], trs)
# get that back into format
geo_anoph_max <- st_sf(trs.sf)

# now take each species and turn it into a line, with years as vertices
x <- geo_anoph_max %>% group_by(Species) %>% nest
geo_anoph_max <- lapply(x$data, function(tr){
  tr <- arrange(tr, Year)
  st_cast(st_combine(tr), "LINESTRING")[[1]]
}) %>%
    st_sfc(crs = 4326)
trs.sf = st_sf(x[,1:2], trs)


# plot maps, one species per facet
ggplot() +
  geom_sf(data=world) +
  geom_sf(data=geo_anoph_max) +
  facet_wrap(~Species) +
  theme_minimal() +
  coord_sf(xlim=st_bbox(geo_anoph_max)[c(1,3)],
           ylim=st_bbox(geo_anoph_max)[c(2,4)], expand=FALSE)

