# 1. Read main pbf
# 3. save
source("data-raw/000.R")

library(geoarrow)
library(arrow)
library(sf)
library(dodgr)

m <- arrow::open_dataset(fname_pbf_parquet)%>%
  filter(!is.na(highway))%>%
  filter(highway%in%(weighting_profiles$weighting_profiles$way%>%unique))%>%
  st_as_sf()


m <- m2poly(m)
readr::write_rds(m, fname_highway_rds)

mw <- arrow::open_dataset(fname_pbf_parquet)%>%
  filter(!is.na(waterway))%>%
  st_as_sf()
readr::write_rds(mw, fname_waterway_rds)

