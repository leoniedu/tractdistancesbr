# 4. read cropped
# 7. contract
# 8. save
source("data-raw/000.R")
library(geoarrow)
library(arrow)
library(sf)
library(dodgr)

mc <- load_pbf_parquet(fname_pbf_parquet,quo(!is.na(highway)))%>%
  filter(highway%in%(wibge$weighting_profiles$way))

mw <- load_pbf_parquet(fname_pbf_parquet,quo(!is.na(waterway)))%>%
  mutate(highway=waterway)%>%
  filter(highway%in%(wibge$weighting_profiles$way))

mc_net <- weight_streetnet (mc, wt_profile = "motorcar", wt_profile_file = "profile.json", id_col = "id")
## connect mw to mc
mw_v <- st_extract_vertices(mw)
mw_v_sample <- mw_v%>%
  group_by(feature_id)%>%
  slice(unique(
    as.integer(seq(1,n(), length=pmax(10, n()/1000))))
)

tmp <- connect_points_to_network(lines_sf = mc, points_sf = head(mw_v_sample,20), max_distance = 25, way_id_column = "id")



graph_w <- weight_streetnet (mw, wt_profile = "motorcar", wt_profile_file = "profile.json", id_col = "id")
graph_r <- weight_streetnet (mc, wt_profile = "motorcar", wt_profile_file = "profile.json", id_col = "id")

## connect
## clear_dodgr_cache()
# This function should generally not be needed, except if graph structure has been directly modified other than through dodgr functions; for example by modifying edge weights or distances. Graphs are cached based on the vector of edge IDs, so manual changes to any other attributes will not necessarily be translated into changes in dodgr output unless the cached versions are cleared using this function.


res_r <- check_uf_intersections(uf_data = uf, map_data = mc, graph_data = graph_r)

res_w <- check_uf_intersections(uf_data = uf, map_data = mw, graph_data = graph_w)

res_r$map <- res_r$map%>%mutate(component=dense_rank(component), id=dense_rank(id))
res_w$map <- res_w$map%>%mutate(component=dense_rank(component), id=dense_rank(id))

map_rw <- bind_rows(
  res_r$map,
  res_w$map%>%
    mutate(id=id+max(res_r$map$id), component=component+max(res_r$map$component)))%>%
  mutate(id=as.character(id))


readr::write_rds(res_w$map, fname_waterway_rds)
readr::write_rds(res_r$map, fname_highway_rds)
readr::write_rds(map_rw, fname_complete_rds)


readr::write_rds(mc2, fname_highway_rds)

## Important: do not contract, since the snap to the network depends on the vertices.
