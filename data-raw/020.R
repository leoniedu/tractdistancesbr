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

#graph_mc <- weight_streetnet (mc, wt_profile = "motorcar", wt_profile_file = "profile.json", id_col = "id")

# dups_mc <- graph_mc%>%
#   ungroup()%>%
#   group_by(from_lat, from_lon, to_lat, to_lon, highway, waterway,feature_id, )%>%filter(n()>1)%>%
#   arrange(from_lat, from_lon, to_lat, to_lon, highway)

mw <- load_pbf_parquet(fname_pbf_parquet,quo(waterway%in%valid_waterways))%>%
  mutate(highway=waterway)


# graph_mw <- weight_streetnet (mw, wt_profile = "motorcar", wt_profile_file = "profile.json", id_col = "id")
#
# dups_mw <- graph_mw%>%
#   ungroup()%>%
#   group_by(from_lat, from_lon, to_lat, to_lon, highway, waterway,feature_id, )%>%filter(n()>1)%>%
#   arrange(from_lat, from_lon, to_lat, to_lon, highway)


# mc_net <- weight_streetnet (mc, wt_profile = "motorcar", wt_profile_file = "profile.json", id_col = "id")
## connect mw to mc
mw_v <- st_extract_vertices(mw)

# mw_v_sample <- mw_v%>%
#   group_by(feature_id)%>%
#   slice(unique(
#     as.integer(seq(1,n(), length=pmax(2, round(n()/100)))))
# )

library(furrr)
library(tictoc)
plan("multisession", workers=2)
#plan("sequential")
tic()
tmp <- connect_points_to_network(lines_sf = mc, points_sf = mw_v, max_distance = 25, way_id_column = "id")
toc()
# ggplot(mw) +
#   geom_sf(color="blue") +
#   geom_sf(data=mc, color="black")+
#   geom_sf(data=uf, fill="transparent", lwd=2)+
#   geom_sf(data=st_cast(tmp$artificial_edges, "POINT"), color="red", alpha=1)

uf_buffer <- st_buffer(uf, dist = units::set_units(10000, "m"))%>%
  st_transform(crs = st_crs(mc))

complete_sf_0 <- tmp$complete_network%>%
  bind_rows(mw)%>%
  st_intersection(uf_buffer%>%select(geom))%>%
  m2poly()%>%
  distinct(geometry, highway, feature_id, .keep_all = TRUE)



graph_mw <- weight_streetnet (complete_sf_0, wt_profile = "motorcar", wt_profile_file = "profile.json", id_col = "id")

dim(complete_sf_0)
complete_sf <- complete_sf_0%>%left_join(graph_mw%>%as_tibble()%>%distinct(id=way_id, component))%>%
  m2poly()%>%
  distinct(geometry, highway, feature_id, .keep_all = TRUE)%>%
  mutate(way=recode_way(highway))
dim(complete_sf)

ggplot(complete_sf) +
  geom_sf(aes(color=way, linetype=component>1), alpha=1/2)+
  geom_sf(data=uf, fill="transparent", lwd=2)



readr::write_rds(mw, fname_waterway_rds)
readr::write_rds(mc, fname_highway_rds)
readr::write_rds(complete_sf, fname_complete_rds)
if (Sys.getenv("DODGR_CACHE")=="TRUE") dodgr::dodgr_save_streetnet(graph_mw, fname_graph_cache)
readr::write_rds(graph_mw, fname_graph)


stop()

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
