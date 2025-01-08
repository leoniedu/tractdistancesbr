# 9. load contracted
# 10. load tract positions
# 11. calculate distances
source("data-raw/000.R")
#plan(sequential)

highways_sf <- readr::read_rds(fname_highway_rds)
waterways_sf <- readr::read_rds(fname_waterway_rds)


highways_sf_simple <- st_simplify_net(highways_sf)
highways_v_simple <- st_extract_vertices(highways_sf_simple)


waterways_sf_simple <- st_simplify_net(waterways_sf)


waterways_sf$id <- 1:nrow(waterways_sf)
waterways_sf$highway <- waterways_sf$waterway
waterways_v <- spatialEco::extract.vertices(waterways_sf)

waterways_sf <- waterways_sf%>%
  filter(highway%in%wibge$weighting_profiles$way)
complete_sf <- readr::read_rds(fname_complete_rds)

highways_net <- weight_streetnet (highways_sf,
                                  wt_profile_file = "profile.json",
                                  wt_profile="motorcar",
                                  id_col = "id")
waterways_net <- weight_streetnet (waterways_sf,
                                   wt_profile_file = "profile.json",
                                   wt_profile="motorcar",
                                   id_col = "id")

waterways_vertices_0 <- dodgr::dodgr_vertices(waterways_net)%>%
  sf::st_as_sf(coords=c("x", "y"))
highways_vertices_0 <- dodgr::dodgr_vertices(highways_net)%>%
  sf::st_as_sf(coords=c("x", "y"))


waterways_links <- st_nearest_feature(waterways_vertices_0, highways_vertices_0)
waterways_links_dists <- sf::st_distance(waterways_vertices_0, highways_vertices_0[waterways_links,], by_element = TRUE)

waterways_vertices <- waterways_vertices_0%>%
  group_by(component)%>%
  slice(as.integer(seq(1,n(), length=pmax(1, n()/1000))))

pts_0 <- match_pts_to_graph(highways_net, st_coordinates(waterways_vertices), distances = TRUE)%>%
  ungroup%>%
  mutate(row_id=1:n())
pts <- pts_0%>%filter(abs(d_signed)<1000
                      , abs(d_signed)>0
                      )
hw_net <- add_nodes_to_graph_labeled(highways_net, st_coordinates(waterways_vertices)[pts$row_id,], )




## points to connect
p0 <- tracts_points%>%arrange(setor_lat)
p1 <- tracts_points%>%arrange(setor_lon)
p <- bind_rows(#head(p0), tail(p0),
               head(p1), tail(p1))%>%unique()


## connect points to network
grw_p0 <- connect_points_to_network(points_sf = p
                                    , lines_sf = complete_sf,
                                    way_id_column = "id", track_memory = TRUE)

## idea: connect all components that are assigned to points
## components to connect
v <- unique(c(table(grw_p0$complete_network$component)%>%sort()%>%tail(2)%>%names,grw_p0$artificial_edges$component))

## filter lines_sf
filtered_sf <- grw_p0$complete_network[grw_p0$complete_network$component%in%v,]

## dodgr net
grw_filtered <- weight_streetnet (filtered_sf, wt_profile = wibge, id_col = "id")


## connect components
tic()
tmp2 <- connect_network_components(lines_sf = filtered_sf, dodgr_net = grw_filtered, way_id_column = "id", track_memory = TRUE)
toc()


## new dodgr net with the connected components

## dodgr net
grw_connected <- weight_streetnet (tmp2$complete_network, wt_profile = wibge, id_col = "id")

## Fix: for some reason (investigate), it finds more than one component, despite connections.
table(grw_connected$component)
## keeping the first component

grw_connected1 <- grw_connected[grw_connected$component==1,]

## calculate path
path <- dodgr::dodgr_paths(grw_connected1, from = st_coordinates(p[1,]), to=st_coordinates(p[nrow(p),]), vertices=FALSE)

d <- grw_connected1[unlist(path),];d$seq <- 1:nrow(d)

## calculate distance
dodgr::dodgr_distances(grw_connected1, from=st_coordinates(p[1,]), to=st_coordinates(p[nrow(p),]))

library(ggplot2)
ggplot(data=tmp2$complete_network%>%
         mutate(
           is_waterway=!is.na(waterway),
           artificial=coalesce(artificial, FALSE))) +
  geom_sf(aes(color=waterway))+
  scale_color_manual(values=c("gray", "pink"))+
  #geom_sf(data=map_connected_p$artificial_edges, color="purple", linewidth=1)+
  geom_sf(data=p, color="lightblue", alpha=1/5)+
  #geom_sf(data=cp$artificial_edges, color="red", linetype=2)+
  geom_sf(data=p, color="blue", size=1) +
  geom_sf(data=p[c(1,nrow(p)), ], color="red", size=3)+
  geom_segment(data=grw_connected1[unlist(path),], color="red", linewidth=1, alpha=1/3,
               aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat))
stop()



library(tictoc)

print(v)
print(length(v))

grw_p0 <- connect_points_to_network(points_sf = tracts_points%>%slice_sample(n=100)
                                    , lines_sf = complete_sf,
                                    way_id_column = "id", track_memory = TRUE)

grw_p1 <- weight_streetnet (grw_p0$complete_network, wt_profile = wibge, id_col = "id")





grw <- weight_streetnet (complete_sf, wt_profile = wibge, id_col = "id")

comps <- grw%>%group_by(component)%>%summarise(d=sum(d))%>%
  arrange(component)%>%
  mutate(p=d/sum(d), cp=cumsum(p))
v <- comps%>%
  filter(cp<.95)%>%
  pull(component)

library(tictoc)

print(v)
print(length(v))

grw_p0 <- connect_points_to_network(points_sf = tracts_points%>%slice_sample(n=100)
                                    , lines_sf = complete_sf,
                                    way_id_column = "id", track_memory = TRUE)

grw_p1 <- weight_streetnet (grw_p0$complete_network, wt_profile = wibge, id_col = "id")




tic()
tmp2 <- connect_network_components(lines_sf = complete_sf, dodgr_net = grw[grw$component%in%v,], way_id_column = "id", track_memory = TRUE)
toc()



tracts_br_2022 <- readRDS("~/github/tractdistancesbr/data-raw/tracts_br_2022.rds")

tracts <- tracts_br_2022%>%
  filter(code_state==ufnow$uf_codigo)%>%
  mutate(setor=as.character(code_tract))%>%
  semi_join(orce::pontos_setores%>%sf::st_drop_geometry())
rm(tracts_br_2022)
gc()

tracts_points <- orce::pontos_setores%>%semi_join(tracts%>%sf::st_drop_geometry())%>%
  distinct(setor, .keep_all = TRUE)

g <- grw[grw$component%in%v,]
p <- ggplot(data=g) +
  geom_segment(aes(x=from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component), alpha=!highway%in%c("river", "canal")))+
  #guides(color="none")+
  geom_sf(data=tracts_points, alpha=1/10)+
  geom_sf(data=uf, lwd=1, fill="transparent")+
  geom_text(aes(x=lon, y=lat, label=component), data=g%>%group_by(component)%>%summarise(lat=mean(to_lat), lon=mean(to_lon)))
p+theme_minimal()

##testing point connections
tmp3 <- connect_points_to_network(points_sf = tracts_points#%>%slice_sample(n=10)
                                  , lines_sf = tmp2$complete_network,
                                 way_id_column = "id", track_memory = TRUE)

tmp4 <- weight_streetnet (tmp3$complete_network, wt_profile = wibge, id_col = "id")




p <- ggplot(data=grw[grw$component%in%v,]) +
  #geom_sf(data=waterways_sf, color="gray", alpha=.5)+
  geom_sf(data=tmp2$artificial_edges, color="purple", linetype=2, size=10)+
  geom_segment(aes(x=from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component), alpha=!highway%in%c("river", "canal")))+
  geom_sf(data=tracts_points, pch="*")
p

##testing point connections
tmp <- connect_points(points = tracts_points%>%slice_sample(n=10), streets = tmp2$complete_network, street_id_col = "id", point_id_col = "setor", waterways = waterways_sf)%>%filter(distance_to_street>1000)

d <- tmp%>%arrange(desc(distance_to_street))%>%slice(10)
bb <- st_bbox(d)
#p <- ggplot(data=tmp%>%arrange(desc(distance_to_street))%>%slice(1))+
p <- ggplot(data=d)+
  geom_sf(data=waterways_sf, color="lightblue", alpha=.8)+
  geom_segment(aes(x=from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component)), data=grw)+
  geom_sf(data=d, color="purple", linetype=2) +
  geom_sf(data=tracts_points)+  coord_sf(xlim=bb[c(1,3)], ylim=bb[c(2,4)])
p



p <- ggplot(data=grw[grw$component>2,]) +
  geom_segment(aes(x=from_lon, y=from_lat, xend = to_lon, yend = to_lat, color=factor(component)))+
  geom_sf(data=tmp2$artificial_edges)
p
p+geom_point(data=gr[gr$way_id=="1037",][1,], aes(x=from_lon, y=from_lat), size=3, color=scales::alpha("red",.1))

# bb <- st_bbox(tmp2$complete_network)
# p <- ggmap::get_map(location = dodgr_vertices(gr)%>%select(lon=x,lat=y), source = "google", maptype = "roadmap", zoom = 6)
# ggmap::ggmap(p)+
#   geom_point(aes(x=from_lon, y=from_lat, color=factor(component)), data=gr[gr$component%in%v,], pch=".")+
#   geom_path(data=as_tibble(st_coordinates(tmp2$artificial_edges))%>%rename(lon=X, lat=Y)%>%mutate(feature_id=rep(1:(n()/2), each=2)), aes(group=feature_id))







visualize_network_connections(tmp2$complete_network, tmp2$artificial_edges, id_roads = "id")

stop()





ggplot(data=tmp2$complete_network[1:10,]) + geom_sf()

library(furrr)
plan(multisession, workers = 4)







agencies <- orce::agencias_bdo%>%dplyr::filter(grepl(glue::glue("^{ufnow$uf_codigo}"), agencia_codigo))

library (osmplotr)
map <- osm_basemap (mc, bg = "gray95") %>%
  #add_osm_objects (mc, col = "gray5") %>%
  print_osm_map ()


vt <- dodgr_vertices (grc)
ggplot(data=vt%>%filter(component<=6), aes(x=x, y=y, color=factor(component)), alpha=1/3)
+
  geom_point() +
  geom_point(aes(x=agencia_lon, y=agencia_lat), data=agencies, color="black")
grc1 <- grc[grc$component==1,]
rm(grc)
gc()

gc()



gc()

agencies_ll <- agencies%>%sf::st_coordinates()
rownames(agencies_ll) <- agencies$agencia_codigo

dd <- dodgr_dists(grc1, to = agencies_ll, from = tracts_points%>%sf::st_coordinates(), shortest = FALSE)
dt <- dodgr_times(grc1, to = agencies_ll, from = tracts_points%>%sf::st_coordinates(), shortest = FALSE)
which(is.na(dd), arr.ind = TRUE)

r <- data.frame(
  agencia_codigo = rep(agencies$agencia_codigo, each = nrow(tracts)),
  setor = rep(tracts$setor, times = nrow(agencies)),
  distancia_km = round(as.vector(dd) / 1000, 2),
  duracao_horas = round(as.vector(dt) / 60, 2)
)

library(tictoc)
tic()
cp <- connect_points(streets = mc, points = tracts_points%>%slice_sample(n=30), street_id_col = "id", point_id_col = "setor", waterways = waterways_sf)
toc()

plot_connections(connections = cp%>%filter(#!is.na(waterway_type),
  distance_to_street>500)
                 , streets = mc, street_id_col = "id", tracts_sf = tracts, tract_id="setor")


stop()
mc_con <- bind_rows(mc%>%mutate(id=as.character(id)), cp)
graph <- weight_streetnet (mc_con, wt_profile = "motorcar", id_col = "id")

dd <- dodgr_dists(grc1_con, to = agencies%>%sf::st_coordinates(), from = tracts%>%sf::st_coordinates(), shortest = FALSE)

stop()

  p <- tracts%>%inner_join(r, by="setor")%>%filter(is.na(distancia_km))%>%distinct(setor, geometry)
p_in <- tracts%>%semi_join(r%>%filter(!is.na(distancia_km)), by="setor")%>%distinct(setor, geometry)
if (nrow(p)>0) {
  cp <- connect_points(streets = mc, points = p, id_col = "id")
} else {
  cp <- mc
}
plot_connections(streets = cp, points = p_in%>%head(20), id_col = "id")
