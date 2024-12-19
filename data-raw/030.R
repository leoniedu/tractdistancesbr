# 9. load contracted
# 10. load tract positions
# 11. calculate distances
source("data-raw/000.R")
#plan(sequential)
gr <- dodgr_load_streetnet(fname_graph)
mc <- readr::read_rds(fname_highway_rds)

comps <- prop.table(table(gr$component))
ccomps <- cumsum(comps)

library(tictoc)
## keep network covering x% of the roads
v <- c(1:which.max(ccomps>.8))
#v <- 1:3
print(max(v))

tic()
tmp2 <- connect_network_components(lines_sf = mc%>%mutate(id=as.character(id)), dodgr_net = gr[gr$component%in%v,], way_id_column = "id", track_memory = TRUE)
toc()

visualize_network_connections(tmp2$complete_network, tmp2$artificial_edges, id_roads = "id")
bb <- st_bbox(tmp2$complete_network)
p <- ggmap::get_map(location = dodgr_vertices(gr)%>%select(lon=x,lat=y), source = "google", maptype = "roadmap", zoom = 6)
ggmap::ggmap(p)+
  geom_point(aes(x=from_lon, y=from_lat, color=factor(component)), data=gr[gr$component%in%v,], pch=".")+
  geom_path(data=as_tibble(st_coordinates(tmp2$artificial_edges))%>%rename(lon=X, lat=Y)%>%mutate(feature_id=rep(1:(n()/2), each=2)), aes(group=feature_id))

stop()





ggplot(data=tmp2$complete_network[1:10,]) + geom_sf()

library(furrr)
plan(multisession, workers = 4)


waterways_sf <- readr::read_rds(fname_waterway_rds)



tracts_br_2022 <- readRDS("~/github/tractdistancesbr/data-raw/tracts_br_2022.rds")

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

tracts <- tracts_br_2022%>%
  filter(code_state==ufnow$uf_codigo)%>%
  mutate(setor=as.character(code_tract))%>%
  semi_join(orce::pontos_setores%>%sf::st_drop_geometry())
rm(tracts_br_2022)
gc()

tracts_points <- orce::pontos_setores%>%semi_join(tracts%>%sf::st_drop_geometry())%>%
  distinct(setor, .keep_all = TRUE)


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
