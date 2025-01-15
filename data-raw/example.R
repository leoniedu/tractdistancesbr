library(dodgr)



devtools::load_all()

#seed <- .Random.seed

w <- weighting_profiles$weighting_profiles%>%filter(name=="foot")%>%
  bind_rows(tibble(name="foot", way="artificial", value=.1, max_speed=.5))


pts <- st_sample(hampi%>%st_bbox(), size=150)%>%st_cast("POINT")%>%st_as_sf()

graph <- weight_streetnet (hampi, wt_profile = w, id_col = "osm_id")


map_connected_0 <- connect_network_components(lines_sf = hampi, dodgr_net = graph, way_id_column = "osm_id")

##FIX: see function dodgr::add_nodes_to_graph instead of connect_points_to_network
##add_nodes_to_graph(graph, xy, dist_tol = 0.000001, intersections_only = FALSE)
hampic <- connect_points_to_network(lines_sf = map_connected_0$complete_network, points_sf = pts, way_id_column = "id")


##FIX
## when osm_id exists weight_streetnet uses it even with a different id_col specification
weight_streetnet_fix <- function(x, ..., id_col="osm_id") {
  if (id_col!="osm_id") {
    if ("osm_id"%in%colnames(x)) {
      x$osm_id <- NULL
    }
  }
  weight_streetnet(x=x, ..., id_col=id_col)
}

graphc <- weight_streetnet_fix(x=hampic$complete_network, id_col = "id", wt_profile=w)



library(tictoc)

tic()
map_connected_1 <- connect_network_components(lines_sf = hampic$complete_network, dodgr_net = graphc, way_id_column = "id")


map_connected_p <- connect_points_to_network(lines_sf = map_connected_1$complete_network, points_sf = pts, way_id_column = "id")

graphcp <- weight_streetnet_fix(x=map_connected_p$complete_network, id_col = "id", wt_profile=w)


path <- dodgr::dodgr_paths(graphcp, from = st_coordinates(pts[1,]), to=st_coordinates(pts[2,]), vertices=FALSE)

library(ggplot2)
ggplot(data=map_connected_p$complete_network) +
  geom_sf(aes(color=artificial)) + scale_color_manual(values=c("black", "pink"))+
  geom_sf(data=map_connected_p$artificial_edges, color="purple", linewidth=1)+
  geom_sf(data=pts) +
  #geom_sf(data=cp$artificial_edges, color="red", linetype=2)+
  geom_sf(data=pts[1:2, ], color="red", size=3)+
  geom_segment(data=graphcp[unlist(path),], color="red", linewidth=3, alpha=1/3,
              aes(x=from_lon, y=from_lat, xend=to_lon, yend=to_lat))



stop()
library(osmplotr)
map <- osm_basemap (graph_connected$artificial_edges, bg = "gray95")%>%
  add_osm_objects (hampi1, col = "gray5") %>%
  add_osm_objects (hampi1%>%bind_rows(graph_connected$artificial_edges)%>%filter(highway=="artificial"), col = "red") %>%
  add_axes () %>%
  print_osm_map ()


visualize_network_connections(graph_connected$complete_network, graph_connected$artificial_edges)
