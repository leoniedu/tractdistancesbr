library(dodgr)

devtools::load_all()

graph <- weight_streetnet (hampi, wt_profile = "foot")
table (graph$component)

map_connected <- connect_network_components(lines_sf = hampi, dodgr_net = graph, way_id_column = "osm_id")

map_connected_s <- connect_components_simple(lines_sf = hampi, dodgr_net = graph)

library(ggplot2)
ggplot(data=hampi%>%left_join(graph%>%as_tibble()%>%select(osm_id=way_id, component))) +
  geom_sf(aes(color=factor(component))) +
  geom_sf(data=map_connected$artificial_edges, color="purple")+
  geom_sf(data=map_connected_s$artificial_edges, color="orange", linewidth=2)


w <- weighting_profiles$weighting_profiles%>%filter(name=="foot")%>%
  bind_rows(tibble(name="foot", way="artificial", value=.1, max_speed=.5))

graph_connected <- weight_streetnet (map_connected$complete_network, wt_profile = w)

ggplot(data=map_connected$complete_network) +
  geom_sf() +
  geom_sf(data=map_connected$artificial_edges, color="purple", linewidth=3)



stop()
library(osmplotr)
map <- osm_basemap (graph_connected$artificial_edges, bg = "gray95")%>%
  add_osm_objects (hampi1, col = "gray5") %>%
  add_osm_objects (hampi1%>%bind_rows(graph_connected$artificial_edges)%>%filter(highway=="artificial"), col = "red") %>%
  add_axes () %>%
  print_osm_map ()


visualize_network_connections(graph_connected$complete_network, graph_connected$artificial_edges)
