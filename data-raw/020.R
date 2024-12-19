# 4. read cropped
# 7. contract
# 8. save
source("data-raw/000.R")

mc <- readr::read_rds(fname_highway_rds)
graph <- weight_streetnet (mc, wt_profile = "motorcar", id_col = "id")
dodgr_save_streetnet(graph, fname_graph)
## Important: do not contract, since the snap to the network depends on the vertices.
