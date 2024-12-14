# 4. read cropped
# 7. contract
# 8. save
source("data-raw/000.R")

mc <- readr::read_rds(fname_mc)
graph <- weight_streetnet (mc, wt_profile = "motorcar", id_col = "id")
dodgr_save_streetnet(graph, fname_graph)
grc <- dodgr_contract_graph (graph)
dodgr_save_streetnet(grc, fname_graphc)
