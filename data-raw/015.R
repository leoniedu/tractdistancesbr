# 2. Crop to state
# 5. deal with multilines
source("data-raw/000.R")

m <- readr::read_rds(fname_pbf_rds)

bb <- dodgr:::process_bbox(osmdata::getbb(glue::glue("{ufnow$uf_nome}, Brazil")), pts=NULL, expand=.05)$bbox

mc <- sf::st_crop(m$lines, xmin=bb[1,1], xmax=bb[2,1], ymin=bb[2,1], ymax=bb[2,2])
rm(m)
gc()
s <- sapply(mc$geometry, function(z) class(z)[2])
# First extract all linestrings from any geometry collections
mc <- sf::st_collection_extract(mc, "LINESTRING")
# Then cast to ensure all are single LINESTRING
mc <- sf::st_cast(mc, "LINESTRING")
mc$id <- 1:nrow(mc)

stopifnot(0==sum(duplicated(mc$id)))
readr::write_rds(mc, fname_mc)
