# 1. Read main pbf
# 3. save
source("data-raw/000.R")

m <- read_pbf_dodgr(fname_pbf)
m$lines$id <- 1:nrow(m$lines)
stopifnot(0==sum(duplicated(m$lines$id)))
readr::write_rds(m, fname_pbf_rds)


