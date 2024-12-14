ufsiglanow <- "ro"
library(dodgr)
library(ggplot2)
library(sf)
library(dplyr)
devtools::load_all()
#https://download.geofabrik.de/south-america/brazil/norte-latest.osm.pbf
ufnow <- orce::ufs%>%dplyr::filter(grepl(ufsiglanow, uf_sigla, ignore.case=TRUE))
fname_pbf <- glue::glue("data-raw/{tolower(ufnow$regiao_nome)}-latest.osm.pbf")
fname_pbf_rds <- glue::glue("data-raw/{tolower(ufnow$regiao_nome)}.rds")
fname_mc <- glue::glue("data-raw/{tolower(ufnow$uf_sigla)}.rds")
fname_graph <- glue::glue("data-raw/graph_{tolower(ufnow$uf_sigla)}.rds")
fname_graphc <- glue::glue("data-raw/graphc_{tolower(ufnow$uf_sigla)}.rds")
