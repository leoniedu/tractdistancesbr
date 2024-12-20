ufsiglanow <- "am"
library(dodgr)
library(ggplot2)
library(sf)
library(dplyr)
devtools::load_all()
#https://download.geofabrik.de/south-america/brazil/norte-latest.osm.pbf
ufnow <- orce::ufs%>%dplyr::filter(grepl(ufsiglanow, uf_sigla, ignore.case=TRUE))
#fname_pbf <- glue::glue("data-raw/{tolower(ufnow$regiao_nome)}-latest.osm.pbf")
#fname_pbf_rds <- glue::glue("data-raw/{tolower(ufnow$regiao_nome)}.rds")
#fname_pbf <- glue::glue("data-raw/south-america-latest.osm.pbf")
#fname_pbf_rds <- glue::glue("data-raw/south-america.rds")
#fname_pbf_rds <- glue::glue("data-raw/brazil.rds")
fname_highway_rds <- glue::glue("data-raw/{toupper(ufnow$uf_sigla)}_highway.rds")
fname_waterway_rds <- glue::glue("data-raw/{toupper(ufnow$uf_sigla)}_waterway.rds")
fname_pbf_parquet <- glue::glue("data-raw/{toupper(ufnow$uf_sigla)}.parquet")
fname_mc <- glue::glue("data-raw/{tolower(ufnow$uf_sigla)}.rds")
fname_graph <- glue::glue("data-raw/graph_{tolower(ufnow$uf_sigla)}.rds")

m2poly <- function(m, geom_out="POINT") {
  s <- sapply(m$geometry, function(z) class(z)[2])
  table(s)
  mpoly <- m[!s%in%c("LINESTRING", geom_out),]
  mpoly2lines <- sf::st_cast(mpoly, "LINESTRING")
  m <- bind_rows(m[s%in%c("LINESTRING"),], mpoly2lines)%>%
    janitor::remove_empty(which="cols")
  m$id <- 1:nrow(m)
  m
}
