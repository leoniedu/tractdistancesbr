ufsiglanow <- "ap"
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
fname_complete_rds <- glue::glue("data-raw/{toupper(ufnow$uf_sigla)}_complete.rds")
fname_pbf_parquet <- glue::glue("data-raw/{toupper(ufnow$uf_sigla)}.parquet")
fname_mc <- glue::glue("data-raw/{tolower(ufnow$uf_sigla)}.rds")
fname_graph <- glue::glue("data-raw/graph_{tolower(ufnow$uf_sigla)}.rds")


# Value:
# Highway preference
# The highway preference specified by the user is a percentage, these are scaled so that the most preferred highway type has a weighted preference of 1.0 (0% always has a weighted preference of 0.0). The calculated score for a segment is divided by this weighted preference.

wmin <- weighting_profiles$weighting_profiles%>%
  group_by(way)%>%
  summarise(max_speed=min(max_speed, na.rm=TRUE), value=.2)
wibge <- list()
wibge$weighting_profiles <-
  wmin%>%
  rows_upsert(weighting_profiles$weighting_profiles%>%
                filter(name=="motorcar", !is.na(max_speed))%>%
                select(way, value, max_speed), "way")%>%
  rows_upsert(
    bind_rows(
      tibble(way=c("artificial", "river", "canal", "ferry"),
             value=c(.05,.075,.06,.1),
             max_speed=c(1,3,2,5))), "way")%>%
  mutate(name="motorcar")%>%
  ungroup()
wibge$surface_speeds <- weighting_profiles$surface_speeds%>%
  group_by(key, value, name="motorcar")%>%
  summarise(max_speed=max(max_speed))%>%
  ungroup()
wibge$penalties <- weighting_profiles$penalties%>%
  filter(name=="motorcar")%>%
  mutate(name="motorcar")

wpj <- jsonlite::toJSON(wibge, pretty = TRUE)
writeLines(wpj, "profile.json")

uf <- readr::read_rds("data-raw/br_states.rds")%>%
  filter(abbrev_state==toupper(ufsiglanow))

