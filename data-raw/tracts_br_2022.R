library(geobr)
tracts <- geobr::read_census_tract(year = 2022, code_tract="all")
readr::write_rds(tracts, "data-raw/tracts_br_2022.rds")


states_br <- geobr::read_state(year = 2020, code_state="all")
readr::write_rds(states_br, "data-raw/br_states.rds")
f <- function(x=bb_r[[1]], expand=2) {
  z <- as.numeric(x)
  ##min
  z[1:2] <- z[1:2]-expand
  ##max
  z[3:4] <- z[3:4]+expand
  names(z) <- attr(x, "names")
  z
}
states_br_out <- states_br%>%bind_cols(lapply(states_br$geom, function(x) f(sf::st_bbox(x)))%>%bind_rows())%>%sf::st_drop_geometry()
readr::write_csv(states_br_out, "data-raw/br_states.csv")


