# 9. load contracted
# 10. load tract positions
# 11. calculate distances
source("data-raw/000.R")
grc <- dodgr_load_streetnet(fname_graphc)
mc <- readr::read_rds(fname_mc)


grc1 <- grc[grc$component==1,]
tracts <- orce::pontos_setores%>%dplyr::filter(grepl(glue::glue("^{ufnow$uf_codigo}"), setor))
agencies <- orce::agencias_bdo%>%dplyr::filter(grepl(glue::glue("^{ufnow$uf_codigo}"), agencia_codigo))

agencies_ll <- agencies%>%sf::st_coordinates()
rownames(agencies_ll) <- agencies$agencia_codigo

dd <- dodgr_dists(grc1, to = agencies%>%sf::st_coordinates(), from = tracts%>%sf::st_coordinates(), shortest = FALSE)
dt <- dodgr_times(grc1, to = agencies_ll, from = tracts%>%sf::st_coordinates(), shortest = FALSE)
which(is.na(dd), arr.ind = TRUE)

r <- data.frame(
  agencia_codigo = rep(agencies$agencia_codigo, each = nrow(tracts)),
  setor = rep(tracts$setor, times = nrow(agencies)),
  distancia_km = round(as.vector(dd) / 1000, 2),
  duracao_horas = round(as.vector(dt) / 60, 2)
)


cp <- connect_points(streets = mc, points = head(tracts,201), id_col = "id")

p <- tracts%>%inner_join(r, by="setor")%>%filter(is.na(distancia_km))%>%distinct(setor, geometry)
p_in <- tracts%>%semi_join(r%>%filter(!is.na(distancia_km)), by="setor")%>%distinct(setor, geometry)
if (nrow(p)>0) {
  cp <- connect_points(streets = mc, points = p, id_col = "id")
} else {
  cp <- mc
}
plot_connections(streets = cp, points = p_in%>%head(20), id_col = "id")
