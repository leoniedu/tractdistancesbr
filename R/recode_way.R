recode_way <- function(highway) {
  r <- case_match(highway,
                  valid_waterways ~ "waterway",
                  "artificial" ~ "artificial",
                  wibge$weighting_profiles$way ~ "road")
  r
}
