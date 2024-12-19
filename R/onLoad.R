# package global variables
package <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname){
  # data release
  package$name <- "tractdistancesbr"
  # local cache dir
  package$cache_dir <- tools::R_user_dir(package$name, which = 'cache')
  dir.create(file.path(package$cache_dir, "dodgr_streetnet_sc"), showWarnings = FALSE, recursive = TRUE)
  dodgr_streetnet_sc_mem <<- memoise::memoise(dodgr::dodgr_streetnet_sc, cache = cachem::cache_disk(dir = file.path(package$cache_dir, "dodgr_streetnet_sc")))
  # Memoise the function with the filesystem cache
  convex_hull_mem <<- memoise::memoise(convex_hull,
                                            cache = cachem::cache_disk(dir = file.path(package$cache_dir, "convex_hull")))
  st_concave_hull_mem <<- memoise::memoise(sf::st_concave_hull,
                                       cache = cachem::cache_disk(dir = file.path(package$cache_dir, "concave_hull")))

}

