.onLoad <- function(libname, pkgname) {
  clinks <<- memoise::memoise(clinks, cache = cachem::cache_disk(max_size = 100e6))
}
