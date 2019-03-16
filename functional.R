#!/usr/bin/env Rscript

geotag.osm <- function(address = NULL) {
  d <- jsonlite::fromJSON(
    gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
         'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
  )
  if (length(d) == 0) { return(c()) }
  coords <- data.frame(lon = as.numeric(d$lon), lat = as.numeric(d$lat))
  rownames(coords) <- address
  return(coords)
}

geotag.google <- function(address = NULL) {
  require(ggmap)
  response <- geocode(address)

  if (response$status == "OK") {
    lat <- response$results[[1]]$geometry$location$lat
    lon <- response$results[[1]]$geometry$location$lon
    coords <- data.frame(
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    )
    rownames(coords) <- address
    return(coords)
  }

  return(c())
}

geotag <- function(address = NULL) {
  coordinates <- c()

  tryCatch({
    coordinates <- geotag.osm(address)
    assertthat::not_empty(coordinates)
  }, error = function(e) {
    tryCatch({
      coordinates <- geotag.google(address)
      assertthat::not_empty(coordinates)
    }, error = function(e) {
    })
  })

  return(coordinates)
}

distPractical <- function(p1, p2) {
  distance <- NA
  tryCatch(
    {
      require(osrm)
      options(osrm.server = "http://172.25.189.168:5002/",  # backend, soft-truck
              osrm.profile = "driving")  # hard coded value in R package, do not change
      route <- osrmRoute(src = p1,
                         dst = p2,
                         overview = FALSE,
                         sp = FALSE)
      distance <- route[2]  # distance in meters
    },
    warning = function(w) {
    },
    error = function(e) {
    },
    finally = {
      return(distance * 0.621371)  # meters --> miles
    }
  )
}
