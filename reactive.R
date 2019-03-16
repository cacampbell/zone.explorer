#!/usr/bin/Rscript

vars <- reactiveValues(addresses.tagged = c())

get.dist.A  <- reactive(
                        {
                            dist.A <- input$dist.A
                            validate(need(!is.null(dist.A), "Need distance A"))
                            return(dist.A)
                        }
)

get.dist.B  <- reactive(
                        {
                            dist.B <- input$dist.B
                            validate(need(!is.null(dist.B), "Need distance B"))
                            return(dist.B)
                        }
)

get.dist.C  <- reactive(
                        {
                            dist.C <- input$dist.C
                            validate(need(!is.null(dist.C), "Need distance C"))
                            return(dist.C)
                        }
)

get.dist.D  <- reactive(
                        {
                            dist.D <- input$dist.D
                            validate(need(!is.null(dist.D), "Need distance D"))
                            return(dist.D)
                        }
)

get.time.A <- reactive(
                       {
                           time.A <- input$time.A
                           validate(need(!is.null(time.A), "Need time A"))
                           return(time.A)
                       }
)

get.time.B <- reactive(
                       {
                           time.B <- input$time.B
                           validate(need(!is.null(time.B), "Need time B"))
                           return(time.B)
                       }
)

get.time.C <- reactive(
                       {
                           time.C <- input$time.C
                           validate(need(!is.null(time.C), "Need time C"))
                           return(time.C)
                       }
)

get.time.D <- reactive(
                       {
                           time.D <- input$time.D
                           validate(need(!is.null(time.D), "Need time D"))
                           return(time.D)
                       }
)

get.address.to.add <- reactive(
                               {
                                   address  <- input$address
                                   validate(need(!is.null(address), "Need an Address to Add"))
                                   return(address)
                               }
)

get.addresses.names <- reactive(
                          {
                              return(rownames(vars$addresses.tagged))
                          })

get.addresses <- reactive(
                          {
                              addresses <- vars$addresses.tagged
                              return(addresses)
                          })

get.address.selected <- reactive({
    address <- input$address.selected
    validate(need(!is.null(address), "Need an address"))
    validate(need(!is.na(address), "Need an address"))
    validate(need(address != "", "Need an address"))
    return(address)
})

zips.tagged <- reactive(
                        {
                            require(dplyr)
                            zips <- read.csv("www/data/zips.csv")
                            zips <- zips[c("zip", "longitude", "latitude")]
                            zips <- zips[complete.cases(zips),]
                            colnames(zips) <- c("zip", "lon", "lat")
                            rownames(zips) <- zips$zip
                            zips$zip <- NULL
                            return(zips)
                        }
)


# Observes when user hits "Add" button
# Attempts to geotag entered text using OSM public API, updates reactive values and fields, then sends notifications
observeEvent(
                             input$addAddress,
                             {
                                 address <- get.address.to.add()
                                 addresses <- get.addresses()

                                 tryCatch({
                                     coordinates <- geotag(address)
                                     assertthat::not_empty(coordinates)
                                     showNotification(paste("Successfully geotagged", address))
                                     vars$addresses.tagged <- rbind(addresses, coordinates)
                                 },
                                 error = function(e) {
                                     tryCatch({
                                        coordinates <- geotag.google(address)
                                        assertthat::not_empty(coordinates)
                                        vars$addresses.tagged <- rbind(addresses, coordinates)
                                     }, error = function(e) {
                                        showNotification(paste("Unable to geotag", address), type = "error")
                                     })
                                 },
                                 finally = {
                                     updateTextInput(session, "address", value = "")
                                 })
                             },
                             ignoreNULL = TRUE,
                             ignoreInit = TRUE
    )

dist.mat <- reactive(
                            {
                                address.sel <- get.address.selected()
                                addresses <- get.addresses()
                                address <- addresses[address.sel,]
                                zips <- zips.tagged()
                                require(tidyverse)
                                require(geosphere)
                                distances <- distm(address, zips, fun=distCosine)
                                rownames(distances) <- address.sel
                                colnames(distances) <- rownames(zips)
                                distances <- distances * 0.000621371  # Meters --> Miles
                                showNotification("Calculated distance matrix", type = "message")
                                return(distances)
                                }
)

dist.mat.melt <- reactive(
                                 {
                                     require(reshape2)
                                     distances <- dist.mat()
                                     distances.melt <- melt(distances)
                                     colnames(distances.melt) <- c("Address", "Zip", "Distance")
                                     showNotification("Melted distance matrix", type = "message")
                                     return(distances.melt)
                                 }
)

# Update dist.mat with practical values from OSRM if the distance is less than the max for zone D
dist.mat.melt.prac <- reactive(
                                      {
                                          addresses <- get.addresses()
                                          zips <- zips.tagged()
                                          dists <- dist.mat.melt()
                                          dist.D <- get.dist.D()

                                          updateDistance <- function(i) {
                                              valid <- function(x) {
                                                  if (!any(is.null(x))) {
                                                      return(!any(is.na(x)))
                                                  } else { return(FALSE) }
                                              }

                                              if (dists[i, 3] <= dist.D) {
                                                  address <- as.character(dists[i, 1])
                                                  zipcode <- as.character(dists[i, 2])
                                                  src <- cbind(address, addresses[address,])
                                                  dest <- cbind(zipcode, zips[zipcode, ])

                                                  if (valid(src) && valid(dest)) {
                                                      d <- distPractical(src, dest)

                                                      if (valid(d)) {
                                                          dists[i, 3]  <- d
                                                      }
                                                  }
                                              }
                                          }

                                          for (i in 1:nrow(dists)) { updateDistance(i) }
                                          dists <- as.data.frame(dists)
                                          colnames(dists) <- c("Address", "Zip", "Distance")
                                          showNotification("Updated Distances with OSRM", type = "message")
                                          return(dists)
                                      }
)

dist.mat.zones <- reactive({
  require(dplyr)

  dist.A <- get.dist.A()
  dist.B <- get.dist.B()
  dist.C <- get.dist.C()
  dist.D <- get.dist.D()

  validate(
    need(
      dist.A >= 0 && dist.A <= dist.B && dist.B <= dist.C && dist.C <= dist.D,
      "Please adjust zone distances and try again (a < b < c < d)"
    )
  )

  assign.zone <- Vectorize(function(x) {
    x <- as.numeric(x)
    if (x >= 0 && x < dist.A) {
      return("A")
    } else if (x >= dist.A && x < dist.B) {
      return("B")
    } else if (x >= dist.B && x < dist.C) {
      return("C")
    } else if (x >= dist.C && x <= dist.D) {
      return("D")
    } else {
      return("E")
    }
  })

  distances.melt <- dist.mat.melt.prac()
  distances.melt <- distances.melt %>% mutate(Zone = assign.zone(Distance))
  colnames(distances.melt) <- c("Address", "Zip", "Distance", "Zone")
  showNotification("Assigned Zones", type = "message")
  return(distances.melt)
})

dist.mat.zones.sum <- reactive({
  require(dplyr)
  mat <- dist.mat.zones()
  mat <- mat %>% group_by(Address, Zone) %>% summarize(Zips = paste(as.character(Zip), collapse=", ")) %>% ungroup()
  mat <- mat[mat$Zone != 'E',]
  return(mat)
})

zones.table <- reactive({
  mat <- dist.mat.zones.sum()
  tabl <- NULL

  if (!is.null(mat)) {
      tabl <- datatable(mat,
                    extensions = "Buttons",
                    filter = "top",
                    rownames = FALSE,

                    options = list(
                      pageLength = 5,
                      lengthChange = TRUE,
                      dom = 'lfrtipB',
                      buttons = c("csv", "excel", "pdf")
                    ))
  }

  return(tabl)
})

get.drive.time.zones <- reactive({
  time.A <- get.time.A()
  time.B <- get.time.B()
  time.C <- get.time.C()
  time.D <- get.time.D()

  validate(
    need(
      time.A >= 0 && time.A <= time.B && time.B <= time.C && time.C <= time.D,
      "Need ascending drive times (a <= b <= c <= d)"
    )
  )

  drivetimes <- c(
    paste("0", "to", as.character(time.A), "min"),
    paste(as.character(time.A), "to", as.character(time.B), "min"),
    paste(as.character(time.B), "to", as.character(time.C), "min"),
    paste(as.character(time.C), "to", as.character(time.D), "min")
  )

  drive_times <- factor(drivetimes)
  return(drive_times)
})

get.map.pal <- reactive({
    drive_times <- get.drive.time.zones()
    factPal <- colorFactor(rev(heat.colors(5)), drive_times)
    return(factPal)
})

map <- reactive({
    require(dplyr)
    require(osrm)
    require(htmltools)
    require(rgdal)

    time.A <- get.time.A()
    time.B <- get.time.B()
    time.C <- get.time.C()
    time.D <- get.time.D()

    validate(
      need(
        time.A >= 0 && time.A <= time.B && time.B <= time.C && time.C <= time.D,
        "Need ascending drive times (a <= b <= c <= d)"
      )
    )

    breaks <- c(time.A, time.B, time.C, time.D)
    drive_times <- get.drive.time.zones()
    factPal <- get.map.pal()(drive_times)
    address.sel <- get.address.selected()
    addresses <- get.addresses()
    address <- addresses[address.sel,]
    isochrone <- osrmIsochrone(loc = c(address[1], address[2]), breaks = breaks, res = 100)
    isochrone@data$drive_times <- factor(paste(isochrone@data$min, "to", isochrone@data$max, "min"))

    leafy <- leaflet()
    leafy <- leafy %>% addTiles()
    leafy <- leafy %>% addMarkers(
      lng=address$lon,
      lat=address$lat,
      popup = rownames(address)
    )

    leafy <- leafy %>% addPolygons(
                                   fill = TRUE,
                                   stroke = TRUE,
                                   color = "grey80",
                                   weight = 1,
                                   fillOpacity = 0.50,
                                   fillColor = factPal,
                                   popup = isochrone@data$drive_times,
                                   group = isochrone@data$drive_times,
                                   data = isochrone
                                   )

    leafy <- leafy %>% addLegend("bottomright",
                                 labels = c("Zone A", "Zone B", "Zone C", "Zone D"),
                                 values = drive_times,
                                 title = "Drive Times",
                                 colors = rev(factPal))
    return(leafy)
})

map.zips <- reactive({
  zips <- zips.tagged()
  zones <- dist.mat.zones()
  leafy <- map()

  validate(need(!is.null(zips), "Need Zips"))
  validate(need(!is.null(zones), "Need Zone Assignments"))
  validate(need(!is.null(leafy), "Need Leaflet Map"))

  require(dplyr)
  zips <- zips %>% mutate(Zip = as.character(rownames(zips)))
  zones$Zip <- as.character(zones$Zip)
  marks <- inner_join(zones, zips, by = c('Zip' = 'Zip'))
  marks <- marks[marks$Zone != 'E',]
  leafy <- leafy %>% addMarkers(
      lng = marks$lon,
      lat = marks$lat,
      popup = paste0(marks$Zip, ": ", as.character(round(marks$Distance, digits = 2)), " mi"),
      clusterOptions = markerClusterOptions()
  )

  return(leafy)
})
