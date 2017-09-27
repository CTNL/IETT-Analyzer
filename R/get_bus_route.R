#' Get bus route information
#'
#' @param routeName Route name
#' @param from Website the data fetched ("trafi","IETT")
#'
#' @return List object
#'         route.going Route shape of going
#'         route.return Route shape of return
#'         stops.going Bus stops of going
#'         stops.return Bus stops of return
#' @export
#'
#' @examples \dontrun{
#'   route <- get_bus_route("43R")
#'   head(route$stops.going)
#'   plot_bus_route(route)
#' }
get_bus_route<-function(routeName, from="IETT"){
  data <- list(name=routeName)

  if(from=="IETT"){
    html <- xml2::read_html(paste0("http://www.iett.istanbul/tr/main/hatlar/",routeName))

    jsParse <- function (js){
      val <- strsplit(as.character(js),"),new google.maps.LatLng(",fixed = T)[[1]]
      val[1] <- gsub("[new google.maps.LatLng(","",val[1],fixed = T)
      val[length(val)] <- gsub("),]","",val[length(val)],fixed = T)
      val <- do.call(rbind, mapply(strsplit,val,",",USE.NAMES = F))
      data.frame(Lat=as.numeric(val[,1]),
                 Lng=as.numeric(val[,2]))
    }

    data$route.going <- jsParse(xml2::xml_contents(xml2::xml_find_first(html, '//div[@id="locationGidis"]')))
    data$route.return <- jsParse(xml2::xml_contents(xml2::xml_find_first(html, '//div[@id="locationDonus"]')))


    tidy_stops <- function(stops){
      stops <- lapply(stops,xml2::xml_attrs)
      stops <- lapply(stops,function(s){
        s[match(c("data-station-name","data-station-lat","data-station-lng"), names(s))]
      })
      stops <- do.call(rbind,stops)
      colnames(stops) <- c("Name","Lat","Lng")
      data.frame(Name=stops[,1],
                 Lat=as.numeric(stops[,2]),
                 Lng=as.numeric(stops[,3]), stringsAsFactors = F)
    }

    data$stops.going <- tidy_stops(xml2::xml_find_all(html, '//*[@data-station-direction="Going"]/li'))
    data$stops.return <- tidy_stops(xml2::xml_find_all(html, '//*[@data-station-direction="Return"]/li'))


  } else if (from=="trafi"){
    if (!requireNamespace("jsonlite", quietly = TRUE) || !requireNamespace("gepaf", quietly = TRUE)) {
      stop("jsonlite and gepaf packages are needed for this function to work. Please install it:\n
           install.packages('jsonlite', 'gepaf')",  call. = FALSE)
    }

    Track <- "1"
    url <- paste0("https://web.trafi.com/schedules/trackstops?scope=istanbul&scheduleId=ist_",
               routeName,"&trackId=",routeName,"-",Track)
    tracks <- jsonlite::fromJSON(url(url))
    data$stops.going <- tracks$Stops
    data$route.going <- gepaf::decodePolyline(tracks$Tracks[1,]$Shape)
    colnames(data$route.going) <- c("Lat","Lng")

    if(nrow(tracks$Tracks) > 1){
      Track <- "2"
      url <- paste0("https://web.trafi.com/schedules/trackstops?scope=istanbul&scheduleId=ist_",
                    routeName,"&trackId=",routeName,"-",Track)
      tracks <- jsonlite::fromJSON(url(url))
      data$stops.return <- tracks$Stops
      data$route.return <- gepaf::decodePolyline(tracks$Tracks[2,]$Shape)
      colnames(data$route.return) <- c("Lat","Lng")
    }

  } else {
    stop("This function is only working for Trafi and IETT (Istanbul, Turkey) website.")
  }

  data
}

#' Plot bus route and stops
#'
#' @param route
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{
#'   route <- get_bus_route("43R")
#'   plot_bus_route(route)
#' }
plot_bus_route<-function(route){
  if(is.character(route)){
    route <- get_bus_route_info(route)
  }
  plot(Lng~Lat, route$route.going, type="l")
  points(Lng~Lat, route$stops.going, col="red")
  lines(Lng~Lat, route$route.return, col="blue")
  points(Lng~Lat, route$stops.return, col="green")
}
