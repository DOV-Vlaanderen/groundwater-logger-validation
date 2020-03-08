# Positions of barometers

locations <- read.csv('./../../data/meta/inbo/Baro_Position_Lambert72.csv',
                      dec = ',', sep = ';', na.strings = 'NULL', stringsAsFactors = FALSE)

CONST_PROJ_LAMBERT72 <- '+proj=lcc +lat_1=51.16666723333333 +lat_2=49.8333339 +lat_0=90 +lon_0=4.367486666666666 +x_0=150000.013 +y_0=5400088.438 +ellps=intl +towgs84=-106.8686,52.2978,-103.7239,0.3366,-0.457,1.8422,-1.2747 +units=m +no_defs'
CONST_PROJ_WGS84 <- '+proj=longlat +datum=WGS84 +no_defs'

geom = sf::st_as_sf(locations, coords = c("Lambert72_X", "Lambert72_Y"),
                    crs = CONST_PROJ_LAMBERT72, agr = "constant", na.fail = FALSE)
geom <- sf::st_transform(geom, crs = CONST_PROJ_WGS84)
geom

ggplot2::ggplot(data = geom) +
    ggplot2::geom_sf() + ggplot2::theme_light()


map <- leaflet::leaflet()
map <- leaflet::addProviderTiles(map, provider = leaflet::providers$Stamen.Terrain)
map <- leaflet::addMarkers(map, data = geom)
map

map.path <- './drifts/analysis_09/barometer_location.html'
dir.create(dirname(map.path), recursive = TRUE, showWarnings = FALSE)

htmlwidgets::saveWidget(map, file = normalizePath(map.path, mustWork = FALSE),
                        selfcontained = TRUE,
                        libdir = tools::file_path_sans_ext(basename(map.path)),
                        knitrOptions = list())
