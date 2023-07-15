#############################################
# 3D city buildings with R
# Milos Popovic 2023/07/14
#############################################

install.packages("remotes")
remotes::install_version(
    "ggplot2",
    version = "3.3.6",
    repos = "http://cran.us.r-project.org"
)

libs <- c(
    "tidyverse", "sf", "rayshader",
    "osmdata", "ggmap"
)

installed_libraries <- libs %in% rownames(
    installed.packages()
)

if(any(installed_libraries == F)){
    install.packages(
        libs[!installed_libraries]
    )
}

invisible(lapply(
    libs, library, character.only = T
))

# 1. GET LINK TO NYC BUILDINGS
#-----------------------------
# read links dataframe
bing_links <- read.csv(
"https://minedbuildings.blob.core.windows.net/global-buildings/dataset-links.csv"
)

head(bing_links)
unique(bing_links$Location)

us_bing_links <- bing_links |>
    dplyr::filter(
        Location == "UnitedStates"
        ) |>
        dplyr::select(
            QuadKey, Url
        )

nrow(us_bing_links)

# 2. COORDINATES TO QUAD KEY
#---------------------------
source("https://raw.githubusercontent.com/milos-agathon/3d-city-buildings/main/R/coordinates_to_quad_key.r")

nycLatitude <- 40.7083
nycLongitude <- -74.0364
zoom <- 9

tileXY <- latLongToTileXY(
    nycLatitude,
    nycLongitude,
    zoom
)

quadKey <- tileXYToQuadKey(
    tileXY$tileX,
    tileXY$tileY,
    zoom
)

nyc_bing_link <- dplyr::filter(
us_bing_links,
QuadKey == as.integer(quadKey)
)

# 3. (DOWN)LOAD NEW YORK BUILDINGS
#---------------------------------

file_name <- "nyc-buildings.csv.gz"

get_nyc_buildings <- function(){
    url <- nyc_bing_link$Url

    download.file(
        url = url,
        destfile = file_name,
        mode = "wb"
    )

}

get_nyc_buildings()

load_nyc_buildings <- function(){
    main_dir <- getwd()
    nyc_buildings_sf <- sf::st_read(
        paste0(
            "/vsigzip/",
            main_dir,
            "/",
            file_name
        ), 
        drivers = "GeoJSONSeq"
    )

    return(nyc_buildings_sf)
}

nyc_buildings_sf <- load_nyc_buildings()
summary(nyc_buildings_sf$height)

# 4. CITY BUFFER
#---------------
# 40.7592317,-73.9766793
get_buffer <- function(){
    nyc <- data.frame(
        long = as.numeric(
            -73.9766793
        ),
        lat = as.numeric(
            40.7592317
            )
    )

    nyc_cents <- nyc |>
        sf::st_as_sf(
            coords = c(
                "long",
                "lat"
            )
        ) |>
        sf::st_set_crs(4326) |>
        sf::st_transform(4326)
    
    sf::sf_use_s2(T)

    nyc_circle <- sf::st_buffer(
        nyc_cents,
        dist = units::set_units(
            2, "km"
        )
    ) |>
    sf::st_transform(
        4326
    )
    
    sf::sf_use_s2(F)

    return(nyc_circle)

}

nyc_circle <- get_buffer()

# 5. CROP DOWNTOWN BUILDINGS
#---------------------------
# get buildings
# Use legacy sf behavior
sf::sf_use_s2(F)

get_nyc_dt_buildings <- function(){
    nyc_dt_buildings <- sf::st_intersection(
        nyc_buildings_sf,
        nyc_circle
    )

    return(nyc_dt_buildings)
}

nyc_dt_buildings <- get_nyc_dt_buildings()

class(nyc_dt_buildings$height)
summary(nyc_dt_buildings$height)
nyc_dt_buildings$height[
    nyc_dt_buildings$height == -1
] <- NA

ggplot()+
geom_sf(
    data = nyc_circle,
    color = "blue",
    fill = "transparent",
    size = 1.5,
    inherit.aes = F
) +
geom_sf(
    data = nyc_dt_buildings,
    color = "pink",
    fill = "pink",
    size = 1.5,
    inherit.aes = F
) +
theme_void()

# 6. ROADS
#---------

nyc_bbox <- sf::st_bbox(
    nyc_dt_buildings
)

get_osm_roads <- function(){
    roads <- nyc_bbox |>
        osmdata::opq() |>
        osmdata::add_osm_feature(
            key = "highway"
        ) |>
        osmdata::osmdata_sf()

    return(roads)
}

roads <- get_osm_roads()
nyc_roads <- roads$osm_lines |>
    sf::st_intersection(
        nyc_circle
    )

ggplot()+
geom_sf(
    data = nyc_circle,
    color = "blue",
    fill = "transparent",
    size = 1.5,
    inherit.aes = F
) +
geom_sf(
    data = nyc_roads,
    color = "#cb2c47",
    fill = "#cb2c47",
    size = 1,
    inherit.aes = F
) +
theme_void()

# 7. STREET LAYER
#----------------

nyc_coords <- c(
    nyc_bbox[["xmin"]],
    nyc_bbox[["ymin"]],
    nyc_bbox[["xmax"]],
    nyc_bbox[["ymax"]]
)

nyc_layer <- ggmap::get_stamenmap(
    nyc_coords,
    zoom = 15,
    maptype = "terrain-background"
)

ggmap::ggmap(nyc_layer)

# 8. PLOT
#--------

cols <- rev(c(
    "#0b1354", "#283680",
    "#6853a9", "#c863b3"
))

texture <- colorRampPalette(
    cols
)(64)


nyc_layer_attributes <- attributes(
    nyc_layer
)

nyc_layer_trans <- matrix(
    adjustcolor(
        nyc_layer,
        alpha.f = 0
),
nrow = nrow(nyc_layer)
)

attributes(nyc_layer_trans) <- nyc_layer_attributes

p1 <- ggmap::ggmap(
    nyc_layer_trans
) +
geom_sf(
    data = nyc_roads,
    fill = "grey80",
    color = "grey80",
    size = .5,
    inherit.aes = F
) +
geom_sf(
    data = na.omit(nyc_dt_buildings),
    aes(
        fill = height
    ),
    color = NA,
    size = 0,
    inherit.aes = F
) +
scale_fill_gradientn(
    name = "",
    colors = texture
) +
theme_void() +
theme(
    legend.position = "none"
)

p2 <- ggmap::ggmap(
    nyc_layer
) +
geom_sf(
    data = nyc_roads,
    fill = "grey80",
    color = "grey80",
    size = .5,
    inherit.aes = F
) +
geom_sf(
    data = na.omit(nyc_dt_buildings),
    aes(
        fill = height
    ),
    color = NA,
    size = 0,
    inherit.aes = F
) +
scale_fill_gradientn(
    name = "",
    colors = texture
) +
theme_void() +
theme(
    legend.position = "none"
)

rayshader::plot_gg(
    ggobj = list(
        p2, p1
    ),
    width = 7,
    height = 7,
    scale = 100,
    solid = F,
    shadow = T,
    shadow_intensity = .99,
    offset_edges = T,
    sunangle = 135,
    phi = 30,
    theta = -30,
    windowsize = c(800, 800),
    multicore = T
)

rayshader::render_camera(
    phi = 45,
    zoom = .55,
    theta = 0
)

rayshader::render_highquality(
    filename = "nyc-downtown.png",
    preview = T,
    light = T,
    lightdirection = 135,
    lightintensity = 700,
    lightaltitude = 45,
    parallel = T,
    width = 2000,
    height = 2000
)
