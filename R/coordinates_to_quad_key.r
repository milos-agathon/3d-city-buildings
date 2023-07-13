# function to get quad key
tileXYToQuadKey <- function(tileX, tileY, zoom) {
    quadKey <- ""
    for (i in seq(zoom, 1)) {
        digit <- 0
        mask <- bitwShiftL(1, i - 1)
        if (bitwAnd(tileX, mask) != 0) {
            digit <- digit + 1
        }
        if (bitwAnd(tileY, mask) != 0) {
            digit <- digit + 2
        }
        quadKey <- paste0(
            quadKey, as.character(
                digit
            )
        )
    }
    return(quadKey)
}

latLongToTileXY <- function(latitude, longitude, zoom) {
    sinLatitude <- sin(latitude * pi / 180)
    pixelX <- (
        (longitude + 180) / 360) * 256 * (2^zoom)
    pixelY <- (
        0.5 - log(
            (1 + sinLatitude) / (1 - sinLatitude)
        ) / (4 * pi)
    ) * 256 * (2^zoom)
    tileX <- floor(pixelX / 256)
    tileY <- floor(pixelY / 256)
    return(list(tileX = tileX, tileY = tileY))
}
