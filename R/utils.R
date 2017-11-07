#' Calculates the coordinate value equivalents of a given STKDE cube
#'
#' @param xyt \code{data.frame} containing the x, y and t coordinates of the
#' input data (projected coordinates).
#' @param cube \code{arma::cube} object containing all STKDE values.
#'
#' @return \code{list} the size of the cube input dimensions filled with the
#' values of the xyt object.
#'
#' @export
get_coordinate_cube_values <- function (xyt, cube)
{
    if (!all (c ("x", "y", "t") %in% names (xyt)))
        stop ("xyt must contain columns 'x', 'y' and 't'")

    x <- xyt$x
    y <- xyt$y
    t <- xyt$t
    dim_xyt <- dim (cube)

    xmin <- min (x)
    ymin <- min (y)
    tmin <- min (t)

    xres <- diff (range (x)) / (dim_xyt [1] - 1)
    yres <- diff (range (y)) / (dim_xyt [2] - 1)
    tres <- diff (range (t)) / (dim_xyt [3] - 1)

    xvals <- cumsum (c (0, rep (xres, dim_xyt [1] - 1))) + xmin
    yvals <- cumsum (c (0, rep (yres, dim_xyt [2] - 1))) + ymin
    tvals <- cumsum (c (0, rep (tres, dim_xyt [3] - 1))) + tmin

    return (list (x = xvals, y = yvals, t = tvals))
}

#' Converts points to a sf object
#'
#' @param dat \code{data.frame} with columns lat and lon.
#' @param epsg The epsg code of the output coordinate reference system.
#'
#' @return \code{sf} object of points.
#'
#' @export
coords_to_sf <- function (dat, epsg = 4326)
{
    pts <- list ("POINT", dim (dat) [1])
    for (i in 1:dim (dat) [1])
    {
        ln <- dat$lon [i]
        lt <- dat$lat [i]
        pts [[i]] <- sf::st_point (c (ln, lt), "XY")
    }
    sfc <- sf::st_sfc (pts, crs = epsg)
    dat$lat <- NULL
    dat$lon <- NULL
    pts_out <- sf::st_sf (sfc, dat)
    return (pts_out)
}
