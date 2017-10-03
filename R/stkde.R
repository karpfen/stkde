#' Calculates a spatiotemporal kernel density estimation (STKDE) and returns it
#' as a 3D-array
#'
#' @param xyt_in \code{data.frame} containing the x, y and t coordinates of the
#' input data (projected coordinates).
#' @param hs Spatial bandwidth in m.
#' @param ht Temporal bandwidth.
#' @param x_size Spatial x dimension of output array.
#' @param y_size Spatial y dimension of output array.
#' @param t_size Temporal t dimension of output array.
#'
#' @return \code{arma::cube} object containing all STKDE values.
#'
#' @export
#' @examples
#' \dontrun{
#' x <- runif (100) * 100
#' y <- runif (100) * 100
#' t <- runif (100) * 100
#' xyt <- data.frame (x, y, t)
#' stkde_val <- stkde (xyt_in = xyt, hs = 20, ht = 20, x_size = 15, y_size = 15,
#' t_size = 15)
#' }
stkde <- function (xyt_in, hs, ht, x_size, y_size, t_size)
{
    if (!all (c ("x", "y", "t") %in% names (xyt_in)))
        stop ("xyt_in must contain columns 'x', 'y' and 't'")

    stkde_val <- rcpp_stkde (xyt_in, hs, ht, x_size, y_size, t_size)
    return (stkde_val)
}
