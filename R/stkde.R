stkde <- function (x, y, t, xi, yi, ti, n, hs, ht)
{
    bw_factor <- 1 / (n * hs * hs * ht)

    u <- (x - xi) / hs
    v <- (y - yi) / hs
    w <- (t - ti) / ht
}
