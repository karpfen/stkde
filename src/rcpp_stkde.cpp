#include <RcppArmadillo.h>
#include <math.h>

//' stkde
//'
//' Calculates a spatiotemporal kernel density estimation (STKDE) for a given
//' point.
//'
//' @param n Total number of points.
//' @param x X coordinate of event.
//' @param y y coordinate of event.
//' @param t T coordinate of event.
//' @param xi X coordinate of grid point.
//' @param yi Y coordinate of grid point.
//' @param ti T coordinate of grid point.
//' @param hs Spatial bandwidth.
//' @param ht Temporal bandwidth.
//'
//' @return stkde value
//'
//' @noRd
float stkde_pt (int n, float x, float y, float t, float xi, float yi, float ti,
        float hs, float ht)
{
    float bw_factor = 1 / (n * hs * hs * ht);

    float u = (x - xi) / hs;
    float v = (y - yi) / hs;
    float w = (t - ti) / ht;

    float usvs = u * u + v * v;
    float ks = 0;
    if (usvs >= 1)
        ks = (2 / M_PI) * (1 - usvs);

    float ws = w * w;
    float kt = 0;
    if (ws >= 1)
        kt = 0.75 * (1 - (ws));

    return (bw_factor * ks * kt);
}

std::vector <int> index_in_range (int n, std::vector <float> x,
        std::vector <float> y, std::vector <float> t, float xval, float yval,
        float tval, float hs, float ht)
{
    std::vector <int> idx;
    float xmin = xval - hs;
    float ymin = yval - hs;
    float tmin = tval - ht;
    float xmax = xval + hs;
    float ymax = yval + hs;
    float tmax = tval + ht;

    for (int i = 0; i < n; i ++)
    {
        float xi = x.at (i);
        float yi = y.at (i);
        float ti = t.at (i);
        if (xi > xmin && xi < xmax)
            if (yi > ymin && yi < ymax)
                if (ti > tmin && ti < tmax)
                    idx.push_back (i);
    }

    return (idx);
}

//' rcpp_stkde
//'
//' Calculates a spatiotemporal kernel density estimation (STKDE) and returns it
//' as a 3D-array
//'
//' @param xyt_in \code{data.frame} containing the x, y and t coordinates of the
//' input data (projected coordinates).
//' @param hs Spatial bandwidth in m.
//' @param ht Temporal bandwidth.
//' @param x_size Spatial x dimension of output array.
//' @param y_size Spatial y dimension of output array.
//' @param t_size Temporal t dimension of output array.
//'
//' @return \code{arma::cube} object containing all STKDE values.
//'
//' @noRd
// [[Rcpp::export]]
arma::cube rcpp_stkde (Rcpp::DataFrame xyt_in, float hs, float ht, int x_size,
        int y_size, int t_size)
{
    std::vector <float> pts_x = Rcpp::as <std::vector <float>> (xyt_in ["x"]);
    std::vector <float> pts_y = Rcpp::as <std::vector <float>> (xyt_in ["y"]);
    std::vector <float> pts_t = Rcpp::as <std::vector <float>> (xyt_in ["t"]);
    int n = pts_x.size ();

    auto xmin = min_element (std::begin (pts_x), std::end (pts_x));
    auto ymin = min_element (std::begin (pts_y), std::end (pts_y));
    auto tmin = min_element (std::begin (pts_t), std::end (pts_t));
    auto xmax = max_element (std::begin (pts_x), std::end (pts_x));
    auto ymax = max_element (std::begin (pts_y), std::end (pts_y));
    auto tmax = max_element (std::begin (pts_t), std::end (pts_t));

    float xrange = abs (xmin - xmax);
    float yrange = abs (ymin - ymax);
    float trange = abs (tmin - tmax);

    float x_resolution = xrange / x_size;
    float y_resolution = yrange / y_size;
    float t_resolution = trange / t_size;

    arma::cube stkde_val (x_size, y_size, t_size);
    stkde_val.zeros ();
    
    for (int i = 0; i < x_size; i ++)
        for (int j = 0; j < y_size; j ++)
            for (int k = 0; k < t_size; k ++)
            {
                float density = 0;
                float xi = i * xrange;
                float yi = j * yrange;
                float ti = k * trange;

                std::vector <int> idx = index_in_range (n, pts_x, pts_y, pts_t,
                        xi, yi, ti, hs, ht);
                int size = idx.size ();
                for (int l = 0; l < idx.size (); l ++)
                {
                    density += stkde_pt (n, pts_x.at (l), pts_y.at (l),
                            pts_t.at (l), xi, yi, ti, hs, ht);
                }
                stkde_val.at (i, j, k) = density;
            }

    return (stkde_val);
}
