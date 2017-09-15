#include <RcppArmadillo.h>
#include <math.h>

//' stkde
//'
//' Calculates the spatiotemporal 
//'
//' @param coords \code{arma::mat} object with x and y coordinates in the
//' columns.
//'
//' @return the linear directional mean.
//'
//' @noRd
float stkde (int n, float x, float y, float t, float xi, float yi, float ti,
        float hs, float ht)
{
    float bw_factor = 1 / (n * hs * hs * ht);

    float u = (x - xi) / hs;
    float v = (y - yi) / hs;
    float w = (t - ti) / ht;

    float ks = 2 / M_PI * (1 - ((u * u) + (v * v))) * ((u * u) + (v * v));
    if (ks >= 1)
        ks = 0;
    float kt = 0.75 * (1 - (w * w));
    if (kt >= 1)
        kt = 0;
}

//' rcpp_stkde
//'
//' Calculates a spatiotemporal kernel density estimation (STKDE).
//'
//' @param pts_x X coordinates of input data.
//' @param pts_y Y coordinates of input data.
//' @param pts_z Z coordinates of input data.
//' @param hs Spatial bandwidth.
//' @param ht Temporal bandwidth.
//'
//' @noRd
// [[Rcpp::export]]
void rcpp_ldm (Rcpp::DataFrame xyz_in, float hs, float ht, int res_sp,
        int res_t)
{
    std::vector <int> pts_x = Rcpp::as <std::vector <int>> (xyz_in ["x"]);
    std::vector <int> pts_y = Rcpp::as <std::vector <int>> (xyz_in ["y"]);
    std::vector <int> pts_z = Rcpp::as <std::vector <int>> (xyz_in ["z"]);

    auto xmin = min_element (std::begin (pts_x), std::end (pts_x));
    auto ymin = min_element (std::begin (pts_y), std::end (pts_y));
    auto zmin = min_element (std::begin (pts_z), std::end (pts_z));
    auto xmax = max_element (std::begin (pts_x), std::end (pts_x));
    auto ymax = max_element (std::begin (pts_y), std::end (pts_y));
    auto zmax = max_element (std::begin (pts_z), std::end (pts_z));

    arma::cube::fixed <res_sp, res_sp, res_t> stkde_val;
}
