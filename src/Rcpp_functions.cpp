#include <Rcpp.h>
// using namespace Rcpp;

// [[Rcpp::export]]
Rcpp::NumericVector drv1(Rcpp::NumericVector &x, double &dt) {
  int n = x.size(); Rcpp::NumericVector tmp(n, NA_REAL);
  double cff = 1 / (4 * dt);
  for (int i = 0; i <= n - 4; ++i) {
    tmp[i] = cff * (-x[i] - x[i + 1] + x[i + 2] + x[i + 3]);
  }
  return(tmp);
}  // End of drv1

// [[Rcpp::export]]
Rcpp::NumericVector drv2(Rcpp::NumericVector &x, double &dt) {
  int n = x.size(); Rcpp::NumericVector tmp(n, NA_REAL);
  double cff = 1 / (4 * dt * dt);
  for (int i = 0; i <= n - 5; ++i) {
    tmp[i] = cff * (-x[i] - 2 * x[i + 2] + x[i + 4]);
  }
  return(tmp);
}  // End of drv2
