#include <Rcpp.h>
#include <RcppCommon.h>

using namespace Rcpp;

// [[Rcpp::export(rng = false)]]
Rcpp::List rcpp_knn_to_sparse_weights(
    Rcpp::IntegerMatrix & knn_index,
    Rcpp::NumericMatrix & knn_dist,
    bool self_loops
) {
  int n_obs = knn_index.nrow();
  int n_neighbors = knn_index.ncol();
  Rcpp::IntegerVector row_idx(n_obs * n_neighbors);
  Rcpp::IntegerVector col_idx(n_obs * n_neighbors);
  Rcpp::NumericVector vals(n_obs * n_neighbors);

  int nrow = knn_index.nrow();
  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < n_neighbors; ++j) {
      if (knn_index(i, j) == 0) continue;
      col_idx[i * n_neighbors + j] = i;
      row_idx[i * n_neighbors + j] = knn_index(i, j) - 1;
      if ((knn_index(i, j) == i + 1) & self_loops) {
        vals[i * n_neighbors + j] = 0;
      } else {
        vals[i * n_neighbors + j] = knn_dist(i, j);
      }
    }
  }
  Rcpp::List res = Rcpp::List::create(
    Rcpp::_["i"] = row_idx,
    Rcpp::_["j"] = col_idx,
    Rcpp::_["x"] = vals
  );
  return res;
}

// [[Rcpp::export(rng = false)]]
Rcpp::List rcpp_knn_to_sparse_idx(
    Rcpp::IntegerMatrix & knn_index,
    bool self_loops
) {
  int n_obs = knn_index.nrow();
  int n_neighbors = knn_index.ncol();
  Rcpp::IntegerVector row_idx(n_obs * n_neighbors);
  Rcpp::IntegerVector col_idx(n_obs * n_neighbors);
  Rcpp::IntegerVector vals(n_obs * n_neighbors);
  vals.fill(1);

  int nrow = knn_index.nrow();
  for (int i = 0; i < nrow; ++i) {
    for (int j = 0; j < n_neighbors; ++j) {
      if (knn_index(i, j) == 0) continue;
      col_idx[i * n_neighbors + j] = i;
      row_idx[i * n_neighbors + j] = knn_index(i, j) - 1;
      if ((knn_index(i, j) == i + 1) & !self_loops) {
        vals[i * n_neighbors + j] = 0;
      }
    }
  }
  Rcpp::List res = Rcpp::List::create(
    Rcpp::_["i"] = row_idx,
    Rcpp::_["j"] = col_idx,
    Rcpp::_["x"] = vals
  );
  return res;
}
