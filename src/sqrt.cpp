#include <Rcpp.h>
#include <cmath>        // std::sqrt
using namespace Rcpp;

// [[Rcpp::export(.sqrt)]]
NumericVector sqrt(NumericVector & squared_distances) {
  // input a squared dist dissimilarity structure as reference

  for (int i = 1; i < squared_distances.size(); i++) {
    if (squared_distances[i] < 0)
      stop("A negative value found in a squared distance matrix");
    squared_distances[i] = std::sqrt(squared_distances[i]);
  }
  return squared_distances;
}
