#include <Rcpp.h>
#include <cmath>        // std::sqrt
using namespace Rcpp;

// [[Rcpp::export(.sqrt)]]
void sqrt(NumericVector & squared_distances) {
  // input a squared dist dissimilarity structure as reference
  // and calculate its square root in place

  for (int i = 1; i < squared_distances.size(); i++) {
    squared_distances[i] = std::sqrt(squared_distances[i]);
    if (squared_distances[i] < 0)
      stop("A negative value found in a squared distance matrix");
  }
}
