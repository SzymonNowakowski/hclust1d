#include <Rcpp.h>
#include <cmath>        // std::sqrt
using namespace Rcpp;

// [[Rcpp::export(.sqrt)]]
NumericVector sqrt(NumericVector & squared_distances) {
  // input a squared dist dissimilarity structure as reference
  // sqrt-ing them in-place has some undesired side effects, so don't do that

  int size = squared_distances.size();
  NumericVector res(size);
  for (int i = 0; i < size; i++) {
    if (squared_distances[i] < 0)
      stop("A negative value found in a squared distance matrix");
    res[i] = std::sqrt(squared_distances[i]);
  }

  if (squared_distances.attr("Labels") != R_NilValue)
    res.attr("Labels") = squared_distances.attr("Labels");

  return res;

}
