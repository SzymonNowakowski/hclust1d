#include <Rcpp.h>
#include <vector>
#include <cmath>        // std::abs
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]


// [[Rcpp::export(.dedistance)]]
NumericVector dedistance(NumericVector & distances, int points_size) {
  //input a dist structure and return the points. Points and their direction (=sign) are arbitrarily chosen

  double epsilon = 1e-10;

  std::function<double(int, int)> dissimilarity = [&](int i, int j) {  //I cannot use auto scope because of recurrent call

    //the dissimilarity between (row) i and j is distances[n*(i-1) - i*(i-1)/2 + j-i] in R for i<j<=n
    //see details in https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/dist

    //but if i, j and distances are 0-based
    //and arbitrary, the following must be applied
    if (i==j) return 0.0;
    if (j<i) return dissimilarity(j,i);
    return distances[points_size * i - (i+1)*i/2 + j-i - 1];
  };

  double max_dissimilarity = dissimilarity(0, 1);
  int first = 0;
  int second = 1;
  for (int i = 1; i < points_size-1; i++)  //we walk through the first sub-diagonal and find two points most far apart
    if (dissimilarity(i, i+1) > max_dissimilarity) {
      max_dissimilarity = dissimilarity(i, i+1);
      first = i;
      second = i + 1;
    }

  NumericVector ret(points_size);  //a 0-valued vector of length==point_size
  if (distances.attr("Labels") == R_NilValue) {
    std::vector<unsigned int> indices(points_size);
    std::iota(indices.begin(), indices.end(), 1);
    ret.names() = indices;
  }
  else
    ret.names() = distances.attr("Labels");


  if (max_dissimilarity < epsilon)
    return ret;

  // we assume second > first, the distance == max_dissimilarity > 0

  for (int i=0; i<points_size; i++) {
    double dis1 = dissimilarity(i, first);
    double dis2 = dissimilarity(i, second);
    if (dis1 + dis2 > max_dissimilarity + epsilon) //i is outside of (first, second) interval
      if (dis1 < dis2)  // i < first
        ret[i] = - dis1;
      else     //i > second
        ret[i] = dis1;
    else    //i is inside (first, second) interval
      ret[i] = dis1;
  }

  return ret;
}
