#ifndef ORDER_H

#define ORDER_H

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

void order(NumericVector & data, std::vector<int> & index);

template <typename T>
void order(std::vector<T> & data, std::vector<int> & index) {
  //https://stackoverflow.com/questions/17554242/how-to-obtain-the-index-permutation-after-the-sorting


  std::iota(index.begin(), index.end(), 0);
  sort(index.begin(), index.end(),
       [&](const int& a, const int& b) {
         return (data[a] < data[b]);
       }
  );
}

#endif
