#ifndef ORDER_H

#define ORDER_H

#include <Rcpp.h>
#include <vector>
using namespace Rcpp;

void order(NumericVector & data, std::vector<int> & index);

void order(std::vector<double> & data, std::vector<int> & index);

#endif
