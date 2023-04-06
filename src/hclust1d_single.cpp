#include <Rcpp.h>
#include <vector>
#include <assert.h>
using namespace Rcpp;

// [[Rcpp::plugins("cpp11")]]

void order(NumericVector & data, std::vector<int> & index) {
  //https://stackoverflow.com/questions/17554242/how-to-obtain-the-index-permutation-after-the-sorting


  std::iota(index.begin(), index.end(), 0);
  sort(index.begin(), index.end(),
       [&](const int& a, const int& b) {
         return (data[a] < data[b]);
       }
  );
}

void order(std::vector<double> & data, std::vector<int> & index) {
  //https://stackoverflow.com/questions/17554242/how-to-obtain-the-index-permutation-after-the-sorting


  std::iota(index.begin(), index.end(), 0);
  sort(index.begin(), index.end(),
       [&](const int& a, const int& b) {
         return (data[a] < data[b]);
       }
  );
}



// [[Rcpp::export(.hclust1d_single)]]
List hclust1d_single(NumericVector & points) {
// only single linkage case,
// which doesn't need a heap because the cluster distances are the same as singleton distances

  int points_size = points.size();

  std::vector<int> order_points(points_size);
  order(points, order_points);


  //the sequence indexed by the numbers of intervals (there are points_size - 1 intervals)
  //and returning an index of a left point in each interval (as if they were ordered, but they are not)
  auto left_seq = [&](int i) {
    //input: indexes from 0 to points_size - 2, count: points_size - 1
    //output: indexes from 0 to points_size -2
    assert(i >= 0 and i < points_size - 1);
    return i;
  };

  //the sequence indexed by the numbers of intervals (there are points_size - 1 intervals)
  //and returning an index of a right point in each interval (as if they were ordered, but they are not)
  auto right_seq = [&](int i) {
    //input: indexes from 0 to points_size - 2, count: points_size - 1
    //output: indexes from 1 to points_size - 1
    assert(i >= 0 and i < points_size - 1);
    return i + 1;
  };

  //the sequence indexed by the numbers of intervals (there are points_size - 1 intervals)
  //and returning an index of a left point in each interval
  auto left_indexes = [&](int i) {
    //input: indexes from 0 to points_size - 2, count: points_size - 1
    assert(i >= 0 and i < points_size - 1);
    return order_points[left_seq(i)];
  };

  //the sequence indexed by the numbers of intervals (there are points_size - 1 intervals)
  //and returning an index of a right point in each interval
  auto right_indexes = [&](int i) {
    //input: indexes from 0 to points_size - 2, count: points_size - 1
    assert(i >= 0 and i < points_size - 1);
    return order_points[right_seq(i)];
  };

  std::vector<double> distances;
  //the sequence of distances within intervals (there are points_size - 1 intervals)
  for (int i = 0; i < points_size - 1; i++)
    distances.push_back(points[right_indexes(i)] - points[left_indexes(i)]);

  std::vector<int> interval_left_ids(points_size-1);
  std::iota(interval_left_ids.begin(), interval_left_ids.end(), -1);
              // in C++: -1 means "no id to the left"

  std::vector<int> interval_right_ids(points_size-2);
  std::iota(interval_right_ids.begin(), interval_right_ids.end(), 1);
  interval_right_ids.push_back(-1); // in C++: -1 means "no id to the right"

  std::vector<int> left_merges(points_size - 1);
  std::vector<int> right_merges(points_size - 1);
  for (int i=0; i<points_size - 1; i++) {
    left_merges[i] = -left_indexes(i) - 1;
    right_merges[i] = -right_indexes(i) - 1;
  }

  std::vector<int> order_distances(points_size-1);
  order(distances, order_distances);
  IntegerMatrix merge(points_size - 1 , 2 );
  NumericVector height(points_size - 1);

  for (int stage = 0; stage < points_size - 1; stage++) {

    int id = order_distances[stage];
    int left_id = interval_left_ids[id];
    int right_id = interval_right_ids[id];

    merge(stage, 0) = left_merges[id];
    merge(stage, 1) = right_merges[id];

    height[stage] = distances[order_distances[stage]];

    if (left_id > -1) {
        interval_right_ids[left_id] = right_id;
        right_merges[left_id] = stage + 1;
      }

    if (right_id > -1) {
        interval_left_ids[right_id] = left_id;
        left_merges[right_id] = stage + 1;
      }
    }

  CharacterVector labels;
  if (points.attr("names") == R_NilValue) {
    labels = points;
  }
  else {
    labels = points.names();
  }

  for (int i=0; i<points_size; i++)
    order_points[i]++;    //make it R conformant

  List ret = List::create(Named("merge")=merge, Named("height")=height, Named("order")=order_points, Named("labels")=labels, Named("method")="single", Named("dist.method")="euclidean");
  ret.attr("class") = "hclust";

  return ret;
}
