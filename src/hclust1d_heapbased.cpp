#include <Rcpp.h>
#include <vector>  //std::vector
#include <numeric> //std::iota
#include <assert.h>
#include "order.h"
#include "heap.h"
using namespace Rcpp;

// [[Rcpp::export(.hclust1d_heapbased)]]
List hclust1d_heapbased(NumericVector & points, int method) {
// general linkage case with a heap
// methods: 0 - single implemented by heap  (undocumented behaviour)
//          1 - complete

// method == 0 is intentionally undocumented
// intended for efficiency tests
// DO NOT USE as it may be dropped in future versions without notice


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
  std::vector<int> left_indexes(points_size - 1);
    //input: indexes from 0 to points_size - 2, count: points_size - 1
  for (int i = 0; i < points_size - 1; i++)
    left_indexes[i] = order_points[left_seq(i)];

  //the sequence indexed by the numbers of intervals (there are points_size - 1 intervals)
  //and returning an index of a right point in each interval
  std::vector<int> right_indexes(points_size - 1);
    //input: indexes from 0 to points_size - 2, count: points_size - 1
  for (int i = 0; i < points_size - 1; i++)
    right_indexes[i] = order_points[right_seq(i)];

  std::vector<double> distances;
  //the sequence of distances within intervals (there are points_size - 1 intervals)
  for (int i = 0; i < points_size - 1; i++)
    distances.push_back(points[right_indexes[i]] - points[left_indexes[i]]);

  std::vector<int> interval_left_ids(points_size-1);
  std::iota(interval_left_ids.begin(), interval_left_ids.end(), -1);
              // in C++: -1 means "no id to the left"

  std::vector<int> interval_right_ids(points_size-2);
  std::iota(interval_right_ids.begin(), interval_right_ids.end(), 1);
  interval_right_ids.push_back(-1); // in C++: -1 means "no id to the right"

  std::vector<int> left_merges(points_size - 1);
  std::vector<int> right_merges(points_size - 1);
  for (int i=0; i<points_size - 1; i++) {
    left_merges[i] = -left_indexes[i] - 1;
    right_merges[i] = -right_indexes[i] - 1;
  }

  struct heap priority_queue = init_heap(distances);

  IntegerMatrix merge(points_size - 1 , 2 );
  NumericVector height(points_size - 1);

  for (int stage = 0; stage < points_size - 1; stage++) {

    std::pair<double, int> key_id = remove_minimum(priority_queue);
    int id = key_id.second;

    int left_id = interval_left_ids[id];
    int right_id = interval_right_ids[id];

    merge(stage, 0) = left_merges[id];
    merge(stage, 1) = right_merges[id];

    height[stage] = key_id.first;

    if (left_id > -1) {

        interval_right_ids[left_id] = right_id;
        right_merges[left_id] = stage + 1;

        switch (method) {
          case 0:   //single_implemented_by_heap linkage
            break;
          case 1:  //complete linkage
            right_indexes[left_id] = right_indexes[id];
            update_key_by_id(priority_queue, left_id, points[right_indexes[left_id]] - points[left_indexes[left_id]]);
            break;

        }
      }

    if (right_id > -1) {

        interval_left_ids[right_id] = left_id;
        left_merges[right_id] = stage + 1;

        switch (method) {
          case 0:   //single_implemented_by_heap linkage
            break;
          case 1:   //complete linkage
            left_indexes[right_id] = left_indexes[id];
            update_key_by_id(priority_queue, right_id, points[right_indexes[right_id]] - points[left_indexes[right_id]]);
            break;

        }
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

  List ret = List::create(Named("merge")=merge, Named("height")=height, Named("order")=order_points, Named("labels")=labels, Named("method")="to_be_overwritten", Named("dist.method")="euclidean");
  ret.attr("class") = "hclust";

  return ret;
}
