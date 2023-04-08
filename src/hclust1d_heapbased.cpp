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
//          2 - average

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

  //the following variables are required for average linkage only
  //each interval (which is a possible merge opportunity)
  //               constitutes of 2 clusters - the left one and the write one
  //at the beginning they are both just the singletons
  std::vector<double> left_part_leftish_weighted_distance_sums(points_size - 1, 0.0);
  std::vector<double> left_part_rightish_weighted_distance_sums(points_size - 1, 0.0);
  std::vector<double> right_part_leftish_weighted_distance_sums(points_size - 1, 0.0);
  std::vector<double> right_part_rightish_weighted_distance_sums(points_size - 1, 0.0);
  std::vector<int> left_part_cluster_counts(points_size - 1, 1);
  std::vector<int> right_part_cluster_counts(points_size - 1, 1);
  std::vector<int> left_part_leftish_indexes = left_indexes;
  std::vector<int> left_part_rightish_indexes = left_indexes;
  std::vector<int> right_part_leftish_indexes = right_indexes;
  std::vector<int> right_part_rightish_indexes = right_indexes;

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
    //the cluster number id is being merged

    int left_id = interval_left_ids[id];
    int right_id = interval_right_ids[id];

    merge(stage, 0) = left_merges[id];
    merge(stage, 1) = right_merges[id];

    height[stage] = key_id.first;

    int id_cluster_count;
    double id_rightish_weighted_distance_sums;
    double id_leftish_weighted_distance_sums;

    if (method == 2) {  //calculate statistics of the currently merged cluster
      id_cluster_count = left_part_cluster_counts[id] + right_part_cluster_counts[id];
      id_rightish_weighted_distance_sums = left_part_rightish_weighted_distance_sums[id] +
                                           right_part_rightish_weighted_distance_sums[id] +
                                           left_part_cluster_counts[id] *
                                           (points[right_part_rightish_indexes[id]] - points[left_part_rightish_indexes[id]]);
      id_leftish_weighted_distance_sums = left_part_leftish_weighted_distance_sums[id] +
                                           right_part_leftish_weighted_distance_sums[id] +
                                           right_part_cluster_counts[id] *
                                           (points[right_part_leftish_indexes[id]] - points[left_part_leftish_indexes[id]]);
    }

    if (left_id > -1) {

        interval_right_ids[left_id] = right_id;
        right_merges[left_id] = stage + 1;

        switch (method) {
          case 0:   //single_implemented_by_heap linkage
            break;
          case 1:  //complete
            update_key_by_id(priority_queue, left_id, points[right_indexes[id]] - points[left_indexes[left_id]]);
            right_indexes[left_id] = right_indexes[id];
            break;
          case 2:  //average linkage
            //id cluster just got merged
            update_key_by_id(priority_queue, left_id,
                             left_part_rightish_weighted_distance_sums[left_id] / left_part_cluster_counts[left_id] +
                             id_leftish_weighted_distance_sums / id_cluster_count +
                             points[left_indexes[id]] - points[left_part_rightish_indexes[left_id]]);

            right_part_leftish_weighted_distance_sums[left_id] = id_leftish_weighted_distance_sums;
            right_part_cluster_counts[left_id] = id_cluster_count;
            right_part_leftish_indexes[left_id] = left_indexes[id];
            right_indexes[left_id] = right_indexes[id];
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
            update_key_by_id(priority_queue, right_id, points[right_indexes[right_id]] - points[left_indexes[id]]);
            left_indexes[right_id] = left_indexes[id];
            break;
          case 2:  //average linkage
            update_key_by_id(priority_queue, right_id,
                             id_rightish_weighted_distance_sums / id_cluster_count +
                             right_part_leftish_weighted_distance_sums[right_id] / right_part_cluster_counts[right_id] +
                             points[right_part_leftish_indexes[right_id]] - points[right_indexes[id]]);

            left_part_rightish_weighted_distance_sums[right_id] = id_rightish_weighted_distance_sums;
            left_part_cluster_counts[right_id] = id_cluster_count;
            left_part_rightish_indexes[right_id] = right_indexes[id];
            left_indexes[right_id] = left_indexes[id];
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
