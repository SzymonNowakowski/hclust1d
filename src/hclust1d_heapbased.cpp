#include <Rcpp.h>
#include <vector>  //std::vector
#include <numeric> //std::iota
#include <assert.h>
#include "order.h"
#include "heap.h"
#include <cmath>  //std::sqrt

using namespace Rcpp;

// [[Rcpp::export(.hclust1d_heapbased)]]
List hclust1d_heapbased(NumericVector & points, int method) {
// general linkage case with a heap
// methods: 0 - single implemented by heap  (undocumented behaviour)
//          1 - complete
//          2 - average (UPGMA)
//          3 - centroid (UPGMC)
//          4 - true_median
//          5 - median aka weighted centroids (WPGMC)
//          6 - mcquitty (WPGMA)
//          7 - ward.D
//          8 - ward.D2

// method == 0 is intentionally undocumented
// intended for efficiency tests
// DO NOT USE as it may be dropped in future versions without notice


  int points_size = points.size();

  std::vector<int> order_points(points_size);
  order(points, order_points);
  std::vector<int> reverse_order_points(points_size);  //needed for the median linkage

  if (method == 4) //true median
    order<int>(order_points, reverse_order_points);
 //   ALWAYS x == x[order(x)][order(order(x))] , order(order(x)) is the inverse permutation of order(x)

  auto median = [&](int leftmost_index_in_unordered, int cluster_count) {
    int leftmost_index_in_ordered = reverse_order_points[leftmost_index_in_unordered];
    int midpoint_index_in_ordered = leftmost_index_in_ordered + cluster_count / 2;
    if (cluster_count % 2 == 1)
      return points[order_points[midpoint_index_in_ordered]];
    return (points[order_points[midpoint_index_in_ordered - 1]] + points[order_points[midpoint_index_in_ordered]])/2.0;
  };

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
  std::vector<int> left_part_leftish_indexes(points_size - 1);
    //input: indexes from 0 to points_size - 2, count: points_size - 1
  for (int i = 0; i < points_size - 1; i++)
    left_part_leftish_indexes[i] = order_points[left_seq(i)];

  //the sequence indexed by the numbers of intervals (there are points_size - 1 intervals)
  //and returning an index of a right point in each interval
  std::vector<int> right_part_rightish_indexes(points_size - 1);
    //input: indexes from 0 to points_size - 2, count: points_size - 1
  for (int i = 0; i < points_size - 1; i++)
    right_part_rightish_indexes[i] = order_points[right_seq(i)];

  std::vector<double> distances;
  //the following variables are required for median and centroid linkage
  std::vector<double> left_centroid_aggregates;
  std::vector<double> right_centroid_aggregates;

  //the sequence of distances within intervals (there are points_size - 1 intervals)
  for (int i = 0; i < points_size - 1; i++) {
    double distance = points[right_part_rightish_indexes[i]] - points[left_part_leftish_indexes[i]];

    if (method == 3 or method == 5 or method == 7 or method == 8) {
      left_centroid_aggregates.push_back(points[left_part_leftish_indexes[i]]);
      right_centroid_aggregates.push_back(points[right_part_rightish_indexes[i]]);
    }

    if (method == 3 or method == 5 or method == 7)
      distances.push_back(distance * distance);   //centroid and median (=weighted centroid) returns a squared euclidean distance
    else
        distances.push_back(distance);   //centroid returns a squared euclidean distance
  }

  //the following variables (some of them)
  // are required for some of the linkages
  //each interval (which is a possible merge opportunity)
  //               constitutes of 2 clusters - the left one and the write one
  //at the beginning they are both just the singletons
  std::vector<double> left_part_leftish_weighted_distance_sums(points_size - 1, 0.0);
  std::vector<double> left_part_rightish_weighted_distance_sums(points_size - 1, 0.0);
  std::vector<double> right_part_leftish_weighted_distance_sums(points_size - 1, 0.0);
  std::vector<double> right_part_rightish_weighted_distance_sums(points_size - 1, 0.0);
  std::vector<int> left_part_cluster_counts(points_size - 1, 1);
  std::vector<int> right_part_cluster_counts(points_size - 1, 1);
  std::vector<int> left_part_rightish_indexes = left_part_leftish_indexes;
  std::vector<int> right_part_leftish_indexes = right_part_rightish_indexes;



  std::vector<int> interval_left_ids(points_size-1);
  std::iota(interval_left_ids.begin(), interval_left_ids.end(), -1);
              // in C++: -1 means "no id to the left"

  std::vector<int> interval_right_ids(points_size-2);
  std::iota(interval_right_ids.begin(), interval_right_ids.end(), 1);
  interval_right_ids.push_back(-1); // in C++: -1 means "no id to the right"

  std::vector<int> left_merges(points_size - 1);
  std::vector<int> right_merges(points_size - 1);
  for (int i=0; i<points_size - 1; i++) {
    left_merges[i] = -left_part_leftish_indexes[i] - 1;
    right_merges[i] = -right_part_rightish_indexes[i] - 1;
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
    double id_centroid_aggregate;
    double id_rightish_weighted_distance_sums;
    double id_leftish_weighted_distance_sums;

    if (method == 2) { //"average"  //calculate statistics of the currently merged cluster
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
    if (method == 3 or method == 7 or method == 8) {  //"centroid" or ward.D or ward.D2
      id_cluster_count = left_part_cluster_counts[id] + right_part_cluster_counts[id];
      id_centroid_aggregate = left_centroid_aggregates[id] + right_centroid_aggregates[id];
    }
    if (method == 4) {  //"true_median"
      id_cluster_count = left_part_cluster_counts[id] + right_part_cluster_counts[id];
    }
    if (method == 5) {  //"median" a.k.a weighted centroid
      id_centroid_aggregate = (left_centroid_aggregates[id] + right_centroid_aggregates[id])/2.0;
    }
    if (method == 6) { //"mcquitty" WPGMA
      id_rightish_weighted_distance_sums = 0.5 * left_part_rightish_weighted_distance_sums[id] +
                                           0.5 * right_part_rightish_weighted_distance_sums[id] +
                                           points[right_part_rightish_indexes[id]] - points[left_part_rightish_indexes[id]];
      id_leftish_weighted_distance_sums = 0.5 * left_part_leftish_weighted_distance_sums[id] +
                                          0.5 * right_part_leftish_weighted_distance_sums[id] +
                                          points[right_part_leftish_indexes[id]] - points[left_part_leftish_indexes[id]];
    }

    if (left_id > -1) {

        bool mult = false;
        bool sqrt = false;

        interval_right_ids[left_id] = right_id;
        right_merges[left_id] = stage + 1;

        switch (method) {
        case 0:   //single_implemented_by_heap linkage
          break;
        case 1:  //complete
          update_key_by_id(priority_queue, left_id,
                           points[right_part_rightish_indexes[id]] -
                           points[left_part_leftish_indexes[left_id]]);
          right_part_rightish_indexes[left_id] = right_part_rightish_indexes[id];
          break;
        case 2:  //average linkage
          //id cluster just got merged
          update_key_by_id(priority_queue, left_id,
                           left_part_rightish_weighted_distance_sums[left_id] / left_part_cluster_counts[left_id] +
                           id_leftish_weighted_distance_sums / id_cluster_count +
                           points[left_part_leftish_indexes[id]] - points[left_part_rightish_indexes[left_id]]);

          right_part_leftish_weighted_distance_sums[left_id] = id_leftish_weighted_distance_sums;
          right_part_rightish_weighted_distance_sums[left_id] = id_rightish_weighted_distance_sums;

          right_part_cluster_counts[left_id] = id_cluster_count;

          right_part_leftish_indexes[left_id] = left_part_leftish_indexes[id];
          right_part_rightish_indexes[left_id] = right_part_rightish_indexes[id];
          break;
        case 8:  //ward.D2
          sqrt = true;
        case 7:  //ward.D
          mult = true;
        case 3: { //centroid works on a squared euclidean distance in theory, but for 1d it makes no difference
            double distance = id_centroid_aggregate / id_cluster_count -
                              left_centroid_aggregates[left_id] / left_part_cluster_counts[left_id];

            distance = distance * distance;

            if (mult)
              distance = 2.0 * distance *
                         (left_part_cluster_counts[left_id] * id_cluster_count) /
                         (left_part_cluster_counts[left_id] + id_cluster_count);
            if (sqrt)
              distance = std::sqrt(distance);

            update_key_by_id(priority_queue, left_id, distance); //centroid, ward.D returns a squared euclidean distance
            right_centroid_aggregates[left_id] = id_centroid_aggregate;
            right_part_cluster_counts[left_id] = id_cluster_count;
            break;
          }
        case 4: {  //true median linkage
            //id cluster just got merged
           double distance = median(left_part_leftish_indexes[id],
                                    id_cluster_count) -
                             median(left_part_leftish_indexes[left_id],
                                    left_part_cluster_counts[left_id]);
           update_key_by_id(priority_queue, left_id, distance);
           right_part_cluster_counts[left_id] = id_cluster_count;
           right_part_leftish_indexes[left_id] = left_part_leftish_indexes[id];
           break;
          }
        case 5: {  //median aka weighted centroids
            double distance = id_centroid_aggregate - left_centroid_aggregates[left_id];
            update_key_by_id(priority_queue, left_id, distance * distance); //median returns a squared euclidean distance
            right_centroid_aggregates[left_id] = id_centroid_aggregate;
            break;
          }
        case 6:  //mcquitty linkage
          //id cluster just got merged
          update_key_by_id(priority_queue, left_id,
                           0.5 * left_part_rightish_weighted_distance_sums[left_id] +
                           0.5 * id_leftish_weighted_distance_sums +
                           points[left_part_leftish_indexes[id]] - points[left_part_rightish_indexes[left_id]]);

          right_part_leftish_weighted_distance_sums[left_id] = id_leftish_weighted_distance_sums;
          right_part_rightish_weighted_distance_sums[left_id] = id_rightish_weighted_distance_sums;

          right_part_leftish_indexes[left_id] = left_part_leftish_indexes[id];
          right_part_rightish_indexes[left_id] = right_part_rightish_indexes[id];
          break;

        }  //switch
      }

    if (right_id > -1) {

        bool mult = false;
        bool sqrt = false;

        interval_left_ids[right_id] = left_id;
        left_merges[right_id] = stage + 1;

        switch (method) {
        case 0:   //single_implemented_by_heap linkage
          break;
        case 1:   //complete linkage
          update_key_by_id(priority_queue, right_id,
                           points[right_part_rightish_indexes[right_id]] -
                           points[left_part_leftish_indexes[id]]);
          left_part_leftish_indexes[right_id] = left_part_leftish_indexes[id];
          break;
        case 2:  //average linkage
          update_key_by_id(priority_queue, right_id,
                           id_rightish_weighted_distance_sums / id_cluster_count +
                           right_part_leftish_weighted_distance_sums[right_id] / right_part_cluster_counts[right_id] +
                           points[right_part_leftish_indexes[right_id]] - points[right_part_rightish_indexes[id]]);

          left_part_leftish_weighted_distance_sums[right_id] = id_leftish_weighted_distance_sums;
          left_part_rightish_weighted_distance_sums[right_id] = id_rightish_weighted_distance_sums;

          left_part_cluster_counts[right_id] = id_cluster_count;

          left_part_rightish_indexes[right_id] = right_part_rightish_indexes[id];
          left_part_leftish_indexes[right_id] = left_part_leftish_indexes[id];
          break;
        case 8:  //ward.D2
          sqrt = true;
        case 7:  //ward.D
          mult = true;
        case 3: { //centroid works on a squared euclidean distance in theory, but for 1d it makes no difference
            double distance = right_centroid_aggregates[right_id] / right_part_cluster_counts[right_id] -
                              id_centroid_aggregate / id_cluster_count;

            distance = distance * distance;

            if (mult)
              distance = 2.0 * distance *
                         (right_part_cluster_counts[right_id] * id_cluster_count) /
                         (right_part_cluster_counts[right_id] + id_cluster_count);
            if (sqrt)
              distance = std::sqrt(distance);

            update_key_by_id(priority_queue, right_id, distance); //centroid, ward.D returns a squared euclidean distance
            left_centroid_aggregates[right_id] = id_centroid_aggregate;
            left_part_cluster_counts[right_id] = id_cluster_count;
            break;
          }
        case 4: {  //true median linkage
            //id cluster just got merged
            double distance = median(right_part_leftish_indexes[right_id],
                                     right_part_cluster_counts[right_id]) -
                              median(left_part_leftish_indexes[id],
                                     id_cluster_count);
            update_key_by_id(priority_queue, right_id, distance);
            left_part_cluster_counts[right_id] = id_cluster_count;
            left_part_leftish_indexes[right_id] = left_part_leftish_indexes[id];
            break;
          }
        case 5: {  //median aka weighted centroids
            double distance = right_centroid_aggregates[right_id] - id_centroid_aggregate;
            update_key_by_id(priority_queue, right_id, distance * distance);  //median returns a squared euclidean distance
            left_centroid_aggregates[right_id] = id_centroid_aggregate;
            break;
          }
        case 6:  //mcquitty linkage
          update_key_by_id(priority_queue, right_id,
                           0.5 * id_rightish_weighted_distance_sums +
                           0.5 * right_part_leftish_weighted_distance_sums[right_id] +
                           points[right_part_leftish_indexes[right_id]] - points[right_part_rightish_indexes[id]]);

          left_part_leftish_weighted_distance_sums[right_id] = id_leftish_weighted_distance_sums;
          left_part_rightish_weighted_distance_sums[right_id] = id_rightish_weighted_distance_sums;

          left_part_rightish_indexes[right_id] = right_part_rightish_indexes[id];
          left_part_leftish_indexes[right_id] = left_part_leftish_indexes[id];
          break;

        } //switch
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
