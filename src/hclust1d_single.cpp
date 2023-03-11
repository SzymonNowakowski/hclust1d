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

// [[Rcpp::export]]
List hclust1d_single(NumericVector points) {
// only single linkage case,
// which doesn't need a heap because the cluster distances are the same as singleton distances

/*
 * order_points <- order(points)
 */

  int points_size = points.size();
  std::vector<int> order_points(points_size);
  order(points, order_points);

// construction of a distance vector

/*
 * if (length(points) >= 2) {
 */

    if (points_size >= 2) {

  /*
   *  count <- length(points)

   * left_seq <- seq_len(count-2)
   * right_seq <- seq(2, len=count-2)
   */
      //the sequence indexed by the numbers of intervals (there are points_size - 1 intervals)
      //and returning an index of a left point in each interval (as if they were ordered, but they are not)
      auto left_seq = [&](int i) {
        // !!! represents R-ish c(left_seq, count-1)
        //input: indexes from 0 to points_size - 2, count: points_size - 1
        //output: indexes from 0 to points_size -2
        assert(i >= 0 and i < points_size - 1);
        return i;
      };

      //the sequence indexed by the numbers of intervals (there are points_size - 1 intervals)
      //and returning an index of a right point in each interval (as if they were ordered, but they are not)
      auto right_seq = [&](int i) {
        // !!! represents R-ish c(right_seq, count)
        //input: indexes from 0 to points_size - 2, count: points_size - 1
        //output: indexes from 1 to points_size - 1
        assert(i >= 0 and i < points_size - 1);
        return i + 1;
      };

  /* left_indexes  <- order_points[c(left_seq, count-1)]     # indicates a lower index in a given interval
   * right_indexes <- order_points[c(right_seq, count)]   # indicates a higher index in a given interval
   * distances <- points[right_indexes] - points[left_indexes]      # lengths of an interval
   */
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

      //the sequence of distances within intervals (there are points_size - 1 intervals)
      std::vector<double> distances;
      for (int i = 0; i < points_size - 1; i++)
        distances.push_back(points[right_indexes(i)] - points[left_indexes(i)]);


  /*
   * left_ids <- c(0, left_seq)    # 0 means "no id to the left"
   * right_ids <- c(right_seq, 0)  # 0 means "no id to the right"
   * left_merges  <- -left_indexes      # minus as in original hclust implementation
   * right_merges <- -right_indexes     # to indicate merge with singletons
   */
      std::vector<int> interval_left_ids(points_size-1);
      std::iota(interval_left_ids.begin(), interval_left_ids.end(), -1);  // in C++ -1 means "no id to the left"

      std::vector<int> interval_right_ids(points_size-2);
      std::iota(interval_right_ids.begin(), interval_right_ids.end(), 0);
      interval_right_ids.push_back(-1);                                     // in C++ -1 means "no id to the right"


    }  //end of the if statement
/*

    } else {   #at most 1 value in v
      distances <- numeric(0)
    }


    order_distances <- order(distances)
      id_seq <- seq_along(distances)   #we compute the id sequence


      merge <- matrix(nrow=points_size - 1, ncol=2)
      height <- rep(0, points_size - 1)

      for (stage in id_seq) {   #id_seq == seq_along(distances)
#to be precise: from the semantic POV, this line should read
#for (stage in seq_along(distances))
#but it just so happens, that the two are equivalent and
#id_seq has already been computed above
#so from the efficiency POV: the current version
#is slightly more efficient

        id <- id_seq[order_distances[stage]]
        left_id <- left_ids[id]
        right_id <- right_ids[id]

        merge[stage, ] <- c(left_merges[id], right_merges[id])
        height[stage] <- distances[order_distances[stage]]

#cat("height =", minimum$key, ";", points[left], "..", points[right], "\n")

        if (left_id > 0) {

          right_ids[left_id]    <- right_id
          right_merges[left_id] <- stage


        }
        if (right_id > 0) {

          left_ids[right_id]    <- left_id
          left_merges[right_id] <- stage


        }
      }

      if (is.null(names(points))) {
        labels <- points
      } else {
        labels <- names(points)
      }

      ret <- list(merge=merge, height=height, order=order_points, labels=labels, call=match.call(), method=method)
        class(ret) <- "hclust"
*/
  List ret;
  return ret;
}
