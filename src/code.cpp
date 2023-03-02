#include <Rcpp.h>
using namespace Rcpp;


//shttps://stackoverflow.com/questions/17554242/how-to-obtain-the-index-permutation-after-the-sorting


// [[Rcpp::export]]
List hclust1d_single(NumericVector points) {
// only single linkage case,
// which doesn't need a heap because the cluster distances are the same as singleton distances


  order_points <- order(points)

# construction of a distance vector
    if (length(points) >= 2) {

      count <- length(points)

      left_seq <- seq_len(count-2)
      right_seq <- seq(2, len=count-2)

      left_indexes  <- order_points[c(left_seq, count-1)]     # indicates a lower index in a given interval
      right_indexes <- order_points[c(right_seq, count)]   # indicates a higher index in a given interval
      distances <- points[right_indexes] - points[left_indexes]      # lengths of an interval

      left_ids <- c(0, left_seq)    # 0 means "no id to the left"
      right_ids <- c(right_seq, 0)  # 0 means "no id to the right"
      left_merges  <- -left_indexes      # minus as in original hclust implementation
      right_merges <- -right_indexes     # to indicate merge with singletons


    } else {   #at most 1 value in v
      distances <- numeric(0)
    }


    order_distances <- order(distances)
      id_seq <- seq_along(distances)   #we compute the id sequence


      merge <- matrix(nrow=count - 1, ncol=2)
      height <- rep(0, count - 1)

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

      return(ret)
}
