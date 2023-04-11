#ifndef HEAP_H

#define HEAP_H
#include <vector>  //std::vector
#include <numeric> //std::iota

/*
 *                          a custom heap implementation
 *
 * in addition to std::heap-like functions it provides the following functionalities:
 *
 * * reverse_lookup for updating key by id
 * * ability to heapify both down and up the tree for updating
 *   (both increasing and decreasing) a key in a middle of a tree
 *
 */

struct heap;

struct heap {
  std::vector<double> keys;
  std::vector<int> ids;
  std::vector<int> reverse_lookup;
};

struct heap init_heap(std::vector<double> keys);

int size(struct heap & h);
bool is_empty(struct heap & h);

std::pair<double, int> read_minimum(struct heap & h);
std::pair<double, int> remove_minimum(struct heap & h);

void remove_all(struct heap & h);
int insert(struct heap & h, double key);

double read_key_by_id(struct heap & h, int id);
void update_key_by_id(struct heap & h, int id, double new_key);

#endif
