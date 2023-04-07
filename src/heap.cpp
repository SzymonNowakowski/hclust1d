#include <vector>  //std::vector
#include <numeric> //std::iota


/*
 *                          a custom heap implementation
 *
 * in addition to std::heap it provides the following functionalities:
 *
 * * reverse_lookup for updating key by id
 * * ability to heapify both down and up the tree
 *
 */

//first some housecleaning functions enabling us access on a vector via tree relations
int left(int i) { return 2*i+1; }
int right(int i) { return 2*i+2; }
int parent(int i) { return (i-1)/2; }

struct heap {
  std::vector<double> keys;
  std::vector<int> ids;
  std::vector<int> reverse_lookup;
  int element_cnt;
};

void remove_all(struct heap & h) { h.element_cnt = 0; }
int size(struct heap & h) { return h.element_cnt; }
bool is_empty(struct heap & h) { return size(h) == 0; }

std::pair<double, int> read_minimum(struct heap & h) {
 if (!is_empty(h))
   return std::pair<double, int>(h.keys[0], h.ids[0]);

 return std::pair<double, int>(0.0, 0);   //for reading minimum of an empty heap
}

void switch_node(struct heap & h, int i, int j) {
          // switches nodes i and j

  if (i==j)
    return;

  int id_i = h.ids[i];
  int id_j = h.ids[j];
  double key_i = h.keys[i];

//  switch keys
  h.keys[i] = h.keys[j];
  h.keys[j] = key_i;

// switch ids of the two nodes
  h.ids[j] = id_i;
  h.ids[i] = id_j;

// finally, update reverse lookups
  h.reverse_lookup[id_i] = j;
  h.reverse_lookup[id_j] = i;
}


void heapify_up(struct heap & h, int i) {
// the assumption is that i is the proper heap
// and that the parent of i is smaller than his sons
// but specifically at i, there may be a problem: i may be smaller than his parent
// this procedure restores the heap property ( key[parent(i)] <= key[i] ) for the node i and its parent

  if (i > 1) {
    int p = parent(i);

    if (h.keys[i] < h.keys[p]) {
      switch_node(h, i, p);
      heapify_up(h, p);
    }
  }
}

int insert(struct heap & h, double key) {
// returning the id of the inserted key
  h.element_cnt++;

  h.keys.push_back(key);
  h.reverse_lookup.push_back(h.element_cnt);

  int id = h.reverse_lookup.size()-1;

  h.ids.push_back(id);
  heapify_up(h, h.element_cnt);

  return id;
}


void heapify_down(struct heap & h, int i) {
// the assumption is that both i's sons are proper heaps
// this procedure restores the heap property ( key[parent(i)] <= key[i] ) for the node i,
//    which may be larger than his sons
//
  int l = left(i);
  int r = right(i);


  int minimal = i;

  if (l <= size(h) and h.keys[l] < h.keys[minimal])
    minimal = l;

  if (r <= size(h) and h.keys[r] < h.keys[minimal])
    minimal = r;

  if (minimal != i) {
    switch_node(h, i, minimal);
    heapify_down(h, minimal);
  }
}

std::pair<double, int> remove_minimum(struct heap & h) {
  std::pair<double, int> r = read_minimum(h);    //it does all the error checking, too
  if (size(h) == 1)
     remove_all(h);
  else {
    switch_node(h, 0, size(h)-1);
    h.element_cnt--;
    heapify_down(h, 0);
  }
  return r;
}

std::vector<int> init_heap(struct heap & h, std::vector<double> keys) {
//returning the ids of inserted keys (to enable update_by_id later on)
//pass by value the keys because they get rearanged

  h.keys = keys;
  h.element_cnt = keys.size();
  h.ids = std::vector<int>(h.element_cnt);
  std::iota(h.ids.begin(), h.ids.end(), 0);
  h.reverse_lookup = std::vector<int>(h.element_cnt);
  std::iota(h.reverse_lookup.begin(), h.reverse_lookup.end(), 0);

  for (int i = parent(h.element_cnt - 1); i>=0; i--)   //parent of the last element is the first one
    heapify_down(h, i);                                 //which may need a rebuild
                                                        //it is safe, as for cnt==0, parent==-1
                                                        //               for cnt==1, parent==-1
                                                        //trigerring an empty loop


  return h.ids;
}

double read_key_by_id(struct heap & h, int id) {
  int index = h.reverse_lookup[id];    // this is the spot when we come to need the reverse_lookup array
  return h.keys[index];
}

void update_key_by_id(struct heap & h, int id, double new_key) {
  int index = h.reverse_lookup[id];    // this is the spot when we come to need the reverse_lookup array
  h.keys[index] = new_key;

// the index's new key either got smaller than his parent's
// or got larger than in one of his sons
// or everything is OK.
//
// Both corrections (in the up direction or in the down direction) will be attempted
// but only *at most* one of them will proceed after the initial checks

  heapify_down(h, index);
  heapify_up(h, index);
}
