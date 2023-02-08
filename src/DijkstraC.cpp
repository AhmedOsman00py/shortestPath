/**
\Dijkstra Algorithm to find shortest path from a node to others
\
\@param matriceAdjacence Data.frame object with three variables (from, to and weight)
\that contains the edges of the graph_DataFrame (from, to) with the weight of the edge (w)
\@param source initial node (could be int or str).
\@return list of two elements.
\Distance: the shortest distance from the start node to other nodes (vector)
\parents : the predecessor of each node (vector)
\@export
\@examples
\matriceAdjacence = t(matrix(data = c(0  , 0  ,  0  ,  0  ,  0  ,    5  , 0,
\                                   3  , 0  ,  0  ,  10 ,  3  ,   11  , 0,
\                                   0  , 1  ,  0  ,  7  ,  0  ,   0   , 0,
\                                   0  , 0  ,  0  ,  0  ,   0 ,    0  , 0,
\                                   0  , 0  ,  0  ,  4  ,  0  ,    0  , 0,
\                                   1  , 0  ,  0  ,  0  ,  0  ,    0  , 0,
\                                   5  , 0  ,  0  ,  0  ,  4  ,    0  , 0), nrow  = 7))
\DijkstraC(matriceAdjacence,2)
*/

#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::List DijkstraC(Rcpp::NumericMatrix adjacency_matrix, int source) {
  int n = adjacency_matrix.nrow();
  Rcpp::NumericVector distances(n);
  Rcpp::IntegerVector predecessors(n);

  for (int i = 0; i < n; i++) {
    distances[i] = INT_MAX;                                                   // distance initiale à l infini pour tous les sommets sauf la source
    predecessors[i] = -1;                                                     // pas de prédécesseur pour tous les sommets sauf la source
  }

  distances[source] = 0;                                                      // distance à 0 pour le sommet source
  std::vector<bool> visited(n, false);                                        // vecteur des sommets visités (initialisés à false)
  while(true){
    int min = INT_MAX;                                                        // indice du sommet avec la plus petite distance parmi ceux non visités
    int min_index = -1;
    for (int i = 0; i < n; i++) {
      if (!visited[i] && (distances[i] <= min)) {
        min = distances[i] ;
        min_index = i;
      }
    }

    if (min_index == -1) {
      break;                                                                  // tous les sommets ont déjà ete visités, on sort de la boucle while.
    }

    visited[min_index] = true;                                                // on marque le sommet comme visité
    for (int i = 0; i < n ; i++) {
      if (!visited[i] && adjacency_matrix(min_index,i) && distances[min_index] != INFINITY && distances[min_index] + adjacency_matrix(min_index,i) < distances[i]) {
        distances[i] = distances[min_index] + adjacency_matrix(min_index,i);
        predecessors[i] = min_index;
      }
    }
  }
  return Rcpp::List::create(_["dist"]=distances, _["parent"]=predecessors +1);
}
