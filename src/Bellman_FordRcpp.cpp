#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;

//' Bellman-Ford's Algorithm to find shortest path from a node to others
//'
//' @param matriceAdjacence Data.frame object with three variables (from, to and weight)
//' that contains the edges of the graph_DataFrame (from, to) with the weight of the edge (w)
//' @param source initial node (could be int or str).
//' @return list of two elements.
//' Distance: the shortest distance from the start node to other nodes (vector)
//' parents : the predecessor of each node (vector)
//' @export
//' @examples
//' matriceAdjacence = t(matrix(data = c(0  , 0  ,  0  ,  0  ,  0  ,    5  , 0,
//'                                    3  , 0  ,  0  ,  10 ,  3  ,   11  , 0,
//'                                    0  , 1  ,  0  ,  7  ,  0  ,   0   , 0,
//'                                    0  , 0  ,  0  ,  0  ,   0 ,    0  , 0,
//'                                    0  , 0  ,  0  ,  4  ,  0  ,    0  , 0,
//'                                    1  , 0  ,  0  ,  0  ,  0  ,    0  , 0,
//'                                    5  , 0  ,  0  ,  0  ,  4  ,    0  , 0), nrow  = 7))
//' bellmanFordRcpp(matriceAdjacence,2)
//'
// [[Rcpp::export]]

Rcpp::List bellmanFordRcpp(Rcpp::NumericMatrix matriceAdjacence, int source){

  int n = matriceAdjacence.nrow();

  Rcpp::NumericVector distance(n);
  Rcpp::IntegerVector predecessor(n);

  for (int i = 0; i < n; i++) {
    distance[i] = R_PosInf;
    predecessor[i] = -1;
  }

  distance[source] = 0;


  for (int i = 1; i <= n-1; i++) {
    for (int u = 0; u < n; u++) {
      for (int v = 0; v < n; v++) {

        if (matriceAdjacence(u,v) > 0) {

          double alt = distance[u] + matriceAdjacence(u,v);

          if (alt < distance[v]) {
            distance[v] = alt;
            predecessor[v] = u;
          }

        }

      }

    }

  }

  return Rcpp::List::create(_["distance"]=distance, _["predecessor"]=predecessor +1 );
}
