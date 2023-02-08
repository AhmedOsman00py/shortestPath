
#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
Rcpp::List bellmanFordRcpp(Rcpp::NumericMatrix adjacencyMatrix, int source){

  int n = adjacencyMatrix.nrow();

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

        if (adjacencyMatrix(u,v) > 0) {

          double alt = distance[u] + adjacencyMatrix(u,v);

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
