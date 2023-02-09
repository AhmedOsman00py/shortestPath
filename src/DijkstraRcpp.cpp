#include <RcppArmadillo.h>
#include <Rcpp.h>
using namespace Rcpp;


//' Dijkstra Algorithm to find shortest path from a node to others
//'
//' @param matriceAdjacence Data.frame object with three variables (from, to and weight)
//' that contains the edges of the matriceAdjacence (from, to) with the weight of the edge (w)
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
//' dijkstraRcpp(matriceAdjacence,2, 5)
//'


// [[Rcpp::export]]
void dijkstraRcpp(NumericMatrix matriceAdjacence, int source, int dest) {
  int D = dest;
  dest = dest-1;
  int V = matriceAdjacence.nrow();
  source = source - 1;

  NumericVector dist(V);


  LogicalVector visited(V);


  for (int i = 0; i < V; i++) {

    dist[i] = INT_MAX;
    visited[i] = false;
  }

  dist[source] = 0;

  for (int count = 0; count < V - 1; count++) {

    int min = INT_MAX;
    int min_index = -1;

    for (int v = 0; v < V; v++)     {

      if (visited[v] == false && dist[v] <= min)         {

        min = dist[v];

        min_index = v;
      }
    }

    visited[min_index] = true;

    for (int v = 0; v < V; v++)      {
      if (!visited[v] && matriceAdjacence(min_index, v) && dist[min_index] != INT_MAX && dist[min_index]+matriceAdjacence(min_index , v) < dist[v])          {
        dist[v] = dist[min_index] + matriceAdjacence(min_index,v);
      }
    }
  }




  if(dest!=source and dist[dest]!=INT_MAX){
    std::vector<int> path ;

    path.push_back(dest);
    while(dest!=source){
      for(int i=0 ; i<V ; i++){
        if(matriceAdjacence(i,dest) !=0 && dist[dest] - matriceAdjacence(i,dest) == dist[i]){
          path.push_back(i);
          dest = i ;
          break;
        }
      }
    }

    Rcpp::Rcout<<"Le plus court chemin entre "<<source + 1<<" et "<< D  <<" est : ";
    while (!path.empty()) {
      Rcpp::Rcout<<path.back() + 1 <<"  ";

      path.pop_back();
    }
    Rcpp::Rcout<< " et distant de : "<<dist[D]<<std::endl;

  }
  if(dist[dest] == INT_MAX){
    Rcpp::Rcout<<"pas de chemin possible entre "<< source + 1 <<" et " << dest ;
  }

  return ;



}
