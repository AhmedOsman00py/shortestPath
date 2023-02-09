// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// bellmanFordRcpp
Rcpp::List bellmanFordRcpp(Rcpp::NumericMatrix matriceAdjacence, int source);
RcppExport SEXP _AlgorithmicDijkstra_bellmanFordRcpp(SEXP matriceAdjacenceSEXP, SEXP sourceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type matriceAdjacence(matriceAdjacenceSEXP);
    Rcpp::traits::input_parameter< int >::type source(sourceSEXP);
    rcpp_result_gen = Rcpp::wrap(bellmanFordRcpp(matriceAdjacence, source));
    return rcpp_result_gen;
END_RCPP
}
// DijkstraC
Rcpp::List DijkstraC(Rcpp::NumericMatrix matriceAdjacence, int source);
RcppExport SEXP _AlgorithmicDijkstra_DijkstraC(SEXP matriceAdjacenceSEXP, SEXP sourceSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericMatrix >::type matriceAdjacence(matriceAdjacenceSEXP);
    Rcpp::traits::input_parameter< int >::type source(sourceSEXP);
    rcpp_result_gen = Rcpp::wrap(DijkstraC(matriceAdjacence, source));
    return rcpp_result_gen;
END_RCPP
}
// dijkstraRcpp
void dijkstraRcpp(NumericMatrix matriceAdjacence, int source, int dest);
RcppExport SEXP _AlgorithmicDijkstra_dijkstraRcpp(SEXP matriceAdjacenceSEXP, SEXP sourceSEXP, SEXP destSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type matriceAdjacence(matriceAdjacenceSEXP);
    Rcpp::traits::input_parameter< int >::type source(sourceSEXP);
    Rcpp::traits::input_parameter< int >::type dest(destSEXP);
    dijkstraRcpp(matriceAdjacence, source, dest);
    return R_NilValue;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_AlgorithmicDijkstra_bellmanFordRcpp", (DL_FUNC) &_AlgorithmicDijkstra_bellmanFordRcpp, 2},
    {"_AlgorithmicDijkstra_DijkstraC", (DL_FUNC) &_AlgorithmicDijkstra_DijkstraC, 2},
    {"_AlgorithmicDijkstra_dijkstraRcpp", (DL_FUNC) &_AlgorithmicDijkstra_dijkstraRcpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_AlgorithmicDijkstra(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
