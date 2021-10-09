#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix rcpp_count_sketch(NumericMatrix X, 
                                      IntegerVector hv, 
                                      IntegerVector gv, 
                                      int m) {
    int nrow = X.nrow(), ncol = X.ncol();
    NumericMatrix OUT(m,ncol);
  
    for (int i = 0; i < nrow; i++) {
      NumericVector row = X( i , _ );
      int h = hv(i)-1;
      int g = gv(i);
      OUT( h,  _ ) =  OUT( h,  _ ) + g * row;
    }
  return OUT;
}