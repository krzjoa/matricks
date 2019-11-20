#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector antidiagonal(NumericMatrix x){
  // We suppose ther matrix is 
  int nrow = x.nrow();
  int ncol = x.ncol();
  
  NumericVector num_vec (nrow);
  
  for(int i=0; i<nrow; i++){
    num_vec(i) = x(i, nrow - 1 - i);
  }
  
  return num_vec;
}



