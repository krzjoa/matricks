#include <Rcpp.h>
using namespace Rcpp;

// Auxilliary funcion to extract antidiagonal from a matrix
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



//' @title Andtidiagonal
//' https://teuder.github.io/rcpp4everyone_en/200_robject.html#member-functions-7
//' Using const?
//' @export
// [[Rcpp::export]]
RObject adiag(const RObject x){
  // We suppose ther matrix is

  if(Rf_isMatrix(x)){
    return antidiagonal((NumericMatrix) x);
  }

  return NumericVector::create(7);
}
